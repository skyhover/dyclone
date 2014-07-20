(** NOTE: This module is a different implementation for CIL's data flow analysis
 * framework. I call it "push" style, which is more suitable for our "local"
 * data flow analyses. The CIL's implementation for forward analysis is "push"
 * style already, so here only contains a push-style backward analysis.
 * Push-style: when a stmt is processed, it is responsible to "push" flows TO
 *             its successors (forward analysis) or predecessors (backward
 *             analysis) and combine the data for its successors/predecessors.
 * Pull-style: when a stmt is processed, it combines data flows FROM its
 *             predecessors (forward analysis) or successors (backward
 *             analysis).
 * For our purpose, we want to ignore the effects of the stmts that are NOT in
 * our "local" list but still want the effects of the stmts in our list to reach
 * the stmts that are not; push-style makes it easier to isolate the effects
 * based on the work-list algorithm unless we apply extra filter on the
 * "combine" function in the pull-style (then we have to modify CIL's framework
 * anyway). Also, I'm not sure (1) it is reasonable for CIL's backward-analysis to
 * require users to set initial data for ALL stmts; (2) funcExitData is
 * necessary if we set all initial data already (unless the users themselves are
 * chaning mind about what should be the out-state for sink stmts, or the CIL's
 * authors were trying to be flexible for users). *)

open Cil
open Pretty

module E = Errormsg
module IH = Inthash
module Q = Queue

(** A framework for data flow analysis for CIL code.  Before using 
    this framework, you must initialize the Control-flow Graph for your
    program, e.g using {!Cfg.computeFileCFG} *)

let debug = ref false

type 't action = 
    Default (** The default action *)
  | Done of 't (** Do not do the default action. Use this result *)
  | Post of ('t -> 't) (** The default action, followed by the given 
                        * transformer *)

type 't stmtaction = 
    SDefault   (** The default action *)
  | SDone      (** Do not visit this statement or its successors *)
  | SUse of 't (** Visit the instructions and successors of this statement
                  as usual, but use the specified state instead of the 
                  one that was passed to doStmt *)

(* For if statements *)
type 't guardaction = 
    GDefault      (** The default state *) 
  | GUse of 't    (** Use this data for the branch *)
  | GUnreachable  (** The branch will never be taken. *)


(****************************
 *
 * Common utilities
 *
 * *************************)

(** get the fun name for a Call instr *)
let getFunCallName (i:instr) =
  match i with
  | Call(lvo, Lval(Var(var),_), args, loc) ->
      Some var.vname
  | _ -> None
;;

(** remove all attributes for a type. Shallow version. *)
let typeRemoveAllAttributes t = 
  match t with 
  | TVoid a -> TVoid []
  | TInt (ik, a) -> TInt (ik, [])
  | TFloat (fk, a) -> TFloat (fk, [])
  | TEnum (enum, a) -> TEnum (enum, [])
  | TPtr (t, a) -> TPtr (t, [])
  | TArray (t, l, a) -> TArray (t, l, [])
  | TFun (t, args, isva, a) -> TFun(t, args, isva, [])
  | TComp (comp, a) -> TComp (comp, [])
  | TNamed (t, a) -> TNamed (t, [])
  | TBuiltin_va_list a -> TBuiltin_va_list []
;;
(** remove all attributes for a type. 
 * This is a deep version; but it has problems with mutable vars. *)
let rec typeRemoveAllDeepAttributes t = 
  if !debug then
    ignore(E.log "MDF: deeply remove attrs for type: %a\n" d_type t);
  match t with 
  | TVoid a -> TVoid []
  | TInt (ik, a) -> TInt (ik, [])
  | TFloat (fk, a) -> TFloat (fk, [])
  | TEnum (enum, a) -> TEnum (enum, [])
  | TPtr (t, a) -> TPtr (typeRemoveAllDeepAttributes t, [])
  | TArray (t, l, a) -> TArray (typeRemoveAllDeepAttributes t, l, [])
  | TFun (t, args, isva, a) -> (
      (* Do NOT recursively call for the component types, avoiding inifinite loops *
      let ra (pn, pt, pa) = (pn, typeRemoveAllDeepAttributes pt, []) in
      match args with
      | None -> TFun(typeRemoveAllDeepAttributes t, None, isva, [])
      | Some args' -> TFun(typeRemoveAllDeepAttributes t, Some (List.map ra args'), isva, [])
      *)
      TFun(t, args, isva, [])
    )
  | TComp (comp, a) -> 
      (* Do NOT recursively call, avoiding inifinite loops *
      let ra fi = {fi with ftype = (typeRemoveAllDeepAttributes fi.ftype)} in
      let comp' = {comp with cfields = (List.map ra comp.cfields)} in
      TComp (comp', [])
      *)
      TComp (comp, [])
  | TNamed (ti, a) ->
      (* Should call recursively for the "same" type *)
      ti.ttype <- typeRemoveAllDeepAttributes ti.ttype;
      TNamed (ti, [])
  | TBuiltin_va_list a -> TBuiltin_va_list []
;;
(** deeply remove type and global attributes from a global; 
 * it's in-place modification of the global.
 * Mainly intended for removing "const" for our code chopper *)
let globalRemoveAllDeepAttributes g =
  match g with
  | GType(ti, l) -> ti.ttype <- typeRemoveAllDeepAttributes ti.ttype
  | GCompTag(ci, l) ->
      ci.cattr <- [];
      List.iter (fun f -> f.fattr <- []; f.ftype <- typeRemoveAllDeepAttributes f.ftype) ci.cfields
  | GCompTagDecl(ci, l) -> ci.cattr <- []
  | GEnumTag(ei, l) -> ei.eattr <- [] 
  | GEnumTagDecl(ei, l) -> ei.eattr <- []
  | GVarDecl(vi, l) -> vi.vattr <- []; vi.vtype <- typeRemoveAllDeepAttributes vi.vtype
  | GVar(vi, i, l) -> vi.vattr <- []; vi.vtype <- typeRemoveAllDeepAttributes vi.vtype
  | GFun(fd, l) ->
      fd.svar.vattr <- [];
      fd.svar.vtype <- typeRemoveAllDeepAttributes fd.svar.vtype;
      let rva v = v.vattr <- []; v.vtype <- typeRemoveAllDeepAttributes v.vtype in
      List.iter rva fd.sformals;
      List.iter rva fd.slocals
  | _ -> ()



(** Check whether a stmt is a block *)
(* NOTE: this is useful because we are "shortcutting" Blocks in our code chopper. *)
let isBlock (s:stmt) =
  match s.skind with
      Block _ -> true
    | _ -> false

(** Given a stmt, return it if it's not a block; otherwise,
 * return the first non-block stmt in its body. *)
let rec getFirstNonBlock s = 
  match s.skind with
      Block b ->
        let getCurrentNonBlock v s =
          match v with
              None -> getFirstNonBlock s
            | Some _ -> v
        in
        List.fold_left getCurrentNonBlock None b.bstmts
    | _ -> Some s

let dumpVarinfo oc (v:varinfo) =
    fprintf oc "VID:%d LOC:%a TYPE:%a NAME:%s\n" v.vid d_loc v.vdecl d_type v.vtype v.vname
let docVarinfo () (v:varinfo) =
    (text "VID:") ++ (num v.vid) ++ (text " LOC:") ++ (d_loc () v.vdecl) ++
    (text " TYPE:") ++ (d_type () v.vtype) ++ (text " NAME:") ++ (text v.vname) ++ line

let getNextUnvisitedStmt stmtlist visitedstmts =
  let unvisited = List.filter (fun s -> not (IH.mem visitedstmts s.sid)) stmtlist in
  match unvisited with
      [] -> None
    | h::r -> Some h

(* Decide the starting stmts based on depth-first search of CFG and the given
 * "local" stmt list. "stmtFilter" is used to decide which successors to follow. *)
let getStartStmts stmtFilter localStmtList =
  (* Any stmt that has no pred is a starting stmt. <-- NO! 
   * The criteria is NOT right because Blocks are omitted from our code
   * chopper but they are included in CIL's CFGs. A compromise is to 
   * assume sequential order for the stmts and use the 
   * first unvisited stmt as a starting stmt. *)
  let visitedSid = IH.create 64 in
  let rec localCFGdfs s =
    if IH.mem visitedSid s.sid then
      ()
    else (
      IH.add visitedSid s.sid true;
      let localSuccs = List.filter stmtFilter s.succs in
      List.iter localCFGdfs localSuccs
    )
  in
  let rec loop sl = match getNextUnvisitedStmt localStmtList visitedSid with
                None -> IH.clear visitedSid; sl
            |   Some s -> localCFGdfs s; loop (sl@[s])
  in
  loop []

(* Decide the ending stmts based on breadth-first search of CFG.
 * "stmtFilter" is used to decide which successors to follow. *)
let getEndStmts stmtFilter localStmtList =
  (* Any stmt that has no succs in "localStmtList" is an ending stmt. <-Not enough! 
   * The criteria is NOT sound because Blocks are omitted from our code
   * chopper but they are included in CIL's CFGs. Have to consider Blocks
   * even if they are not in localStmtList. *)
  let visitedSid = IH.create 64 in
  let localCFGbfs s =
    let queuedStmts = Q.create () in
    let endStmts = ref [] in
    if (IH.mem visitedSid s.sid) then (* TODO: refactor further || not (isStmtLocal s) then *)
      []
    else (
      Q.add s queuedStmts;
      while not (Q.is_empty queuedStmts) do
        let hs = Q.take queuedStmts in
        if !debug then
          ignore(E.log "BFS: checking stmt %a\n" d_stmt hs);
        if IH.mem visitedSid hs.sid then ()
        else (
          IH.add visitedSid hs.sid true;
          let dumpQS qs = Q.iter (fun s -> ignore(E.log "BFS: Queued SID:%d\n" s.sid)) qs in
          (* NOTE: "&&" will be shortcutted; so
           * "checkEachSucc" was not performed on every
           * element in a list, causing a semantic bug 
           * in the fold_left...
          let checkEachSucc t s = 
            t && ( if isLocalSucc s then (
              Q.add s queuedStmts; (* only performed for the first un-localSucc due to the shortcut of "&&" *)
              if !debug then dumpQS queuedStmts;
              false
            ) else (
              if !debug then ignore(E.log "BFS: Not queued SID:%d\n" s.sid);
              true ) )
          in
          if List.fold_left checkEachSucc true hs.succs then (
            if !debug then
              ignore(E.log "BFS: found an ending stmt %a\n" d_stmt hs);
            endStmts := !endStmts@[hs]
          )
          *)
          (*
          List.iter (fun s -> if isLocalSucc s then Q.add s queuedStmts) hs.succs;
          if hasNoLocalSucc hs then
            endStmts := !endStmts@[hs]
          *)
          (* just refactor the above "iter" code to make it 
           * similar to getStartStmts and easier to 
           * be used with "stmtFilter": *)
          let localSuccs = List.filter stmtFilter hs.succs in
          ( match localSuccs with
              [] -> endStmts := !endStmts@[hs]
              (* The ordering of the queued stmts may be improved *)
            | _ -> List.iter (fun s -> Q.add s queuedStmts) localSuccs );
          if !debug then dumpQS queuedStmts
        )
      done;
      !endStmts
    )
  in
  let rec loop sl = match getNextUnvisitedStmt localStmtList visitedSid with
                None -> IH.clear visitedSid; sl
            |   Some s -> loop (sl@(localCFGbfs s))
  in
  loop []

(* find the first label for a stmt; it's called "pickLabel" in cil.ml *)
let getFirstLabel s =
  try
    Some ( List.find (
            fun l -> match l with
                       Label _ -> true
                     | _ -> false )
           s.labels )
  with Not_found -> None

let computeLabelUseDefStmt s lused ldef =
  (* add new lables *)
  let newlabel l =
      match l with
          Label (s, loc, t) -> Hashtbl.add ldef s loc
        | _ -> ()
  in
  List.iter newlabel s.labels;

  (* look for used labels; only in Goto. *)
  match s.skind with
      Goto (sref, loc) -> ( (* use the first Label in sref  *)
        let label = getFirstLabel !sref in
        match label with
            Some (Label(str, loc, b)) -> Hashtbl.add lused str loc
          | _ -> E.s (E.log "CodeWrapper: no label for Goto: %a\n" d_stmt s)
      )
    | _ -> ()
;;

(** collect all var names used in the slist -- shallow version *)
let computeVarUseDefStmtList ?(acc_used=Usedef.VS.empty) ?(acc_defs=Usedef.VS.empty) slist : Usedef.VS.t * Usedef.VS.t =
  List.fold_left (fun (u,d) s -> Usedef.computeUseDefStmtKind ~acc_used:u ~acc_defs:d s.skind) (acc_used, acc_defs) slist

(** collect all var names used in the slist -- deep version *)
(* NOTE: CIL's original Usedef.computeDeepUseDefStmtKind has a bug, causing loss
 * of certain vars. Fixed it *)
let computeDeepVarUseDefStmtList ?(acc_used=Usedef.VS.empty) ?(acc_defs=Usedef.VS.empty) slist : Usedef.VS.t * Usedef.VS.t =
  List.fold_left (fun (u,d) s -> Usedef.computeDeepUseDefStmtKind ~acc_used:u ~acc_defs:d s.skind) (acc_used, acc_defs) slist


module LocalStmts = struct
    let localStmtList: stmt list ref = ref []
    let debug = debug

    (* mapping from stmt ids included in localStmtList to statements (or references?)
     * It contains the ids of all "primary" stmts, in addition to the top-level
     * stmts in localStmtList *)
    (* Use the following hashtables to quickly decide whether a stmt is in 
     * localStmtList (deep or shallow). The deep processing makes sense because
     * our code chopper preserves boundaries of If/Switch/Loop (but not Block). *)
    let localSIDHT = IH.create 64
    let isStmtLocal (s:stmt) =
        IH.mem localSIDHT s.sid
    let rec hashSID s = 
        let rec hashSIDopt os = match os with
                None -> ()
            |   Some s' -> hashSID s'
        in
        assert (s.sid>=0);
        if not (isStmtLocal s) then
          IH.add localSIDHT s.sid s;
        match s.skind with
          If (e, b1, b2, l) -> 
            hashSIDlist b1.bstmts;
            hashSIDlist b2.bstmts
        | Switch (e, b, sl, l) -> (* Probably no use in CIL's CFG *)
            hashSIDlist b.bstmts;
            hashSIDlist sl
        | Loop (b, l, s1, s2) ->
            hashSIDlist b.bstmts;
            hashSIDopt s1;
            hashSIDopt s2
        | Block b ->
            hashSIDlist b.bstmts
        | Instr il ->
            (* CIL's instr doesn't have an sid. If we assume CIL's CFG generation
             * separates each instr into a stmt, it would be fine.
             * Update: it's not always true, so we may have related bugs. TODO *)
            ()
        | Return _ | Goto _ | Break _ | Continue _ -> ()
        | _ -> ()
    and hashSIDlist sl = 
        List.iter hashSID sl

    let localTopSIDHT = IH.create 16
    let isStmtLocalTop (s:stmt) =
        IH.mem localTopSIDHT s.sid
    let rec hashTopSID s =
        assert (s.sid>=0);
        IH.replace localTopSIDHT s.sid s
    and hashTopSIDlist sl =
        List.iter hashTopSID sl

    (* Init the data inside this module with a given stmt list *)
    let init sl =
      if !debug then
        ignore(E.log "LocalStmts: initialize hashtables for local stmts...\n");
      try
        IH.clear localSIDHT;
        IH.clear localTopSIDHT;
        localStmtList:=sl;
        hashSIDlist sl;
        hashTopSIDlist sl
      with e -> (ignore(E.log "LocalStmts: weird exception\n"); raise e)

    let rec isLocalSucc s = match s.skind with
            Block b -> (isStmtLocal s) || (try
                    isLocalSucc (List.hd b.bstmts)
                with Failure "hd" -> false)
        |   _ -> isStmtLocal s
    let hasNoLocalSucc s = 
        List.fold_left (fun t i -> t && not (isLocalSucc i)) true s.succs
    let rec isLocalPred s = match s.skind with
            Block b -> (isStmtLocal s) || (
              List.fold_left (fun t i -> t || isLocalPred i) false s.preds )
        |   _ -> isStmtLocal s
    let hasNoLocalPred s =
        List.fold_left (fun t i -> t && not (isLocalPred i)) true s.preds

end


(******************************************************************
 **********
 **********         BACKWARDS 
 **********
 ********************************************************************)
module type BackwardsPushTransfer = sig
  val name: string (* For debugging purposes, the name of the analysis *)

  val debug: bool ref (** Whether to turn on debugging *)

  type t  (** The type of the data we compute for each block start. In many 
           * presentations of backwards data flow analysis we maintain the 
           * data at the block end. This is not easy to do with JVML because 
           * a block has many exceptional ends. So we maintain the data for 
           * the statement start. *)

  val pretty: unit -> t -> Pretty.doc (** Pretty-print the state *)

  val stmtStartData: t Inthash.t
  (** For each block id, the data at the exit of the block. This data structure 
   * should contain initial data for each block used for "compute" unless the
   * block is a sink (then "funcExitData" is used for it). *)

  val funcExitData: Cil.stmt -> t
  (** The data at function exit.  Used for statements with no successors. *)

  val setInitData: Cil.stmt -> t
  (** compute the init value to for each stmt. Usually bottom. *)

  val combineSuccessors: Cil.stmt -> old:t -> t -> t option
  (** When the analysis reaches the start of a block, combine the old data 
   * with the one we have just computed. Return None if the combination is 
   * the same as the old data, otherwise return the combination. In the 
   * latter case, the predecessors of the statement are put on the working 
   * list. *)

  val combineStartData: t -> t -> t
  (** Combine two data together *)

  val doStmt: Cil.stmt -> t -> t action
  (** The (backwards) transfer function for a statement. The {!Cil.currentLoc} is 
   * set before calling this. If it returns None, then we have some default 
   * handling. Otherwise, the returned data is the data before the branch 
   * (not considering the exception handlers) *)

  val doInstr: Cil.instr -> t -> t action
  (** The (backwards) transfer function for an instruction. The 
   * {!Cil.currentLoc} is set before calling this. If it returns None, then we 
   * have some default handling. Otherwise, the returned data is the data 
   * before the branch (not considering the exception handlers) *)

  val filterStmt: Cil.stmt -> Cil.stmt -> bool
  (** Whether to put a predecessor block into the worklist. The parameters are the 
   * predecessor and the block that pushs the flow to the predecessor. *)
  
end

module BackwardsPushDataFlow = 
  functor (T : BackwardsPushTransfer) -> 
  struct
    let worklist: Cil.stmt Q.t = Q.create ()

    (** get data for a stmt; if not exist, give an initial value *)
    let getStmtStartData (s: stmt) : T.t = 
      try IH.find T.stmtStartData s.sid
      with Not_found -> begin
          if LocalStmts.hasNoLocalSucc s then (
              let d = T.funcExitData s in
              if !T.debug then 
                ignore (E.log "BF(%s): funcExitData provides data for block %d: %a\n" T.name s.sid T.pretty d);
              d
          ) else (
              let d = T.setInitData s in
              if !T.debug then 
                ignore (E.log "BF(%s): setInitData provides data for block %d: %a\n" T.name s.sid T.pretty d);
              d
          )
      end

    (** The function pushes flow back to a predecessor and combine the data for
     * the predecessor. *)
    let reachedStatement (pred: stmt) (s:stmt) (d: T.t) : unit = 
      (** see if we know about it already *)
      E.pushContext (fun _ -> dprintf "Backwardly reached statement %d with %a" pred.sid T.pretty d);
      let newdata: T.t option = 
        if IH.mem T.stmtStartData pred.sid then (
          let old = getStmtStartData pred in
          match T.combineSuccessors pred ~old:old d with 
          | None -> (* We are done here: return None *)
              if !T.debug then 
                ignore (E.log "BF(%s): backwardly reached stmt %d with %a\n  implies the old state %a\n"
                          T.name pred.sid T.pretty d T.pretty old);
              None
          | Some d' -> begin
              (* We have changed the data: return Some d' *) 
              if !T.debug then 
                ignore (E.log "BF(%s): weaken data for block %d: %a\n" 
                          T.name pred.sid T.pretty d');
              Some d'
            end
        ) else (
          (* it's the first time to handle this stmt: return Some union *)
          let dinit = getStmtStartData pred in
          match T.combineSuccessors pred ~old:dinit d with 
          | None -> (* d is empty: return Some dinit *)
              if !T.debug then 
                ignore (E.log "BF(%s): set data for block %d for the first time to its init value: %a\n" 
                          T.name pred.sid T.pretty dinit);
              Some dinit
          | Some d' -> begin (* We have updated data: return Some d' *) 
              if !T.debug then 
                ignore (E.log "BF(%s): push data to block %d for the first time: %a\n" 
                          T.name pred.sid T.pretty d');
              Some d'
            end
        )
      in
      E.popContext ();
      match newdata with 
        None -> ()
      | Some d' -> 
          IH.replace T.stmtStartData pred.sid d';
          if T.filterStmt pred s && 
            not (Q.fold (fun exists s' -> exists || s'.sid = pred.sid) false worklist) then (
              if !T.debug then 
                ignore (E.log "BF(%s): add into worklist: sid %d\n" T.name pred.sid);
              Q.add pred worklist
          ) else (
            if !T.debug then 
              ignore (E.log "BF(%s): filtered sid %d\n" T.name pred.sid)
          )
    ;;

    (** Process a statement and return true if the set of live return 
     * addresses on its entry has changed. *)
    (* TODO: it may be a bug or feature: update "s"'s stmtStartData with
     * the result of processStmt too, in addition to pushing data backwards *)
    let processStmt (s: stmt) : bool = 
      currentLoc := get_stmtLoc s.skind;
      if !T.debug then 
        ignore (E.log "BF(%s).stmt %d\n" T.name s.sid);

      (* Find the previous state for the stmt *)
      let init: T.t = getStmtStartData s in
      (* do the stmt *)
      let d: T.t = match T.doStmt s init with 
           Done d' -> d'
         | Default -> init (* do nothing *)
         | Post f -> f init (* transform the flow *)
      in
      (* then do the instr *)
      let curr: T.t = match s.skind with 
            Instr il -> begin
             (* Scan the instructions in reverse order. 
              * This may Stack_overflow on very long blocks ! *)
              let handleInstruction (i: instr) (s: T.t) : T.t = 
                currentLoc := get_instrLoc i;
                match T.doInstr i s with 
                    | Done s' -> s'
                    | Default -> s (* do nothing *)
                    | Post f -> f s
              in
              List.fold_right handleInstruction il d
            end
          | _ -> d
      in
      (* so we get the out-state "curr" for the stmt,
       * now we push the flow back to its predecessors *)
      currentLoc := get_stmtLoc s.skind;
      if !T.debug then
        ignore(E.log "BF(%s): pushing stmt %d: state: %a\n" T.name s.sid T.pretty curr);
      (* TODO: may consider branching stmts as the forward analysis does *)
      (* TODO: may use better ordering for the predecessors. *)
      List.iter (fun s' -> reachedStatement s' s curr) s.preds;
      true


    (** Compute the data flow. Must have the CFG initialized *)
    let compute (sinks: stmt list) = 
      Q.clear worklist;
      List.iter (fun s -> Q.add s worklist) sinks;
      if !T.debug && not (Q.is_empty worklist) then
        ignore (E.log "\nBF(%s): processing ......\n" T.name); 
      let rec fixedpoint () = 
        if !T.debug &&  not (Q.is_empty worklist) then 
          ignore (E.log "BF(%s): worklist= %a\n" T.name
                    (docList (fun s -> num s.sid)) 
                    (List.rev
                       (Q.fold (fun acc s -> s :: acc) [] worklist)));
        let keepgoing = 
          try 
            let s = Q.take worklist in 
            processStmt s
          with Q.Empty -> 
            if !T.debug then 
              ignore (E.log "BF(%s): done\n\n" T.name);
            false
        in
        if keepgoing then
          fixedpoint ();
      in
      fixedpoint ();
          
  end



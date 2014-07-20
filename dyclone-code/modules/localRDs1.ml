(** This is similar to the reachingdefs.ml in CIL, except that it is tailored
 * for dyclone's purpose and computes "local" reaching definitions for an
 * arbitrary code trunk. CFG must have been established before calling this
 * functionality. *)

(* NOTE: CIL's CFG generation seemly transforms each instr in an Instr construct
 * into a stmt, thus giving each instr a sid and making book keeping easier. We
 * so assume this proper: each instr/stmt has a unique id. *)

open Cil
open Pretty

module E = Errormsg
module DF = Dataflow
(* TODO: It seems CIL's Usedef module isn't all right;
 * we may need to have our own way of deciding defs/uses *)
module UD = Usedef (* help decide which vars are defined/used *)
module IH = Inthash

(* NOTE: what is a var def/use (based on CIL's datatypes:
    * We'd better to consider statically decided vars;
    * Writes into array elements, struct/union fields are treated as writes into
    * the array vars, struct/union vars. *)

module IOS = 
  Set.Make(struct
    type t = int option
    let compare io1 io2 =
      match io1, io2 with
	Some i1, Some i2 -> Pervasives.compare i1 i2
      | Some i1, None -> 1
      | None, Some i2 -> -1
      | None, None -> 0
  end)

(* return the intersection of Inthashes ih1 and ih2 *)
let ih_inter ih1 ih2 =
  let ih' = IH.copy ih1 in
  IH.iter (fun i v -> if not(IH.mem ih2 i) then
      IH.remove ih' i else ()) ih1;
  ih'

let ih_union ih1 ih2 =
  let ih' = IH.copy ih1 in
  IH.iter (fun i v -> if not(IH.mem ih' i) then
      IH.add ih' i v else ()) ih2;
  ih'

(* For path-sensitive may-analysis, the "combine" will contain the union of 
 * the entries from iosh1 and iosh2. In addition, if iosh1 (vid->Set(int option))
 * has an entry that iosh2 does not, the Set in the combined result will contain
 * "None" in addition to the elements in iosh1's original Set. *)
(* XXX this function is a performance bottleneck *)
let iosh_combine iosh1 iosh2 =
  let iosh' = IH.copy iosh1 in
  IH.iter (fun id ios1 ->
      try 
          let ios2 = IH.find iosh2 id in
          let newset = IOS.union ios1 ios2 in
          IH.replace iosh' id newset
      with Not_found ->
          let newset = IOS.add None ios1 in
          IH.replace iosh' id newset) iosh1;
  IH.iter (fun id ios2 ->
    if not(IH.mem iosh1 id) then
        let newset = IOS.add None ios2 in
        IH.add iosh' id newset) iosh2;
  iosh'


(* determine if two IOS.t IH.t s are the same *)
let iosh_equals iosh1 iosh2 =
(*  if IH.length iosh1 = 0 && not(IH.length iosh2 = 0) ||
  IH.length iosh2 = 0 && not(IH.length iosh1 = 0)*)
  if not(IH.length iosh1 = IH.length iosh2)
  then 
    false
  else
    IH.fold (fun vid ios b ->
      if not b then b else
      try let ios2 = IH.find iosh2 vid in
      if not(IOS.compare ios ios2 = 0) then
	 false
      else true
      with Not_found -> false)
    iosh1 true
      
(* replace an entire set with a singleton.
   if nothing was there just add the singleton *)
(* IOS.t IH.t -> int -> varinfo -> unit *)
let iosh_replace iosh i vi =
  if IH.mem iosh vi.vid then
    let newset = IOS.singleton (Some i) in
    IH.replace iosh vi.vid newset
  else
    let newset = IOS.singleton (Some i) in
    IH.add iosh vi.vid newset


let proc_defs vs iosh f = 
  let pd vi = 
      let newi = f() in
      iosh_replace iosh newi vi in
  UD.VS.iter pd vs

(* define a module to be used with CIL's DF engine *)
(* NOTE: the "stmtlist" from our code chopper does not contain CIL Blocks, so
 * whenever traversing CFGs, we have to consider the "shortcutted" Blocks *)
module LocalRD slist = struct
    let name = "Local Reaching Definitions"
    let debug = ref false

    let localStmtList: stmt list = slist

    (* Should the analysis calculate may-reach or must-reach *)
    let mayReach = ref true (* use "may" for our purpose *)
    let curSID = ref -1 (* instr has no sid and uses its parent stmt's sid *)
 
    (* The type of the elements in stmtStartData:
        * A hash from variable ids to a set of stmt ids that reach this statement.
        * None in the set means there is a path to this point on which there is 
        * no definition of the variable *)
    (* Update: this is too much for our purpose; we only need the names of the
     * defined vars.
    type t = IOS.t IH.t *)
    type t = UD.VS.t IH.t

    let copy iosh = IH.copy iosh

    (* entries for the starting statements must be added before calling compute *)
    let stmtStartData = IH.create 32

    (* mapping from stmt ids included in localStmtList to statements (or references?)
     * It contains the ids of all "primary" stmts, in addition to the top-level
     * stmts in localStmtList *)
    (* Use the following hashtables to quickly decide whether a stmt is in 
     * localStmtList (deep or shallow). The deep processing makes sense because
     * our code chopper reserves boundaries of If/Switch/Loop (but not Block). *)
    let localSIDHT = IH.create 64
    let isStmtLocal (s:stmt) =
        IH.mem localSIDHT s.sid
    let rec hashSID s = 
        assert (s.sid>=0);
        if not (isStmtLocal s) then
          IH.add localSIDHT s.sid s;
        match s.skind with
          If (e, b1, b2, l) -> 
            hashSID b1;
            hashSID b2
        | Switch (e, b, sl, l) -> (* Probably no use in CIL's CFG *)
            hashSID b;
            hashSIDlist sl
        | Loop (b, l, s1, s2) ->
            hashSID b;
            hashSID s1;
            hashSID s2
        | Block b ->
            hashSIDlist b.bstmts
        | Instr il -> () (* CIL's CFG generation separates each intrs from each other *)
        | Return _ | Goto _ | Break _ | Continue _ -> ()
        | _ -> ()
    and rec hashSIDlist sl = 
        List.iter hashSID sl

    (* NOTE: be careful with "shortcutted" Blocks because our code chopper does
     * not respect Blocks *)
    let isBlock (s:stmt) =
        match s.skind with
          Block _ -> true
        | _ -> false

    (* Not really good criteria because of the "shortcutted" Blocks *)
    let hasNoLocalPred s = 
        if not (isStmtLocal s) then
            false
        else
            List.fold_left (fun i -> not (isStmtLocal i)) true s.preds

    let localTopSIDHT = IH.create 16
    let isStmtLocalTop (s:stmt) =
        IH.mem localTopSIDHT s.sid
    let hashTopSID s =
        assert (s.sid>=0);
        if not (isStmtLocalTop s) then
            IH.add localTopSIDHT s.sid s
    and hashTopSIDlist sl =
        List.iter hashTopSID sl

    (* Init the data inside this module with a given stmt list *)
    let init sl =
        (*localStmtList:=sl;*)
        hashSIDlist sl;
        hashTopSIDlist sl


    (* Decide the starting stmts based on depth-first search of CFG *)
    let getStartStmts () =
        (* Any stmt that has no pred is a starting stmt. <-- NO! 
         * The criteria is NOT right because Blocks are omitted from our code
         * chopper. A compromise is to assume sequential order for the stmts and
         * use the first unvisited stmt as a starting stmt. *)
        let visitedSid = IH.create 64 in
        let getFirstUnvisitedStmt () =
            let uvs = List.filter (fun s -> not (IH.mem visitedSid s.sid)) localStmtList in
            match uvs with
              [] -> None
            | h::r -> Some h
        in
        let rec localCFGdfs s =
            if IH.mem visitedSid s.sid then
                ()
            else (
                IH.add visitedSid s.sid true;
                let localSuccs = List.filter (fun i -> isStmtLocal i || isBlock i) s.succs in
                List.iter localCFGdfs localSuccs
            )
        in
        let rec loop sl = match getFirstUnvisitedStmt () with
                None -> IH.clear visitedSid; sl
            |   Some s -> loop (sl@[s])
        in
            loop []

    (* pretty printer *)
    let pretty () (d:t) = text "TODO"

    let computeFirstPredecessor stm iosh =
        IH.copy iosh

    let combinePredecessors (stm:stmt) ~(old:t) (iosh:t) =
        if iosh_equals old iosh then None 
        else
            Some (iosh_combine old iosh)

    let doInstr inst iosh =
      let transform iosh' =
	let _, defd = UD.computeUseDefInstr inst in
        let getid () = !curSID in
        proc_defs defd iosh' getid; iosh'
      in
      DF.Post transform

    (* most of the work gets done at the instruction level, assuming each
     * instruction has a stmt id *)
    let doStmt stm iosh =
        curSID := stm.sid;
        DF.SDefault

    let doGuard condition _ = DF.GDefault

    (* If a stmt is not in localStmtList (hashed into localSIDHT), 
     * it is excluded from the worklist *)
    let filterStmt stm = (isStmtLocal stm) || (isBlock stm)

end

(** Compute the RDs for a list of stmts with the initial RD data: *)
let stmtlistRDs slist iosh out =

let computeLocalRDs slist =
    stmtlistRDs slist 

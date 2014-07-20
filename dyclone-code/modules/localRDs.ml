(** This is similar to the reachingdefs.ml in CIL, except that it is tailored
 * for dyclone's purpose and computes "local" reaching definitions for an
 * arbitrary code trunk. CFG must have been established before calling this
 * functionality. *)

(* NOTE: CIL's CFG generation seemly transforms each instr in an Instr construct
 * into a stmt, thus giving each instr a sid and making book keeping easier. We
 * so assume this proper: each instr/stmt has a unique id.
 * Update: no, it's not safe to make such an assumption. *)

(* TODO: we may need to change the way CIL counts var defs. We may also
 * need to consider lvalues that are not direct variables, such as *ptr,
 * array[i], struct.field, etc. *)

(* TODO: certain exp in return stmts, e.g., a constant, should be counted as reachingDefs, *)

(* TODO: don't union reachingDefs from different sink stmts together; handle
 * them separately for higher accuracy. Similarly, since RD is a may-analysis,
 * some vars should not be printed at certain cases; otherwise, __dyc_print_
 * may segfault because of some unitialized values for the vars. At least, we
 * should initialize all vars in trunkwrapper.ml. *)

(* TODO: change the may-RD analysis to exclude all *may*-used defs, instead of
 * *must*-used defs; this required changes to the dataflow type from UD.VS to
 * (UD.VS, UD.VS). Let's try must-RD analysis first. *)


open Cil
open Pretty

module E = Errormsg
module DF = Dataflow
module MDF = Mydataflow
(* TODO: It seems CIL's Usedef module isn't all right;
 * we may need to have our own way of deciding defs/uses *)
module UD = Usedef (* help decide which vars are defined/used *)
module IH = Inthash
module Q = Queue

(* NOTE: what is a var def/use (based on CIL's datatypes:
    * We'd better to consider statically decided vars;
    * Writes into array elements, struct/union fields are treated as writes into
    * the array vars, struct/union vars. *)
(* NOTE: For our purpose, we only need to know WHICH vars are defined, 
 * not WHERE they are defined. This makes implementation much easier. *)


(* define a module to be used with CIL's DF engine *)
(* NOTE: the "stmtlist" from our code chopper does not contain CIL Blocks, so
 * whenever traversing CFGs, we have to consider the "shortcutted" Blocks *)
module LocalRDTransfer = struct
    let name = "Local Reaching Definitions"
    let debug = ref false

    (* Should the analysis calculate may-reach or must-reach *)
    let mayReach = ref true (* use "may" for our purpose *)
    let curSID = ref (-1) (* instr has no sid and uses its parent stmt's sid *)
 
    (* The type of the elements in stmtStartData:
        * A hash from variable ids to a set of stmt ids that reach this statement.
        * None in the set means there is a path to this point on which there is 
        * no definition of the variable *)
    (* Update: this is too much for our purpose; we only need the names of the
     * defined vars.
    type t = IOS.t IH.t *)
    type t = UD.VS.t

    let copy iosh = iosh (*???*)

    (* entries for the starting statements must be added before calling compute *)
    let stmtStartData = IH.create 32

    (* Init the data inside this module with a given stmt list *)
    let init sl =
      if !debug then
        ignore(E.log "LocalRDs: initialize stmtStartData etc.\n");
      IH.clear stmtStartData;
      MDF.LocalStmts.init sl

    (* pretty printer *)
    let pretty () (d:t) = 
        UD.VS.fold (fun e v -> v ++ (MDF.docVarinfo () e)) d nil

    let computeFirstPredecessor stm iosh =
        copy iosh

    (* For a simple path-sensitive may-analysis, the "combine" will be the union of 
     * the old and new entries. For a must-analysis, TODO. *)
    let combinePredecessors (stm:stmt) ~(old:t) (iosh:t) =
      let cf = if !mayReach then UD.VS.union else UD.VS.inter in
      let newdata = cf old iosh in
      if UD.VS.equal newdata old then None
      else
        Some newdata

    let doInstr inst iosh =
      let transform old =
	let used, defd = UD.computeUseDefInstr inst in
        (* Opt 1: we only care for WHICH, not WHERE
          UD.VS.union defd old *)
        (* Opt 2, we heuristically filter out used vars as intermediate vars
         * in order to reduce the number of out-variables. *)
        (* We currently remove all fun calls first;
         * TODO: consider to filter out the vars used in fun calls
         * TODO: consider to keep the vars used in fun calls that are
         * in assignments to some lvalues *)
          UD.VS.union defd (UD.VS.diff old used)
        (* Opt 3: the opt 2 only filters out *must*-used defs; we try to do it
         * for all *may*-used defs. Quite a change and not sure about its
         * benefits: TODO. On the other hand, it seems we can achieve
         * "better" results with *must*-RD analysis. *)
      in
      DF.Post transform

    (* most of the work gets done at the instruction level, assuming each
     * instruction has a stmt id *)
    let doStmt stm iosh =
      (* Opt 1: keep the RDs the same: 
        curSID := stm.sid;
        DF.SDefault *)
      (* Opt 2: part of the filter that removes all *must*-used vars: *)
        match stm.skind with
        | Instr _ -> DF.SDefault
        | _ ->
            let used, defd = UD.computeUseDefStmtKind stm.skind in
            (* defd should be empty *)
            DF.SUse (UD.VS.diff iosh used) 

    let doGuard condition _ = DF.GDefault

    (* If a stmt is not in localStmtList (hashed into localSIDHT), 
     * it is excluded from the worklist *)
    let filterStmt stm = (MDF.LocalStmts.isStmtLocal stm) || (MDF.isBlock stm)

end

module LocalRD = DF.ForwardsDataFlow(LocalRDTransfer)

(** Compute the out-RDs for one instr, given its in-RDs *)
let outRDIntr (d: LocalRDTransfer.t) (i: instr) = 
    match LocalRDTransfer.doInstr i d with
      DF.Done d' -> d'
    | DF.Default -> d (* do nothing *)
    | DF.Post f -> f d

(** Compute the out-RDs for one stmt, given its in-RDs *)
let outRDStmt (s:stmt) (d: LocalRDTransfer.t) =
    match s.skind with 
      Instr il -> List.fold_left outRDIntr d il
      (* all the following stmts (not including their substmts) have no effect on RDs. *)
    | Goto _ | Break _ | Continue _ | If _ 
    | TryExcept _ | TryFinally _ 
    | Switch _ | Loop _ | Return _ | Block _ -> d

(** Compute the RDs for a list of stmts with the initial RD data: *)
let stmtlistRDs slist iosh out =
    LocalRDTransfer.init slist;
    (* stmtStartData should be empty now *)
    if !(LocalRDTransfer.debug) then (
        ignore(E.log "LocalRDs: localSIDs: \n");
        IH.iter (fun k v -> ignore(E.log "SID:%d\n" k)) MDF.LocalStmts.localSIDHT
    );

    let localStmtFilter s = MDF.LocalStmts.isStmtLocal s || MDF.isBlock s in
    let startstmts = MDF.getStartStmts localStmtFilter !(MDF.LocalStmts.localStmtList) in
    if !(LocalRDTransfer.debug) then (
        ignore(E.log "LocalRDs: starting stmts: \n");
        List.iter (fun s -> ignore(E.log "%a\n" d_stmt s)) startstmts
    );
    let initstmtdata s = 
      try
        let svalue = IH.find iosh s.sid in
        IH.add LocalRDTransfer.stmtStartData s.sid svalue
      with Not_found -> (ignore(E.log "LocalRDs: missing init data for start stmt: SID=%d\n%a\n" s.sid d_stmt s);
                         raise Not_found)
    in
    (* initialize stmtStartData with the given data "iosh" *)
    List.iter initstmtdata startstmts;
    (* compute the localRDs *)
    LocalRD.compute startstmts;
    (* At this point, stmtStartData should have the in-RDs for each stmt. *)
    (* TODO: CIL's dataflow.ml may have a bug (or feature) there; certain stmts may have
     * incorrect values (only subsets of correct ones) in stmtStartData. It
     * should have the correct values for endStmts though. *)
    let stmtFinalLocalRDsIn s = try IH.find LocalRDTransfer.stmtStartData s.sid
        with Not_found -> UD.VS.empty
    in
    if out then (
        (* compute the out-RDs for the "ending" stmts *)
        let endStmts = MDF.getEndStmts MDF.LocalStmts.isLocalSucc !(MDF.LocalStmts.localStmtList) in
        let rdout s = outRDStmt s (stmtFinalLocalRDsIn s) in
        if !(LocalRDTransfer.debug) then (
            ignore(E.log "LocalRDs: ending stmts: \n");
            List.iter (fun s -> ignore(E.log "%a\n" d_stmt s)) endStmts
        );
        if !(LocalRDTransfer.mayReach) then
          List.fold_left (fun d s -> UD.VS.union d (rdout s)) UD.VS.empty endStmts
        else (
          match endStmts with
          | [] -> ignore(E.log "LocalRDs: Warning: no sink stmts found\n"); UD.VS.empty
          | hd::tl -> List.fold_left (fun d s -> UD.VS.inter d (rdout s)) (rdout hd) tl
        )
    ) else (
        (* collect the in-RDs for the "starting" stmts. Not really useful. *)
        if !(LocalRDTransfer.mayReach) then
          List.fold_left (fun d s -> UD.VS.union d (stmtFinalLocalRDsIn s)) UD.VS.empty startstmts
        else (
          match startstmts with
          | [] -> ignore(E.log "LocalRDs: Warning: no start stmts found\n"); UD.VS.empty
          | hd::tl -> List.fold_left (fun d s -> UD.VS.inter d (stmtFinalLocalRDsIn s)) (stmtFinalLocalRDsIn hd) tl
        )
    )

let computeLocalRDs slist =
    let initiosh = IH.create 16 in
    let emptyiosh s = IH.add initiosh s.sid UD.VS.empty in
    List.iter emptyiosh slist;
    stmtlistRDs slist initiosh true

let dumpLocalRDs ?(oc=stdout) rds =
  (* count the number of different types of rds *)
  (* TODO: may need to classify the rds in a finer-grain; not for now *)
  let cc v (cn, cp) = if isPointerType v.vtype then (cn, cp+1) else (cn+1, cp) in
  let cn, cp = UD.VS.fold cc rds (0,0) in
  UD.VS.iter (fun i -> ignore(MDF.dumpVarinfo oc i)) rds;
  fprintf oc "Total Vardef #: |%d|%d|%d\n" (UD.VS.cardinal rds) cn cp

let outputLocalRDs ?(oc=stdout) slist =
    let v = computeLocalRDs slist in
    dumpLocalRDs ~oc:oc v

    
(* no use if we don't want to invoke it on command line
let feature : featureDescr = 
  { fd_name = "localRDs";
    fd_enabled = ref false;
    fd_description = "Given a stmt list, compute its ending reaching definitions.";
    fd_extraopt = [ 
                ];
    fd_doit = 
        (function (f: file) -> ());
    fd_post_check = false;
  } 
*)

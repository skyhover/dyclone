
(** Calculate which variables are live at each statememnt, in and/or out. *)

open Cil
open Pretty

module UD = Usedef
module IH = Inthash
module E = Errormsg

module MDF = Mydataflow
module VS = UD.VS

let debug = ref false

let live_label = ref ""
let live_func = ref ""


module LocalLiveFlow = struct
  let name = "Local Liveness"
  let debug = debug
  type t = VS.t

  let pretty () (vs:t) =
    VS.fold (fun e v -> v ++ (MDF.docVarinfo () e)) vs nil
    
  let stmtStartData = IH.create 32

  (* Init the data inside this module with a given stmt list *)
  let init sl =
    if !debug then ignore(E.log "LocalLiveness: initialize stmtStartData etc.\n");
    IH.clear stmtStartData;
    MDF.LocalStmts.init sl

  let funcExitData s = VS.empty
  let setInitData s = VS.empty

  let combineSuccessors (stm:stmt) ~(old:t) (now:t) =
    let newdata = VS.union old now in
    if VS.equal newdata old then None
    else
      Some newdata

  let combineStartData = VS.union

  let doStmt s c =
    if !debug then ignore(E.log "LocalLiveness: looking at stmt %d: %a\n" s.sid d_stmt s);
    match s.skind with
    (* since we do backward flow, we cannot simply accumulate use/def for Instr
     * forwardly; we handle Instr separately in doInstr. *)
      Instr il -> MDF.Default
    | _ ->
        let handle_stm vs = 
          let u, d = UD.computeUseDefStmtKind s.skind in
          (* for these stmts, d should be empty *)
          VS.union u (VS.diff vs d)
          (* also, we could filter out function names, but don't have to since
           * we replace all fun calls with random variables (but not enough for
           * fun pointers <- TODO
          VS.union (VS.filter (fun v -> not(isFunctionType v.vtype)) u) (VS.diff vs d) *)
        in
        MDF.Post handle_stm

  let doInstr i c =
    if !debug then ignore(E.log "LocalLiveness: looking at instr: %a\n" d_instr i);
    let transform vs =
      let u, d = UD.computeUseDefInstr i in
      VS.union u (VS.diff vs d)
      (* we could filter out function names (and TODO pointers to functions)
       * Q: should two calls to the same function be replaced with the same
       * input var or two different input vars? Different. 
      VS.union (VS.filter (fun v -> not(isFunctionType v.vtype)) u) (VS.diff vs d) *)
    in
    MDF.Post transform

  let filterStmt pred stmt = MDF.LocalStmts.isLocalPred pred

end

module L = MDF.BackwardsPushDataFlow(LocalLiveFlow)

(** Compute the out-liveness for one instr, given its in-liveness *)
let outLiveInstr i d =
    match LocalLiveFlow.doInstr i d with
        MDF.Post f -> f d
      | _ -> E.s (E.bug "LocalLiveness: inpossible doInstr: %a\n" d_instr i)

(** Compute the out-liveness for one stmt, given its in-liveness *)
let outLiveStmt s d =
  match s.skind with
      Instr il -> List.fold_right outLiveInstr il d
    | _ -> begin
          match LocalLiveFlow.doStmt s d with
                MDF.Post f -> f d
              | _ -> d
        end


(** Compute Liveness for a list of stmts with the initial liveness data: *)
let stmtlistLiveness slist iosh beginning =
    LocalLiveFlow.init slist;
    (* stmtStartData should be empty now *)
    if !(LocalLiveFlow.debug) then (
        ignore(E.log "LocalLiveness: localSIDs: \n");
        IH.iter (fun k v -> ignore(E.log "SID:%d\n" k)) MDF.LocalStmts.localSIDHT
    );

    (* XXX: This does not compute the best ordering of the sinks to 
     * give to the work-list algorithm.  *)
    let sinkStmts = MDF.getEndStmts MDF.LocalStmts.isLocalSucc !(MDF.LocalStmts.localStmtList) in
    if !(LocalLiveFlow.debug) then (
      ignore(E.log "LocalLiveness: %d Sink stmts: \n" (List.length sinkStmts));
      List.iter (fun s -> ignore(E.log "sid:%d:%a\n" s.sid d_stmt s)) sinkStmts
    );
    let initstmtdata s = 
      try
        let svalue = IH.find iosh s.sid in
        IH.add LocalLiveFlow.stmtStartData s.sid svalue
      with Not_found -> (
        (* For convenience, just set them to empty *)
        IH.add LocalLiveFlow.stmtStartData s.sid (LocalLiveFlow.funcExitData s)
        (*
        ignore(E.log "LocalLiveness: missing init data for sink stmt: SID=%d\n%a\n" s.sid d_stmt s);
        raise Not_found
        *)
      )
    in
    (* initialize stmtStartData with the given data "iosh" *)
    List.iter initstmtdata sinkStmts;
    (* compute the localLiveness *)
    L.compute sinkStmts;
    (* At this point, stmtStartData should have the in-Liveness for each stmt. *)
    (* TODO: mydataflow.ml may have a bug (or feature) there; certain stmts may have
     * incorrect values (only subsets of correct ones) in stmtStartData. It
     * should have the correct values for startstmts though. *)
    let stmtFinalLocalLivenessIn s = try
          let l = IH.find LocalLiveFlow.stmtStartData s.sid in
          if !debug then
            ignore(E.log "LocalLiveness: liveness for stmt %d: %a\n" s.sid LocalLiveFlow.pretty l);
          if l == VS.empty then raise Not_found;
          l
        with Not_found -> (
          if !debug then
            ignore(E.log "LocalLiveness: Warning: empty liveness for stmt %d: %a\n" s.sid d_stmt s);
          VS.empty
        )
    in
    if beginning then (
      (* compute the out-liveness for the "starting" stmts *)
      let localStmtFilter s = MDF.LocalStmts.isStmtLocal s || MDF.isBlock s in
      let startstmts = MDF.getStartStmts localStmtFilter !(MDF.LocalStmts.localStmtList) in
      if !(LocalLiveFlow.debug) then (
        ignore(E.log "LocalLiveness: starting stmts: \n");
        List.iter ( fun s -> ignore(E.log "sid %d: %a\n" s.sid d_stmt s) ) startstmts (*
                            if (List.length s.preds)==0 then
                              ignore(E.log "pred empty\n")
                            else
                              List.iter (fun sp -> ignore(E.log "pred sid %d: %a\n" sp.sid d_stmt sp)) s.preds
                  ) startstmts *)
      );
      let outlive s = outLiveStmt s (stmtFinalLocalLivenessIn s) in
      (* TODO: union all startstmts together may not be good since some
       * startstmts may not be reachable...remove dead code after CFG
       * contruction: have to call "elim_dead_code" in deadcodelim.ml *)
      List.fold_left (fun d s -> VS.union d (outlive s)) VS.empty startstmts
    ) else (
      (* collect the in-liveness for the "ending" stmts. Not really useful. *)
      List.fold_left (fun d s -> VS.union d (stmtFinalLocalLivenessIn s)) VS.empty sinkStmts
    )

let computeLocalLiveness slist =
    let initiosh = IH.create 16 in
    let emptyiosh s = IH.add initiosh s.sid VS.empty in
    List.iter emptyiosh slist;
    stmtlistLiveness slist initiosh true

let dumpLocalLiveness ?(oc=stdout) ins =
  (* count the number of different types of rds *)
  (* TODO: may need to classify the ins in a finer-grain; not for now *)
  let cc v (cn, cp) = if isPointerType v.vtype then (cn, cp+1) else (cn+1, cp) in
  let cn, cp = UD.VS.fold cc ins (0,0) in
  VS.iter (fun i -> ignore(MDF.dumpVarinfo oc i)) ins;
  fprintf oc "Total Live Vars #: |%d|%d|%d\n" (VS.cardinal ins) cn cp

let outputLocalLiveness ?(oc=stdout) slist =
    let v = computeLocalLiveness slist in
    dumpLocalLiveness ~oc:oc v


(* TODO: We may need to treat function names/arguments differently.
 * Update: now we replace all fun calls before chopping. The only prblem left
 * with functions is when fun names are used as pointers (not calls) *)


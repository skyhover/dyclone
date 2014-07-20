
(* (manually) define the nodes used in vector genration for Deckard *)
(* since we only consider stmt list (from the code chopper) for now,
 * we here only need to handle these CIL types:
   * stmt, instr, exp, lval, lhost, offset, unop, binop
 * we don't have to handle these since they should not appear in stmt list:
   * fundec, typ, compinfo, etc.
 *)

open Pretty
open Cil
module E = Errormsg

type deckardNode = 
  | DKNs of stmt
  | DKNi of instr
  | DKNe of exp
  | DKNl of lval
  | DKNlh of lhost
  | DKNo of offset
  | DKNu of unop
  | DKNb of binop
  | DKNt of typ
  | DKNempty
;;

let deckardDimension = 50;;
let dkNode2Id dkn =
  match dkn with
  (* stmt.skind *)
  | DKNs s ->
      (
        match s.skind with
        | Return _ -> 1
        | Goto _ -> 2
        | Break _ -> 3
        | Continue _ -> 4
        | If _ -> 5
        | Switch _ -> 6
        | Loop _ -> 7
        | TryFinally _ -> 8
        | TryExcept _ -> 9
        | Instr _ | Block _ -> -1  (* ignore these semantically insignificant nodes *)
      )
  (* instr *)
  | DKNi(Set _) -> 10
  | DKNi(Call _) -> 11
  | DKNi(Asm _) -> 12
  (* exp *)
  | DKNe(Const _) -> 13  (* same for all kinds of constants *)
  | DKNe(Lval _) -> 14
  | DKNe(SizeOf _) -> 15
  | DKNe(SizeOfE _) -> 15
  | DKNe(SizeOfStr _) -> 16
  | DKNe(AlignOf _) -> 17
  | DKNe(AlignOfE _) -> 17
  | DKNe(UnOp _) -> 18
  | DKNe(BinOp _) -> 19
  | DKNe(CastE _) -> -1
  | DKNe(AddrOf _) -> 20
  | DKNe(StartOf _) -> 20
  (* lval *)
  | DKNl(_) -> -1  (* ignore itself, but consider its components *)
  | DKNlh(Var _) -> 21
  | DKNlh(Mem _) -> 22
  | DKNo(NoOffset) -> -1  (* ignore *)
  | DKNo(Field _) -> 23
  | DKNo(Index _) -> 24
  (* unop *)
  | DKNu(Neg) -> 25
  | DKNu(BNot) -> 26
  | DKNu(LNot) -> 27
  (* binop *)
  | DKNb(PlusA) -> 28
  | DKNb(PlusPI) -> 28
  | DKNb(IndexPI) -> 28
  | DKNb(MinusA) -> 29
  | DKNb(MinusPI) -> 29
  | DKNb(MinusPP) -> 29
  | DKNb(Mult) -> 30
  | DKNb(Div) -> 31
  | DKNb(Mod) -> 32
  | DKNb(Shiftlt) -> 33
  | DKNb(Shiftrt) -> 34
  | DKNb(Lt) -> 35
  | DKNb(Gt) -> 35
  | DKNb(Le) -> 35
  | DKNb(Ge) -> 35
  | DKNb(Eq) -> 36
  | DKNb(Ne) -> 36
  | DKNb(BAnd) -> 37
  | DKNb(BXor) -> 38
  | DKNb(BOr) -> 39
  | DKNb(LAnd) -> 40
  | DKNb(LOr) -> 41
  | DKNt(TVoid _) -> 42
  | DKNt(TInt _) -> 43
  | DKNt(TFloat _) -> 43
  | DKNt(TEnum _) -> 43
  | DKNt(TPtr _) -> 44
  | DKNt(TArray _) -> 45
  | DKNt(TFun _) -> 46
  | DKNt(TNamed(ti,_)) -> -1  (* but we may count its component *)
  | DKNt(TComp _) -> 47
  | DKNt(TBuiltin_va_list _) -> 48
  | _ -> -1
;;

let id2Dknode id =
  (* inconvenient to reverse *)
  DKNempty
;;

let incrVec vec id =
  if (id<0 || id>=Array.length vec) then
    ()
  else
    vec.(id) <- vec.(id) + 1
;;

let vecSize vec =
  Array.fold_left (fun v c -> v+c) 0 vec
;;

let outputVec ?(oc=stdout) v =
  Array.iter (fun i -> ignore(fprintf oc "%d " i)) v;
  ignore(fprintf oc "\n")
;;

class nodeVisitor vec = object (self)
  inherit nopCilVisitor

  method vexpr (e:exp) = 
    let eid = dkNode2Id(DKNe e) in
    incrVec vec eid;
    DoChildren

  method vlval (l:lval) =
    let lid = dkNode2Id(DKNl l) in
    incrVec vec lid;
    DoChildren

  method voffs (o:offset) =
    let oid = dkNode2Id(DKNo o) in
    incrVec vec oid;
    DoChildren

  method vinitoffs (o:offset) =
    let oid = dkNode2Id(DKNo o) in
    incrVec vec oid;
    DoChildren  (* initializer offset *)

  method vinst (i:instr) =
    let iid = dkNode2Id(DKNi i) in
    incrVec vec iid;
    DoChildren

  method vstmt (s:stmt) =
    let sid = dkNode2Id(DKNs s) in
    incrVec vec sid;
    DoChildren

  (* ignore block: method vblock (b: block) = DoChildren *)

  (* stmt may use some types, in sizeof, e.g., so we count those *)
  method vtype (t:typ) =
    let tid = dkNode2Id(DKNt t) in
    incrVec vec tid;
    DoChildren

end


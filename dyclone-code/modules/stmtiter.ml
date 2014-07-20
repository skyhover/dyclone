
open Pretty
open Cil
module E = Errormsg
module H = Hashtbl
module UD = Usedef
module SS = Set.Make (struct
    type t = string
    let compare = String.compare
  end);;


class stmtVisitor = object (self)
  inherit nopCilVisitor
  val mutable sctotal = 0 (* total stmt counts (including instrs) for a file *)
  val mutable fsctotal = 0 (* total stmt counts for a function *)
  val mutable sctop = 0 (* total top stmt counts for a file *)
  val mutable fsctop = 0 (* total top stmt counts for a function *)
  val mutable fvuse = UD.VS.empty (* the vars used in a function *)
  val mutable fvdef = UD.VS.empty (* the vars defined in a function *)

  method vstmt (s : stmt) : stmt visitAction =
    (match s.skind with
    | Instr il ->
        sctotal <- sctotal + (List.length il);
        fsctotal <- fsctotal + (List.length il)
    | Block _ -> ()
    | _ -> 
        sctotal <- sctotal + 1;
        fsctotal <- fsctotal + 1
    );
    let u, d = UD.computeUseDefStmtKind s.skind in
    fvuse <- UD.VS.union fvuse u;
    fvdef <- UD.VS.union fvdef d;
    DoChildren

  method vfunc f =
    (* let sct = List.length f.sbody.bstmts in *)
    (* this is better for matching our code chopper *)
    let rec c v s =
      match s.skind with
      | Instr il -> v + (List.length il)
      | Block b -> List.fold_left c v b.bstmts
      | _ -> v + 1
    in
    let sct = List.fold_left c 0 f.sbody.bstmts in
    sctop <- sctop + sct;
    fsctotal <- 0; fsctop <- sct;
    fvuse <- UD.VS.empty; fvdef <- UD.VS.empty;
    let outf f' =
      print_char ' '; print_string f.svar.vname;
      print_char ' '; print_int fsctop;
      print_char ' '; print_int fsctotal;
      print_char ' '; print_int (UD.VS.cardinal fvuse);
      print_char ' '; print_int (UD.VS.cardinal fvdef);
      print_newline ();
      f
    in
    ChangeDoChildrenPost (f, outf)

end

let featureEntry (f:file) =
  let aaVisitor = new stmtVisitor in
  print_string ("FILE:" ^ f.fileName);
  print_newline ();
  visitCilFileSameGlobals aaVisitor f;
;;

let feature : featureDescr = 
  { fd_name = "stmtiter";
    fd_enabled = ref false;
    fd_description = "Visit each statement via CIL's visitor pattern.";
    fd_extraopt = [ ];
    fd_doit = featureEntry;
    fd_post_check = false;
  } 


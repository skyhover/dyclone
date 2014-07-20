
open Pretty
open Cil
module E = Errormsg
module H = Hashtbl
(* module stmt_set = 
    Set.Make ( struct
        type t = stmt
        let compare = compare
    end ) (* Q: only for ordered types? *)
*)

(* TODO: 
    * decide whether preorder or postorder is better
    * using Hashtbl will be more efficient than List.
    *)
let methodwrapper = object (self)
    val mutable visitedStmts = []
    val mutable sctotal = 0
    method funCFGdfs s =
        if List.mem s visitedStmts then
            ()
        else (
            (* preorder process of s *)
            visitedStmts <- s :: visitedStmts;
            sctotal <- sctotal + 1;
            print_int sctotal;
            print_string "  stmt id: ";
            print_int s.sid;
            print_newline ();
            dumpStmt Cil.defaultCilPrinter stdout 3 s;
            print_newline ();
            List.iter self#funCFGdfs s.succs
            (* postorder process of s *)
        )

    method funCFGdfs_succs s =
        if List.mem s visitedStmts then
            ()
        else (
            (* preorder process of s *)
            visitedStmts <- s :: visitedStmts;
            sctotal <- sctotal + 1;
            print_int sctotal;
            print_string "  stmt id: ";
            print_int s.sid;
            print_newline ();
            dumpStmt Cil.defaultCilPrinter stdout 3 s;
            print_newline ();
            print_string "succs stmt ids: ";
            List.iter (fun i -> print_int i.sid; print_string "  ") s.succs;
            print_newline ();
            List.iter self#funCFGdfs_succs s.succs
            (* postorder process of s *)
        )
end

class fileCFGVisitor = object (self)
  inherit nopCilVisitor

  method vfunc (f : fundec) : fundec visitAction =
      (* for each function, traverse the intra-procedural CFG *)
      methodwrapper#funCFGdfs_succs (List.nth f.sallstmts 0);
      SkipChildren

end

let feature : featureDescr = 
  { fd_name = "cfgdfs";
    fd_enabled = ref false;
    fd_description = "Traverse CFG of a file.";
    fd_extraopt = [ ];
    fd_doit = 
    (function (f: file) -> 
      let aaVisitor = new fileCFGVisitor in
      visitCilFileSameGlobals aaVisitor f);
    fd_post_check = false;
  } 


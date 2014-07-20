
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
    * using Hashtbl and Queue will be more efficient than List.
    *)
let bfswrapper = object (self)
    val mutable visitedStmts = []
    val mutable queuedStmts = []
    val mutable sctotal = 0
    method funCFGbfs s =
        if List.mem s visitedStmts then
            ()
        else (
            queuedStmts <- queuedStmts@[s];
            while List.length queuedStmts > 0 do
              let (hd :: tl) = queuedStmts in
                queuedStmts <- tl;
                if List.mem hd visitedStmts then
                    ()
                else (
                    (* preorder process of hd *)
                    visitedStmts <- hd :: visitedStmts;
                    sctotal <- sctotal + 1;
                    print_int sctotal;
                    print_string "  stmt id: ";
                    print_int hd.sid;
                    print_newline ();
                    dumpStmt Cil.defaultCilPrinter stdout 3 hd;
                    print_newline ();
                    print_string "succs stmt ids: ";
                    List.iter (fun i -> print_int i.sid; print_string "  ") hd.succs;
                    print_newline ();
                    List.iter (fun i -> if List.mem i visitedStmts then () else queuedStmts <- queuedStmts@[i]) hd.succs
                    (* postorder process of s *)
                )
            done
        )
end

class fileCFGBFS = object (self)
  inherit nopCilVisitor

  method vfunc (f : fundec) : fundec visitAction =
      (* for each function, traverse the intra-procedural CFG *)
      bfswrapper#funCFGbfs (List.nth f.sallstmts 0);
      SkipChildren

end

let feature : featureDescr = 
  { fd_name = "cfgbfs";
    fd_enabled = ref false;
    fd_description = "Traverse CFG of a file in a breadth-first style. Must be used with --domakeCFG";
    fd_extraopt = [ ];
    fd_doit = 
    (function (f: file) -> 
      let aaVisitor = new fileCFGBFS in
      visitCilFileSameGlobals aaVisitor f);
    fd_post_check = false;
  } 


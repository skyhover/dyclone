(** assume the input file is a code trunk:
  * find all __dyc_read_ functions in __dyc_foo,
  * construct the same number of __dyc_random_ and __dyc_print_ functions
 * Update: we can do all these in the trunkwrapper with less effort. *)

open Pretty
open Cil
module E = Errormsg
module SS = Set.Make (struct
    type t = string
    let compare = String.compare
  end);;

let funNameSet = ref SS.empty;;

class codeInfoVisitor = object (self)
  inherit nopCilVisitor

  method vfunc (f : fundec) : fundec visitAction =
    funNameSet := SS.add f.svar.vname !funNameSet;
    SkipChildren

end

let featureEntry (f:file) =
  let aaVisitor = new codeInfoVisitor in
  visitCilFileSameGlobals aaVisitor f;
  (* print funNameSet *)
  print_string f.fileName;
  SS.iter (fun s -> print_char ' '; print_string s) !funNameSet;
  print_newline ()
;;

let feature : featureDescr = 
  { fd_name = "codeinfo";
    fd_enabled = ref false;
    fd_description = "Calculate bastic statistics about a C file before hand for other uses. Output to stdout";
    fd_extraopt = [ ];
    fd_doit = featureEntry;
    fd_post_check = false;
  } 


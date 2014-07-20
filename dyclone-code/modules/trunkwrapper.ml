(* The file aims to make a code trunk compilable:
  * Replace function calls with a new in-variable;
  * Replace undefined labels;
  * Replace all return stmts with "goto __dyc_dummy_label;"
  * Construct "__dyc_foo" function that wraps the code trunk:
    * Construct variable declarations (sformals, slocals, globals, etc.)
    * Construct outputs
  * Collect all type definitions in the .i file;
  * Construct __dyc_random_ functions for each type;
  * Construct __dyc_read_ functions for each type;
  * Construct __dyc_print_ functions for each type;
  * Remove asm;
  * Remove Set instr that have no side-effects; (Done by CIL itself)
  * handle function pointers:
    * only handle the pointer type, not the fun type. *)

open Cil
open Pretty

module E = Errormsg
module UD = Usedef
module MDF = Mydataflow

module LS = Set.Make (struct 
                        type t = string
                        let compare = Pervasives.compare
                      end)
(* Or, use the Hashtbl module with in-place modifcation *)

(** debug flag *)
let debug = ref false;;

(** safeguad the types we support. 
 * For unsupported types, we:
  * NOT construct __dyc_ functions for them;
  * NOT count them as invars/rds, so no calls to__dyc_random/print/read_ for them *)
let rec isSupportedType t =
  match t with
    | TVoid _ -> false
    | TInt _ -> true
    | TFloat _ -> true
    | TPtr _ -> true
    | TArray _ -> false
    | TFun _ -> false
    | TNamed _ -> (
        let nt = unrollType t in
        match nt with
        | TNamed _ -> E.s (E.bug "isSupportedType: impossible\n")
        | _ -> isSupportedType nt
      )
    | TComp _ -> true (* even if no field in the struct/union is supported *)
    | TEnum _ -> true (* treat it as an int for now *)
    | TBuiltin_va_list _ -> false

(** Check whether a type can be used with sizeof.
 * This may be compiler-dependent. *)
let isSizeofType t =
  match unrollType t with
  | TArray(t, None, _) -> false (* but "true" if it is within a comp *)
  | TArray(t, Some z, _) when isZero z -> true (* size==0 *)
  | TComp (comp, _) -> (* Struct or union *)
      comp.cdefined (* reply on the frontend of cil *)
      (* List.for_all (fun fi -> isCompleteType fi.ftype) comp.cfields *)
  | _ -> true


(* return the name for a type *)
let rec getTypeName t = 
  match t with
    | TVoid _ -> "_void"
    | TInt(ik, a) -> (
      (* classify the types based on their sizes; otherwise, 
       * it may cause unexpected results for __dyc_print_ functions;
       * sign/unsign doesn't matter though. *)
        match ik with
        |   IChar   (*  char*)
        |   ISChar  (*  signed char*)
        |   IUChar  (*  unsigned char*)
        -> "_char"
        |   IInt  (*  int*)
        |   IUInt   (*  unsigned int*)
        -> "_int"
        |   IShort  (*  short*)
        |   IUShort   (*  unsigned short*)
        -> "_short"
        |   ILong   (*  long*)
        |   IULong  (*  unsigned long*)
        -> "_long"
        |   ILongLong   (*  long long (or _int64 on Microsoft Visual C) *)
        |   IULongLong  (*  unsigned long long (or unsigned _int64 on Microsoft Visual C) *)
        -> "_longlong"
    )
    | TFloat (FFloat, _) -> "_float"
    | TFloat (_, _) -> "_double"
    | TPtr (tp, a) -> "_ptr_" ^ (getTypeName tp)
    | TArray (tp, e, a) -> "_array" ^ (getTypeName tp)
    | TFun _ -> "_fun_name_is_not_here"
    | TNamed (tp, a) -> "_typdef_" ^ tp.tname
    | TComp (tp, a) -> "_comp_" ^ (string_of_int tp.ckey) ^ tp.cname
    | TEnum (tp, a) -> "_enum_" ^ tp.ename
    | TBuiltin_va_list _ -> "_builtin_va_list"
;;
let getTypeNameUnroll t =
  let nt = unrollType t in
  getTypeName nt
;;

let typeFunNamePrefixRandom = "__dyc_random";;
let typeFunNamePrefixRead = "__dyc_read";;
let typeFunNamePrefixPrint = "__dyc_print";;
let getFunNameTypeName ?(prefix=typeFunNamePrefixRandom) tn = prefix ^ tn;;
let getFunNameType ?(prefix=typeFunNamePrefixRandom) t = prefix ^ (getTypeName t);;

let getTypeNameFun tn fmap =
  Hashtbl.find fmap tn
;;
let rec getTypeFun t fmap =
  let tn = getTypeName t in
  try
    Hashtbl.find fmap tn
  with Not_found -> (
    match t with
    | TNamed(tp, a) -> getTypeFun tp.ttype fmap
    | _ -> raise Not_found
  )
;;


let getFunNameTypeUnroll ?(prefix=typeFunNamePrefixRandom) t = 
  let nt = unrollType t in
  match nt with
  | TEnum _ | TInt _ | TFloat _ -> prefix ^ "pre_byte"
  | _ -> getFunNameType ~prefix:prefix nt
;;

(** constrct a fun call to a __dyc_random_ function for var "vname" *)
let typeGenAssignmentRandom (var:varinfo) (t:typ) fmap : instr list =
  if not(isSupportedType t) || not(isSizeofType t) then
    []
  else (
    let tn = getTypeNameUnroll t in
    let callf =
      try
        let t,f1,f2,f3 = Hashtbl.find fmap tn in
        f1.svar
      with Not_found ->
        let fn = getFunNameTypeName ~prefix:typeFunNamePrefixRandom tn in
        makeVarinfo false fn (TFun(t, Some [("exp", uintType, [])], false, []))
    in
    [ Call(Some (Var(var),NoOffset), Lval(Var(callf),NoOffset), [zero], locUnknown) ]
  )
;;

(** constrct a fun call to a __dyc_read_ function for var "vname" *)
let typeGenAssignmentRead (var:varinfo) (t:typ) fmap : instr list =
  if not(isSupportedType t) || not(isSizeofType t) then
    []
  else (
    let tn = getTypeNameUnroll t in
    let callf =
      try
        let t,f1,f2,f3 = Hashtbl.find fmap tn in
        f2.svar
      with Not_found ->
        let fn = getFunNameTypeName ~prefix:typeFunNamePrefixRead tn in
        makeVarinfo false fn (TFun(t, Some [], false, []))
    in
    [ Call(Some (Var(var),NoOffset), Lval(Var(callf),NoOffset), [], locUnknown) ]
  )
;;

(** constrct a fun call to a __dyc_print_ function for var "vname" *)
let typeGenCallPrint (var:varinfo) (t:typ) fmap : instr list =
  if not(isSupportedType t) || not(isSizeofType t) then
    []
  else (
    let tn = getTypeNameUnroll t in
    let callf =
      try
        let t,f1,f2,f3 = Hashtbl.find fmap tn in
        f3.svar
      with Not_found ->
        let fn = getFunNameTypeName ~prefix:typeFunNamePrefixPrint tn in
        makeVarinfo false fn (TFun(voidType, Some [(var.vname, t, [])], false, []))
    in
    [ Call(None, Lval(Var(callf),NoOffset), [Lval(Var(var),NoOffset)], locUnknown) ]
  )
;;

(** construct an instr that sets a var to 0.
 * TODO: can't use "&" on register variables etc.; currently we treat different
 * types differently, it should be ok most of the time. *)
let varGenInit (var:varinfo) : instr list =
  if not(isSupportedType var.vtype) || not(isSizeofType var.vtype) then
    []
  else (
    match unrollType var.vtype with
    | TEnum _ | TInt _ | TFloat _ | TPtr _ -> 
        [ Set((Var(var),NoOffset), zero, locUnknown) ]
    | _ -> 
        let callmemset = makeVarinfo false "memset" (TFun(voidPtrType, Some[("s", voidPtrType, []); ("c", intType, []); ("n", uintType, [])], false, [])) in
        [ Call(None, Lval(Var(callmemset),NoOffset), [mkAddrOrStartOf (Var(var),NoOffset); zero; SizeOf(var.vtype)], locUnknown) ]
  )
;;

(** construct a __dyc_random_ function for type t *)
(* precondition: t is supported *)
let typeGenFunRandom (tt:typ) : fundec =
  if !debug then
    ignore(E.log "Trunkwrapper: construct __dyc_random for type: %a\n" d_type tt);
  let t = MDF.typeRemoveAllDeepAttributes tt in
  match t with
  | TEnum _ | TInt _ | TFloat _ -> (
      let f = emptyFunction (typeFunNamePrefixRandom ^ "pre_byte") in
      f.svar.vtype <- TFun(charType, Some [("__dyc_exp", uintType, [])], false, []);
      f.svar.vstorage <- Extern;
      f
    )
  | TPtr (tp, a) -> (
      let f = emptyFunction (getFunNameType ~prefix:typeFunNamePrefixRandom t) in
      f.svar.vtype <- TFun(t, Some [], false, []);
      let farg = makeVarinfo false "__dyc_exp" uintType in (* the argument controls the probability of null pointers *)
      setFormals f [farg];
      f.svar.vstorage <- Extern;
      (* construct the func body:
        * randomize the pointer to NULL or a new memory cell. *)
      let fbody =
        if !debug then
          ignore(E.log "Trunkwrapper: TPtr(type): %a\n" d_type tp);
        if (isSupportedType tp) && (isSizeofType tp) (* so that "sizeof" makes sense *) then (
          let fptr = emptyFunction (typeFunNamePrefixRandom ^ "pre_ptr") in
          fptr.svar.vtype <- TFun(intType, Some [("__dyc_exp", uintType, [])], false, []);
          let callalloc = makeVarinfo false "calloc" (TFun(voidPtrType, Some[("nmemb", uintType, []); ("size", uintType, [])], false, [])) in
          let tpname = getFunNameTypeUnroll ~prefix:typeFunNamePrefixRandom tp in
          let tpf = makeVarinfo false tpname (TFun(tp, Some [("__dyc_exp", uintType, [])], false, [])) in
          Formatcil.cStmts
            "%t:thistype __dyc_rsl = 0;
             int __dyc_tmp_ptr = %l:fexp (exp);
             if ( __dyc_tmp_ptr != 0 ) {
               exp += 1;
               __dyc_rsl = %v:callalloc ( 1, sizeof(%t:targettype) );
               *__dyc_rsl = %v:tpf (exp);
               exp -= 1;
             }
             return __dyc_rsl;"
            (fun n t -> makeLocalVar f n t) (* used to add the local decls into the fundec *)
            locUnknown
            [ ("thistype", Ft t);
              ("fexp", Fl (Var(fptr.svar),NoOffset));
              ("exp", Fv farg);
              ("callalloc", Fv callalloc);
              ("targettype", Ft tp);
              ("tpf", Fv tpf) ]
        ) else (
          f.svar.vtype <- TFun(voidPtrType, Some [("__dyc_exp", uintType, [])], false, []); (* change return type *)
          Formatcil.cStmts "return 0;"
              (fun n t -> makeLocalVar f n t) (* no use here *)
              locUnknown [] 
        )
      in
      f.sbody.bstmts <- fbody;
      f
    )
  | TNamed (td, a) -> (
    (* if 't' is supported, 'td.ttype' must also be supported *)
      let f = emptyFunction (getFunNameType ~prefix:typeFunNamePrefixRandom t) in
      f.svar.vtype <- TFun(t, Some [], false, []);
      let farg = makeVarinfo false "__dyc_exp" uintType in
      setFormals f [farg];
      f.svar.vstorage <- Extern;
      let fbody = 
        let tdname = getFunNameTypeUnroll ~prefix:typeFunNamePrefixRandom td.ttype in
        let tdf = makeVarinfo false tdname (TFun(td.ttype, Some [("__dyc_exp", uintType, [])], false, [])) in
        Formatcil.cStmts
            "%t:thistype __dyc_rsl = %v:tdf (exp);
             return __dyc_rsl;"
            (fun n t -> makeLocalVar f n t) (* used to add the local decls into the fundec *)
            locUnknown
            [ ("thistype", Ft t);
              ("tdf", Fv tdf);
              ("exp", Fv farg) ]
      in
      f.sbody.bstmts <- fbody;
      f
    )
  | TComp (cc, a) -> (
      if !debug then
        ignore(E.log "Trunkwrapper: TComp(%s): complete? %s; sizeof? %s\n" (compFullName cc) (if isCompleteType t then "yes" else "no") (if cc.cdefined then "yes" else "no"));
      let f = emptyFunction (getFunNameType ~prefix:typeFunNamePrefixRandom t) in
      f.svar.vtype <- TFun(t, Some [], false, []);
      let farg = makeVarinfo false "__dyc_exp" uintType in
      setFormals f [farg];
      f.svar.vstorage <- Extern;
      (* Hack 09/08/2008: add an extra call to __dyc_randompre_ptr and an "if" in the fbody;
       * This is to make it so that it has more than 50% chances that all pointer members are NULL,
       * so that recursive calls always have more than 50% chances to terminate before stack overflow.
       * Update: 09/11/2008: The above (50%) is still not good enough, so we revised it again so that
       * all __dyc_random_ is of an exponential decade kind of style. *)
      let fbody = 
        let ccc = List.filter ( fun fd -> fd.fname != missingFieldName && isSupportedType fd.ftype && isSizeofType fd.ftype ) cc.cfields in (* remove unsupported fields *)
        let assignfield fd =
          let fdname = getFunNameTypeUnroll ~prefix:typeFunNamePrefixRandom fd.ftype in
          let fdf = makeVarinfo false fdname (TFun(fd.ftype, Some [("__dyc_exp", uintType, [])], false, [])) in
          (fd.fname, Fv fdf)
        in
        let holes = List.map assignfield ccc in
        let patterns = List.fold_left (fun pat h -> pat ^ "__dyc_rsl." ^ (fst h) ^ " = %v:" ^ (fst h) ^ "(__dyc_exp);\n") "\n" holes in
        (* for the Hack 09/08/2008, disabled:
        let assignfield0 pat fd =
          if isPointerType fd.ftype then (
            pat ^ "__dyc_rsl." ^ fd.fname ^ " = 0;\n"
          ) else (
            pat ^ "__dyc_rsl." ^ fd.fname ^ " = %v:" ^ fd.fname ^ "(__dyc_exp);\n"
          )
        in
        let patterns0 = List.fold_left assignfield0 "" ccc in
        let fptr = emptyFunction (typeFunNamePrefixRandom ^ "pre_ptr") in
        fptr.svar.vtype <- TFun(intType, Some [("__dyc_exp", uintType, [])], false, []); *)
        Formatcil.cStmts
            ( "%t:__dyc_thistype __dyc_rsl; " (* for the hack:
               "int __dyc_ptrcontrol = %l:__dyc_fptr ();
               if ( __dyc_ptrcontrol == 0 ) {
                 " ^ patterns0 ^
              "} else {
                " *) ^ patterns ^
              "return __dyc_rsl;" )
            (fun n t -> makeLocalVar f n t) (* used to add the local decls into the fundec *)
            locUnknown
            ( [ ("__dyc_thistype", Ft t);
                (* for the hack: ("__dyc_fptr", Fl(Var(fptr.svar),NoOffset)); *)
                ("__dyc_exp", Fv farg) ] @ holes )
      in
      f.sbody.bstmts <- fbody;
      f
    )
  | _ -> E.s (E.bug "typeGenFunRandom: not supported type: %a\n" d_type t)

(** construct a __dyc_read_ function for type t *)
(* precondition: t is supported *)
let typeGenFunRead (tt:typ) : fundec =
  if !debug then
    ignore(E.log "Trunkwrapper: construct __dyc_read for type: %a\n" d_type tt);
  let t = MDF.typeRemoveAllDeepAttributes tt in
  match t with
  | TEnum _ | TInt _ | TFloat _ -> (
      let f = emptyFunction (typeFunNamePrefixRead ^ "pre_byte") in
      f.svar.vtype <- TFun(intType, Some [], false, []);
      f.svar.vstorage <- Extern;
      f
    )
  | TPtr (tp, a) -> (
      let f = emptyFunction (getFunNameType ~prefix:typeFunNamePrefixRead t) in
      f.svar.vtype <- TFun(t, Some [], false, []);
      f.svar.vstorage <- Extern;
      (* construct the func body: set the pointer to NULL or a new memory cell. *)
      let fbody =
        let fptr = emptyFunction (typeFunNamePrefixRead ^ "pre_ptr") in
        fptr.svar.vtype <- TFun(intType, Some [], false, []);
        if (isSupportedType tp) && (isSizeofType tp) (* so that "sizeof" makes sense *) then (
          let callalloc = makeVarinfo false "calloc" (TFun(voidPtrType, Some[("nmemb", uintType, []); ("size", uintType, [])], false, [])) in
          let tpname = getFunNameTypeUnroll ~prefix:typeFunNamePrefixRead tp in
          let tpf = makeVarinfo false tpname (TFun(tp, Some [], false, [])) in
          Formatcil.cStmts
            "%t:thistype __dyc_rsl = 0;
             int __dyc_tmp_ptr = %l:fexp ();
             if ( __dyc_tmp_ptr != 0 ) {
               __dyc_rsl = %v:callalloc ( 1, sizeof(%t:targettype) );
               *__dyc_rsl = %v:tpf ();
             }
             return __dyc_rsl;"
            (fun n t -> makeLocalVar f n t) (* used to add the local decls into the fundec *)
            locUnknown
            [ ("thistype", Ft t);
              ("fexp", Fl (Var(fptr.svar),NoOffset));
              ("callalloc", Fv callalloc);
              ("targettype", Ft tp);
              ("tpf", Fv tpf) ]
        ) else (
          f.svar.vtype <- TFun(voidPtrType, Some [], false, []); (* change return type *)
          Formatcil.cStmts
              "%l:fexp (); /*consume one input*/
               return 0;"
              (fun n t -> makeLocalVar f n t) (* no use here *)
              locUnknown [("fexp", Fl (Var(fptr.svar),NoOffset))] 
        )
      in
      f.sbody.bstmts <- fbody;
      f
    )
  | TNamed (td, a) -> (
    (* if 't' is supported, 'td.ttype' must also be supported *)
      let f = emptyFunction (getFunNameType ~prefix:typeFunNamePrefixRead t) in
      f.svar.vtype <- TFun(t, Some [], false, []);
      f.svar.vstorage <- Extern;
      let fbody = 
        let tdname = getFunNameTypeUnroll ~prefix:typeFunNamePrefixRead td.ttype in
        let tdf = makeVarinfo false tdname (TFun(td.ttype, Some [], false, [])) in
        Formatcil.cStmts
            "%t:thistype __dyc_rsl = %v:tdf ();
             return __dyc_rsl;"
            (fun n t -> makeLocalVar f n t) (* used to add the local decls into the fundec *)
            locUnknown
            [ ("thistype", Ft t);
              ("tdf", Fv tdf) ]
      in
      f.sbody.bstmts <- fbody;
      f
    )
  | TComp (cc, a) -> (
      let f = emptyFunction (getFunNameType ~prefix:typeFunNamePrefixRead t) in
      f.svar.vtype <- TFun(t, Some [], false, []);
      f.svar.vstorage <- Extern;
      let fbody = 
        let ccc = List.filter ( fun fd -> fd.fname != missingFieldName && isSupportedType fd.ftype && isSizeofType fd.ftype ) cc.cfields in (* remove unsupported (bit-)fields *)
        let assignfield fd =
          let fdname = getFunNameTypeUnroll ~prefix:typeFunNamePrefixRead fd.ftype in
          let fdf = makeVarinfo false fdname (TFun(fd.ftype, Some [], false, [])) in
          (fd.fname, Fv fdf)
        in
        let holes = List.map assignfield ccc in
        let patterns = List.fold_left (fun pat h -> pat ^ "__dyc_rsl." ^ (fst h) ^ " = %v:" ^ (fst h) ^ "();\n") "\n" holes in
        Formatcil.cStmts
            ( "%t:__dyc_thistype __dyc_rsl;" ^ patterns ^ "return __dyc_rsl;")
            (fun n t -> makeLocalVar f n t) (* used to add the local decls into the fundec *)
            locUnknown
            ( ("__dyc_thistype", Ft t) :: holes )
      in
      f.sbody.bstmts <- fbody;
      f
    )
  | _ -> E.s (E.bug "typeGenFunRead: not supported type: %a\n" d_type t)

(** construct a __dyc_print_ function for type t *)
(* precondition: t is supported *)
let typeGenFunPrint (t:typ) : fundec =
  if !debug then
    ignore(E.log "Trunkwrapper: construct __dyc_print for type: %a\n" d_type t);
  (* don't do this for print: let t = MDF.typeRemoveAllDeepAttributes tt in *)
  match t with
  | TEnum _ | TInt _ | TFloat _ -> (
      let f = emptyFunction (typeFunNamePrefixPrint ^ "pre_byte") in
      f.svar.vtype <- TFun(voidType, Some [("i", intType, [])], false, []);
      f.svar.vstorage <- Extern;
      f
    )
  | TPtr (tp, a) -> (
      let f = emptyFunction (getFunNameType ~prefix:typeFunNamePrefixPrint t) in
      let farg = makeVarinfo false "__dyc_thistype" (TPtr(typeAddAttributes [Attr("const",[])] tp, a)) in
      setFormals f [farg];
      f.svar.vstorage <- Extern;
      (* print p0 or p1, then recursively print *)
      let fbody =
        let fptr = makeVarinfo false (typeFunNamePrefixPrint ^ "pre_ptr") (TFun(voidType, Some [("p", voidPtrType, [])], false, [])) in
        ( Formatcil.cStmts "fptr(p);"
              (fun n t -> makeVarinfo false  n t) (* no use here *)
              locUnknown [("fptr", Fv fptr); ("p", Fv farg)] )
        @ (
          if isSupportedType tp && isSizeofType tp then (
            let tpname = getFunNameTypeUnroll ~prefix:typeFunNamePrefixPrint tp in
            let tpf = makeVarinfo false tpname (TFun(tp, Some [], false, [])) in
            Formatcil.cStmts "if ( p != 0 ) %v:tpf (*(p));"
                (fun n t -> makeVarinfo false  n t) (* no use here *)
                locUnknown [("tpf", Fv tpf); ("p", Fv farg)]
          ) else (
            farg.vtype <- TPtr(TVoid([Attr("const",[])]),[Attr("const",[])]); (* change the argu to const void const * *)
            setFormals f [farg];
            []
          )
        )
      in
      f.sbody.bstmts <- fbody;
      f
    )
  | TNamed (td, a) -> (
    (* if 't' is supported, 'td.ttype' must also be supported *)
      let f = emptyFunction (getFunNameType ~prefix:typeFunNamePrefixPrint t) in
      let farg = makeVarinfo false "__dyc_thistype" t in
      setFormals f [farg];
      f.svar.vstorage <- Extern;
      let fbody = 
        let tdname = getFunNameTypeUnroll ~prefix:typeFunNamePrefixPrint td.ttype in
        let tdf = makeVarinfo false tdname (TFun(td.ttype, Some [], false, [])) in
        [mkStmtOneInstr (Call(None, Lval(Var(tdf),NoOffset), [Lval(Var(farg),NoOffset)], locUnknown))]
      in
      f.sbody.bstmts <- fbody;
      f
    )
  | TComp (cc, a) -> (
      let f = emptyFunction (getFunNameType ~prefix:typeFunNamePrefixPrint t) in
      let farg = makeVarinfo false "__dyc_thistype" t in
      setFormals f [farg];
      f.svar.vstorage <- Extern;
      let fbody = 
        let ccc = List.filter ( fun fd -> fd.fname != missingFieldName && isSupportedType fd.ftype ) cc.cfields in (* remove unsupported (bit-)fields *)
        let printfield fd =
          let fdname = getFunNameTypeUnroll ~prefix:typeFunNamePrefixPrint fd.ftype in
          let fdf = makeVarinfo false fdname (TFun(voidType, Some [("v", fd.ftype, [])], false, [])) in
          (fd.fname, Fv fdf)
        in
        let holes = List.map printfield ccc in
        let patterns = List.fold_left (fun pat h -> pat ^ "%v:" ^ (fst h) ^ " (%v:__dyc_thistype ." ^ (fst h) ^ ");\n") "\n" holes in
        Formatcil.cStmts patterns
            (fun n t -> makeVarinfo false n t) (* should not be used here *)
            locUnknown
            (("__dyc_thistype", Fv farg)::holes)
      in
      f.sbody.bstmts <- fbody;
      f
    )
  | _ -> E.s (E.bug "typeGenFunPrint: not supported type: %a\n" d_type t)
;;


(** dump all __dyc_ functions: extern decls only, no defs. *)
let dumpTypeFunMapHeaders ?(oc=stdout) fmap =
  let printfheaders tname (t, randomfdec, readfdec, printfdec) =
    match t with
    | TPtr _ | TNamed _ | TComp _ -> (
        dumpGlobal defaultCilPrinter oc (GVarDecl(randomfdec.svar, randomfdec.svar.vdecl));
        dumpGlobal defaultCilPrinter oc (GVarDecl(readfdec.svar, readfdec.svar.vdecl));
        dumpGlobal defaultCilPrinter oc (GVarDecl(printfdec.svar, printfdec.svar.vdecl))
      )
    | _ -> () (* other types are defined in dycfoo.h *)
  in
  Hashtbl.iter printfheaders fmap;
;;

(** dump all __dyc_ functions: the defs, no decls. *)
let dumpTypeFunMapDefs ?(oc=stdout) fmap =
  let printfdefs tname (t, randomfdec, readfdec, printfdec) =
    match t with
    | TPtr _ | TNamed _ | TComp _ -> (
        dumpGlobal defaultCilPrinter oc (GFun(randomfdec, randomfdec.svar.vdecl));
        dumpGlobal defaultCilPrinter oc (GFun(readfdec, readfdec.svar.vdecl));
        dumpGlobal defaultCilPrinter oc (GFun(printfdec, printfdec.svar.vdecl))
      )
    | _ -> ()
  in
  Hashtbl.iter printfdefs fmap
;;

(** dump all __dyc_ functions: with extern decls come first, then the defs. *)
let dumpTypeFunMap ?(oc=stdout) fmap =
  dumpTypeFunMapHeaders ~oc:oc fmap;
  dumpTypeFunMapDefs ~oc:oc fmap
;;

(* a map from typename to a tuple of the type and its fundecs:
  * (the type, __dyc_random_, __dyc_read_, __dyc_print_) *)
let typeFunMap = Hashtbl.create 128;;
let globalTypeCount = ref 0;;

(* A visitor that collects type definitions and construct value functions for
 * them. NOTE: it seems that CIL transforms all local type definitions to globals,
 * so we only need to use "vglob" to collect type definitions.
 * However, to construct _dyc_ functions all types used, we have to use "vtype". *)
class typeGenVisitorClass ?(oc=stdout) () : cilVisitor = object
  inherit nopCilVisitor
  initializer Hashtbl.clear typeFunMap; globalTypeCount := 0

  (** collect explicit type defs/decls *)
  (* NOTE: CIL removes unused globals. *)
  method vglob g = 
    match g with
    | GType _ (* a typedef *) 
    | GCompTag _ (* a definition of a struct or union *)
    | GCompTagDecl _
    | GEnumTag _ (* a definition of an enum *)
    | GEnumTagDecl _
    -> (* collect all these defs/decls *)
      globalTypeCount := !globalTypeCount + 1;
      if !debug then
        ignore(E.log "Trunkwrapper: No. %d global: %a\n" !globalTypeCount d_global g);
      MDF.globalRemoveAllDeepAttributes g; (* NOTE: if this is time-consuming, we could use "sed -i 's/\<const\>//g'" etc. as a post-processing step to remove unwanted attributes *)
      dumpGlobal defaultCilPrinter oc g;
      (* Also, we should construct __dyc_ functions for *some* (definitions) of it;
       * Or, we could wait when it is actually used before the construction (via
       * "vtype"). *)
      DoChildren
    | _ -> DoChildren (* can't SkipChildren, otherwise vtype will be skipped *)
    
  (* construct __dyc_ functions for all used types.
   * Output all the functions after all the global type defs/decls.
   * NOTE: since a type may be used with different attributes, so the final
   * compilation phase may report mismatched attributes (e.g., const, etc.) *)
  method vtype t =
    if (isSupportedType t) && (isSizeofType t) then (
      let tname = getTypeName t in
      if not(Hashtbl.mem typeFunMap tname) then (
        if !debug then
          ignore(E.log "Trunkwrapper: constructing __dyc_ funs for type: %a\n" d_type t);
        let randomfdec = typeGenFunRandom t in
        let readfdec = typeGenFunRead t in
        let printfdec = typeGenFunPrint t in
        Hashtbl.add typeFunMap tname (t, randomfdec, readfdec, printfdec);
      ) else (
        if !debug then
          ignore(E.log "Trunkwrapper: __dyc_ funs already constructed for type: %a\n" d_type t)
      )
    ) else (
      if !debug then
        ignore(E.log "Trunkwrapper: unsupported type: %a\n" d_type t)
    ); DoChildren

end


(* store new in-variables used to replace fun calls; 
 * used mainly for generating unique var name. *)
let funcallVarnames = ref LS.empty;;
let funcallVars = ref UD.VS.empty;;

(* replace a function call with something else *)
let replaceInstrFunCall i fdec fvars : instr list =
  match i with
  | Call (lv, f, args, loc) -> (
      match lv with
      | None -> []
      | Some lv' -> (
          let rtype = 
            try
              let ftype = typeOf f in
              match ftype with
              | TFun(r, _, _, _) -> r
              | TPtr(TFun(r, _, _, _), _) -> r
              | _ -> ignore(E.log "CodeWrapper: can't find a return type for exp1: %a\n---->Use intType instead. May cause compilation failure later.\n" d_exp f); intType
            with e -> 
              ignore(E.log "CodeWrapper: can't find a return type for exp2: %a\n---->Use intType instead. May cause compilation failure later.\n" d_exp f); intType
          in
          (* Opt 1: replace the fun call with a new in-variable *)
          let newvar = makeLocalVar fdec ("__dyc_funcallvar_" ^ (string_of_int (UD.VS.cardinal !fvars + 1))) rtype in
          fvars := UD.VS.add newvar !fvars;
          [Set(lv', Lval(Var(newvar),NoOffset), loc)]
          (* Opt 2: replace the fun call with a (dynamic) call to 
           * __dyc_random_ function *
           try
             let rpf = getTypeFun rtype in
             [Call(lv, Lval(Var(rpf.svar),NoOffset), [], loc)]
           with Not_found -> (
             let rpf = genFun_ReturnType rtype in (* also added into typeFunMap *)
             [Call(lv, Lval(Var(rpf.svar),NoOffset), [], loc)]
           )
          *)
        )
    )
  | _ -> E.s (E.bug "can't replace non-function call instructions.\n")

let dummyLabelName = "__dyc_dummy_label";;
(* use the dummpy stmt as the exit point *)
let dummyLabeledStmt = 
  let es = mkEmptyStmt () in
  es.labels <- [Label(dummyLabelName, locUnknown, false)];
  es

let replaceLabelStmt s ldef =
  match s.skind with
      Goto (sref, loc) -> (
        (* NOTE: if sref is a block, its labels may not be include in "labelDef",
         * but it seems that CIL always moves labels for Block to the first
         * stmt in the Block, so we don't have to worry about Blocks here *)
        if !sref == dummyLabeledStmt then false
        else (
          let label = MDF.getFirstLabel !sref in
          match label with
          | Some Label(str, loc', b) ->
              if not(Hashtbl.mem ldef str) then (
                s.skind <- Goto (ref dummyLabeledStmt, loc);
                true
              ) else false
          | _ -> E.s (E.log "CodeWrapper: no label for Goto: %a\n" d_stmt s)
        )
      )
    | _ -> false

let replaceReturnStmt s =
  match s.skind with
  | Return(eo, loc) -> s.skind <- Goto(ref dummyLabeledStmt, loc)
  | _ -> ()


(** The visitor does the following:
  * Replaces all function calls with a new in-variable;
  * Remove asm instr;
  * Replace "sizeof(exp)" with "sizeof(typ)" since the "exp"
    * may use variables that have not been counted as in-vars;
    * Usedef.ignoreSizeof control whether to count vars in sizeof(exp);
  * Replace return stmts with gotos; *)
(* NOTE: replacing fun calls etc. may change the number of stmts and/or its
 * locations, causing a bit different (may be erroneous/inconsistent) chopping
 * results; so, it's better to be done before any chopping *)
(* TODO: may add an argument to represent what the fun call is replaced with *)
class funReplaceVisitorClass fdec fvars : cilVisitor = object
  inherit nopCilVisitor
  initializer fvars := UD.VS.empty

  method vinst (i : instr) : instr list visitAction =
    match i with
        Call (lv, f, args, loc) ->
          ChangeTo (replaceInstrFunCall i fdec fvars)
      | Asm _ -> ChangeTo []
      | _ -> DoChildren

  method vexpr e =
    match e with
    | SizeOfE(se) -> let et = 
        try
          typeOf se
        with _ -> charType
      in
      ChangeTo (SizeOf et)
    | _ -> DoChildren

  method vstmt s =
    replaceReturnStmt s;
    DoChildren

end


(** store use/def labels *)
let labelUsed = Hashtbl.create 32;;
let labelDef = Hashtbl.create 32;;

(** The visitor does the following:
  * Collect use/def info about labels; *)
class stmtInfoVisitorClass luse ldef : cilVisitor = object
  inherit nopCilVisitor
  initializer Hashtbl.clear luse; Hashtbl.clear ldef

  method vstmt (s : stmt) : stmt visitAction =
    MDF.computeLabelUseDefStmt s luse ldef; (* shallow is enough for DoChildren *)
    DoChildren

end

let changedStmtSkind = Hashtbl.create 32;;
let restoreStmtSkinds smap =
  Hashtbl.iter (fun s k -> !s.skind <- k) smap;
  Hashtbl.clear changedStmtSkind
;;

(** The visitor does the following:
  * Replaces undefined labels; *)
class stmtReplaceVisitorClass : cilVisitor = object
  inherit nopCilVisitor
  initializer Hashtbl.clear changedStmtSkind

  method vstmt (s: stmt) : stmt visitAction =
    if !debug then
      ignore(E.log "Trunkwrapper: before stmtReplaceVisitorClass: %a\n-->loc: %a\n" d_stmt s d_loc (get_stmtLoc s.skind));
    let oldskind = s.skind in
    if replaceLabelStmt s labelDef then
      (* record the old skind for restoring later *)
      Hashtbl.add changedStmtSkind (ref s) oldskind;
    if !debug then
      ignore(E.log "Trunkwrapper: after stmtReplaceVisitorClass: %a\n-->loc: %a\n" d_stmt s d_loc (get_stmtLoc s.skind));
    DoChildren

end

(** Make a code trunk compilable.
 * Must use with --domakeCFG *)
let trunkwrapper fdec slist =
  (* create a wrapper function named "__dyc_foo" *)
  let wrapper = emptyFunction "__dyc_foo" in

  (* collect label info; slist should remain unchanged (==slist1) *)
  let stmtInfoVisitor = new stmtInfoVisitorClass labelUsed labelDef in
  let slist1 = List.map (fun s -> visitCilStmt stmtInfoVisitor s) slist in
  (* replace undefined labels
   * Update: shouldn't change the stmts anymore since that may change the semantic
   * of the next code trunk; Try to output differently *)
  let stmtReplaceVisitor = new stmtReplaceVisitorClass in
  let slist2 = List.map (fun s -> visitCilStmt stmtReplaceVisitor s) slist1 in 

  (* add local declarations for the in-variables:
   * NOTE: it is not enough to just declare in-vars or all vars used in "fdec";
   * we need to decl all vars from the original fundec AND all global vars *)
  let vuses, vdefs = MDF.computeDeepVarUseDefStmtList slist2 in
  let localvars = UD.VS.union vuses vdefs in
  (* special handing for fun types: 
    * we can't handle fun type during random value generation, 
    * but pointer type is ok, although gcc may report incompatible pointer assignments;
    * and all fun calls have been replaced before hand. *)
  let addlocalvar v = 
    if isFunctionType v.vtype then 
      ignore(makeLocalVar wrapper v.vname (TPtr(v.vtype,[])))
    else
      ignore(makeLocalVar wrapper v.vname v.vtype)
    (* TODO: array types must have a constant size to be compilable by gcc.
     *   how come the variables of such types? "extern" variables.
     * Need to seriously consider how to handle array types *)
  in
  UD.VS.iter addlocalvar localvars;
  (* construct "__dyc_read_" fun calls for all in-variables,
   * and initial values (0) for all other variables *)
  let invars = LocalLiveness.computeLocalLiveness slist2 in
  let uninitvars = UD.VS.diff localvars invars in
  wrapper.sbody.bstmts <- [ (mkStmt (Instr (UD.VS.fold (fun v il -> il @ (typeGenAssignmentRead v v.vtype typeFunMap)) invars [])));
                            (mkStmt (Instr (UD.VS.fold (fun v il -> il @ (varGenInit v)) uninitvars []))) ] @ slist2 @ [dummyLabeledStmt];
  (* construct output stmts for the reaching defs *)
  let rds = LocalRDs.computeLocalRDs slist2 in
  wrapper.sbody.bstmts <- wrapper.sbody.bstmts @ [mkStmt (Instr (UD.VS.fold (fun v il -> il @ (typeGenCallPrint v v.vtype typeFunMap)) rds []))];

  (* construct a main function for random value generation *)
  let randomGen = emptyFunction "main" in
  randomGen.svar.vtype <- TFun(intType, Some [], false, []);
  UD.VS.iter (fun v -> ignore(makeLocalVar randomGen v.vname v.vtype)) invars;
  (* need to call __dyc_initrandomseed *)
  let initseed = makeVarinfo false "__dyc_initrandomseed" (TFun(voidType, Some [], false, [])) in
  randomGen.sbody.bstmts <- (mkStmtOneInstr (Call(None, Lval(Var(initseed),NoOffset), [], locUnknown))) :: 
    [ (mkStmt (Instr (UD.VS.fold (fun v il -> il @ (typeGenAssignmentRandom v v.vtype typeFunMap)) invars [])));
      (mkStmt (Instr (UD.VS.fold (fun v il -> il @ (typeGenCallPrint v v.vtype typeFunMap)) invars [])));
      (* add a "return 0;" so that we know if .gen.exe succeeds: *)
      (mkStmt (Return (Some(Const(CInt64(Int64.zero, IInt, Some "0"))), locUnknown)))
    ];

  (* return the function tuple *)
  wrapper, randomGen, invars, rds
;;

let dumpTrunkWrapper ?(oc=stdout) wrapper =
  (* output a fun def *)
  dumpGlobal defaultCilPrinter oc (GFun(wrapper, locUnknown))
;;


(** output the stmts *)
let dumpStmtList oc slist =
  List.map (fun s -> fprintf oc "%a\n" d_stmt s) slist
;;




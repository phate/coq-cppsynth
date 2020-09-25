Require Import
  Coq.Arith.Compare_dec
  Coq.Numbers.DecimalString
  Coq.Numbers.DecimalNat
  Coq.Strings.String
  Coq.Lists.List
  CPPSynth.SExpression
  CPPSynth.StringUtil
  CPPSynth.Exception
  CPPSynth.CoqAst
  CPPSynth.CPPAst
  CPPSynth.CPPAstBase
  CPPSynth.CPPAstSerialize
  CPPSynth.CPPAstVisit
  CPPSynth.ListClass
  CPPSynth.Monad.

(* concatenate two scopes (i.e. nest one in another) *)
Fixpoint scope_concat (s1 s2 : scope_t) : scope_t :=
  match s1 with
    | scope_none => s2
    | scope_id id s => scope_id id (scope_concat s s2)
    | scope_template tplid s => scope_template tplid (scope_concat s s2)
  end.

(* nest an id expr in one scope; if it is scoped already, then nest the given scopes deeper *)
Definition idexpr_scopify (scope : scope_t) (e : idexpr_t) :=
  match e with
    | idexpr_id s id => idexpr_id (scope_concat scope s) id
    | idexpr_destructor s id => idexpr_destructor (scope_concat scope s) id
    | idexpr_template s tplid => idexpr_template (scope_concat scope s) tplid
    | idexpr_operator s op => idexpr_operator (scope_concat scope s) op
  end.

Fixpoint decl_scopify (scope : scope_t) (decl : decl_t) :=
  match decl with
    | decl_simple ds type id attr =>
      decl_simple ds type (idexpr_scopify scope id) attr
    | decl_initdef ds type id attr value =>
      decl_initdef ds type (idexpr_scopify scope id) attr value
    | decl_fundef ds funtype id attr body =>
      decl_fundef ds funtype (idexpr_scopify scope id) attr body
    | decl_consdesdecl ds id args qual attr =>
      decl_consdesdecl ds (idexpr_scopify scope id) args qual attr
    | decl_consdesdef ds id args qual attr body =>
      decl_consdesdef ds (idexpr_scopify scope id) args qual attr body
    | decl_class_fwd id =>
      decl_class_fwd (idexpr_scopify scope id)
    | decl_class id is_final inherits clsdecls =>
      decl_class (idexpr_scopify scope id) is_final inherits clsdecls
    | decl_templated formargs decl =>
      decl_templated formargs (decl_scopify scope decl)
  end.

Definition decl_maybe_template (tpl : tplformargs_t) (d : decl_t) : decl_t :=
  match tpl with
    | tplformargs_nil => d
    | _ => decl_templated tpl d
  end.

Definition idexpr_maybe_template (tplargs : tplargs_t) (id : idexpr_t) : idexpr_t :=
  match id with
    | idexpr_id scope id =>
      idexpr_template scope (templateid_make id tplargs)
    | idexpr_destructor scope _ =>
      id (* illegal *)
    | idexpr_template scope tplid =>
      let (id, tplargs2) := tplid in
      idexpr_template scope (templateid_make id tplargs)
    | idexpr_operator scope op =>
      id
  end.

(* If a decl_t is actually a definition of something, turn this into
a (forward) declaration of the object. This can be either variables,
functions or classes. *)
Fixpoint decl_remove_def (decl : decl_t) : decl_t :=
  match decl with
    | decl_initdef ds type id attrs value =>
      decl_simple ds type id attrs
    | decl_fundef ds funtype id attrs body =>
      if funbody_is_stmts body then
        decl_simple ds (typeexpr_function funtype) id attrs
      else
        decl
    | decl_consdesdef ds id args qual attrs body =>
      if funbody_is_stmts body then
        decl_consdesdecl ds id args qual attrs
      else
        decl
    | decl_class id is_final inherits decls =>
      decl_class_fwd id
    | decl_templated args decl =>
      decl_templated args (decl_remove_def decl)
    | _ => decl
  end.

Definition decls_remove_defs (decls : decls_t) : decls_t :=
  from_list (map decl_remove_def (to_list decls)).

Definition clsdecl_remove_defs (cdecl : clsdecl_t) : clsdecl_t :=
    let (vis, decls) := cdecl in
    let decls := decls_remove_defs decls in
    clsdecl_group vis decls.

Definition clsdecls_remove_defs (cdecls : clsdecls_t) : clsdecls_t :=
  from_list (map clsdecl_remove_defs (to_list cdecls)).

Fixpoint cls_remove_defs (decl : decl_t) : decl_t :=
  match decl with
    | decl_class id is_final inherits clsdecls =>
      decl_class id is_final inherits (clsdecls_remove_defs clsdecls)
    | decl_templated args decl =>
      decl_templated args (cls_remove_defs decl)
    | _ => decl
  end.

(* Clear decl specifiers that may be applied to a class member but
that have a different meaning / are illegal when un-inlining the definition
of the class member *)
Definition declspec_extract_member (ds : declspec.t) : declspec.t :=
  let ds := declspec.clear_static ds in
  let ds := declspec.clear_mutable ds in
  let ds := declspec.clear_virtual ds in
  let ds := declspec.clear_explicit ds in
  let ds := declspec.clear_friend ds in
  ds.

Definition attrspec_extract_member (attr : attrspec.t) : attrspec.t :=
  attrspec.none.

Fixpoint decl_is_member_def (decl : decl_t) : bool :=
  match decl with
    | decl_simple ds type id attr =>
      false
    | decl_initdef ds type id attr value =>
      declspec.is_static ds
    | decl_fundef ds funtype id attr body =>
      funbody_is_stmts body
    | decl_consdesdecl ds id args qual attr =>
      false
    | decl_consdesdef ds id args qual attr body =>
      funbody_is_stmts body
    | decl_class_fwd id =>
      false
    | decl_class id is_final inherits clsdecls =>
      true
    | decl_templated formargs decl =>
      decl_is_member_def decl
  end.

Fixpoint decl_extract_member_def (decl : decl_t) : decl_t :=
  match decl with
    | decl_initdef ds type id attr value =>
      decl_initdef (declspec_extract_member ds) type id (attrspec_extract_member attr) value
    | decl_fundef ds funtype id attr body =>
      decl_fundef (declspec_extract_member ds) funtype id (attrspec_extract_member attr) body
    | decl_consdesdef ds id args qual attr body =>
      decl_consdesdef (declspec_extract_member ds) id args qual (attrspec_extract_member attr) body
    | decl_class id is_final inherits clsdecls =>
      decl_class id is_final inherits clsdecls
    | decl_templated formargs decl =>
      decl_templated formargs (decl_extract_member_def decl)
    | _ => decl
  end.

Fixpoint decls_extract_member_defs (decls : decls_t) : list decl_t :=
  match decls with
    | decls_nil => nil
    | decls_cons decl decls =>
      (if decl_is_member_def decl then
        (decl_extract_member_def decl) :: nil
      else
        nil) ++ decls_extract_member_defs decls
  end.

Definition clsdecl_extract_member_defs (clsdecl : clsdecl_t) : list decl_t :=
  let (visibility, decls) := clsdecl in
  decls_extract_member_defs decls.

Fixpoint clsdecls_extract_member_defs (clsdecls : clsdecls_t) : list decl_t :=
  match clsdecls with
    | clsdecls_nil => nil
    | clsdecls_cons clsdecl clsdecls =>
      (clsdecl_extract_member_defs clsdecl) ++ (clsdecls_extract_member_defs clsdecls)
  end.

(* convenient builders *)

(* id expressions *)
(* make simple unqualified name id *)
Definition mk_sid (s : string) : idexpr_t :=
  idexpr_id scope_none s.

Definition mk_tplid1 (s : string) (type : typeexpr_t) : idexpr_t :=
  idexpr_template scope_none (templateid_make s (from_list (tplarg_type type :: nil))).

(* qualify id expr with "std::" scope *)
Definition mk_std (id : idexpr_t) := idexpr_scopify (scope_id "std" scope_none) id.

(* type expressions *)

Definition mk_named_type (s : string) : typeexpr_t :=
  typeexpr_id (mk_sid s).
Definition mk_type_auto := typeexpr_primitive primtype.auto.
Definition mk_fn_type (result_type : typeexpr_t) (args : list (option string * typeexpr_t)) : typeexpr_t :=
  let fnargs := from_list (map (fun (a : option string * typeexpr_t) => let (name, type) := a in
    match name with
      | None => funarg_anon type
      | Some n => funarg_named type n
    end) args) in
  typeexpr_function (funtypeexpr_make result_type fnargs fnqual.none false).

Definition mk_shared_ptr_of (type : typeexpr_t) : typeexpr_t :=
  typeexpr_id (mk_std (mk_tplid1 "shared_ptr" type)).
Definition mk_constref_of (type : typeexpr_t) : typeexpr_t :=
  typeexpr_reference (typeexpr_const type).
Definition mk_constptr_of (type : typeexpr_t) : typeexpr_t :=
  typeexpr_pointer (typeexpr_const type).

Definition mk_declval_of (type : typeexpr_t) :=
  expr_call
    (expr_id (mk_std (mk_tplid1 "declval" type)))
    (from_list nil).

(* expressions *)

Definition mk_move_of (expr : expr_t) : expr_t :=
  expr_call
    (expr_id (mk_std (mk_sid "move")))
    (from_list (expr :: nil)).
Definition mk_make_shared_of (type : typeexpr_t) (args : list expr_t) : expr_t :=
  expr_call
    (expr_id (mk_std (mk_tplid1 "make_shared" type)))
    (from_list args).
Definition mk_throw_logic_error (msg : string) : expr_t :=
  let std_logic_error := expr_id (mk_std (mk_sid "logic_error")) in
  let arg := expr_literal (literal.str msg) in
  expr_throw (expr_call std_logic_error (from_list (arg :: nil))).

(* declarations *)

Definition mk_deleted_constructor (name : string) (args : list funarg_t) : decl_t :=
  decl_consdesdef
    declspec.none
    (idexpr_id scope_none name)
    (from_list args)
    fnqual.none
    attrspec.none
    funbody_delete.

Definition mk_virtual_destructor_decldef (name : string) : decl_t :=
  let ds := declspec.none in
  let ds := declspec.set_virtual ds in
  decl_consdesdef
    ds
    (idexpr_destructor scope_none name)
    (from_list nil)
    fnqual.none
    attrspec.none
    (funbody_stmts cinits_nil (from_list nil)).

(* Class operations *)

(* helper to replace all occurrences of one type expr with a different type expr *)
Module type_replace.
  Inductive t :=
    | make :
      forall (orig : typeexpr_t),
      forall (repl : typeexpr_t),
      t.
  Definition typeexpr_post (typeexpr : typeexpr_t) (this : t) : typeexpr_t * t :=
    let (orig, repl) := this in
    if typeexpr_eq_dec typeexpr orig then (repl, this) else (typeexpr, this).
  Definition vmt :=
    (visitor.override_typeexpr_post _ typeexpr_post (visitor.vmt t)).
End type_replace.

(* For a (possibly templated) class of given name, extract stand-alone member definitions *)
Definition class_extract_member_defs
    (tplargs : tplformargs_t) (clsname : string) (decls : clsdecls_t)
    : list decl_t :=
  let scope := scope_id clsname scope_none in
  let clsid := idexpr_maybe_template (tplformargs_to_tplargs tplargs) (mk_sid clsname) in
  let edecls := (clsdecls_extract_member_defs decls) in
  let replace := type_replace.make (typeexpr_id (mk_sid clsname)) (typeexpr_id clsid) in
  let edecls := map (
    fun decl => snd (decl_visit _ type_replace.vmt replace decl)) edecls in
  map (decl_maybe_template tplargs) (map (decl_scopify scope) edecls).

(* For a (possibly templated) class of given name, strip member definitions so they can be moved out. *)
Definition class_strip_member_defs
    (tplargs : tplformargs_t) (clsname : string) (decls : clsdecls_t)
    : decl_t :=
  (decl_maybe_template tplargs (decl_class (mk_sid clsname) true clsinherits_nil (clsdecls_remove_defs decls))).


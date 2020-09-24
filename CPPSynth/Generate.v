Require Import
  Coq.Arith.Compare_dec
  Coq.Numbers.DecimalString
  Coq.Numbers.DecimalNat
  Coq.Strings.String
  Coq.Lists.List
  CPPSynth.SExpression
  CPPSynth.StringUtil
  CPPSynth.Exception
  CPPSynth.Gallina
  CPPSynth.CoqAstBase
  CPPSynth.CPPAst
  CPPSynth.CPPAstBase
  CPPSynth.CPPAstHelper
  CPPSynth.CPPAstSerialize
  CPPSynth.CPPAstVisit
  CPPSynth.ListClass
  CPPSynth.XFEnv
  CPPSynth.XFInductive.

Definition product_unfold (e : Gallina.expr_t) : list (option string * Gallina.expr_t) * Gallina.expr_t :=
  let head := nil in
  (fix loop (e : Gallina.expr_t) (head : list (option string * Gallina.expr_t))
      : list (option string * Gallina.expr_t) * Gallina.expr_t :=
    match e with
      | expr_prod arg body =>
        let (argname, argtype) := match arg with
          | arg_named name type => (Some (name.to_string name), type)
          | arg_anonymous type => (None, type)
        end in
        let head := head ++ (argname, argtype) :: nil in
        loop body head
      | _ =>
        (head, e)
    end) e head.

(* make choice of prefix name configurable *)
Definition product_named_unfold (e : Gallina.expr_t) : list (string * Gallina.expr_t) * Gallina.expr_t :=
  let head := nil in
  let index := 0 in
  (fix loop (e : Gallina.expr_t) (index : nat) (head : list (string * Gallina.expr_t))
      : list (string * Gallina.expr_t) * Gallina.expr_t :=
    match e with
      | expr_prod arg body =>
        let (name, argtype) := match arg with
          | arg_named name type => (name.to_string name, type)
          | arg_anonymous type => (("x" ++ (string_of_nat index))%string, type)
        end in
        let head := head ++ (name, argtype) :: nil in
        loop body (S index) head
      | _ =>
        (head, e)
    end) e index head.

Fixpoint collect_tplformargs_from_product
    (e : Gallina.expr_t) : tplformargs_t :=
  match e with
    | expr_prod arg body =>
      let (name, argtype) := match arg with
        | arg_named name type => (name.to_string name, type)
        | arg_anonymous type => ("T"%string, type)
      end in
      tplformargs_cons (tplformarg_typename name) (collect_tplformargs_from_product body)
    | _ => tplformargs_nil
  end.

(*** INDUCTIVE XFORM CODE ***)

Definition make_repr_base_class (name : string) :=
  let repr_cls_type := typeexpr_id (idexpr_id scope_none name) in
  let eq_operator :=
    decl_fundef
      (declspec.set_virtual declspec.none)
      (funtypeexpr_make
        (typeexpr_primitive primtype.bool)
        (from_list (funarg_named (mk_constref_of repr_cls_type) "other" :: nil))
        (fnqual.set_noexcept (fnqual.set_const fnqual.none))
        false)
      (idexpr_operator scope_none (genop.binary binop.eq))
      attrspec.none
      funbody_abstract in
  decl_class
    (idexpr_id scope_none name)
    false
    (from_list nil)
    (clsdecls_cons
      (clsdecl_group
        visibility_spec.vis_public
        (from_list
          (mk_virtual_destructor_decldef name ::
          eq_operator ::
          nil)))
      (from_list nil)).

Definition constructor_reprclass_name (cons : inductive_constructor.t) : string :=
  ((inductive_constructor.name cons) ++ "_repr").

Definition constructor_reprclass_members (cons : inductive_constructor.t)
    : list (string * Gallina.expr_t) :=
  fst (product_named_unfold (inductive_constructor.type cons)).

Definition make_constructor_repr_class
    (cons : inductive_constructor.t)
    (environ : env.t) :=
  let name := constructor_reprclass_name cons in
  let members := constructor_reprclass_members cons in

  let repr_cls_id := idexpr_id scope_none name in
  let repr_cls_type := typeexpr_id repr_cls_id in
  let repr_constptr := typeexpr_pointer (typeexpr_const repr_cls_type) in

  let base_cls_id := (idexpr_id scope_none "repr_base") in
  let base_cls_type := typeexpr_id base_cls_id in

  let member_decls := map
    (fun (mem : string * Gallina.expr_t) =>
      let (name, e) := mem in
      let type := env.to_type e environ in
      decl_simple declspec.none type (idexpr_id scope_none name) attrspec.none) members in
  let init_formal_args := map
    (fun (mem : string * Gallina.expr_t) =>
      let (name, e) := mem in
      let type := env.to_type e environ in
      funarg_named type (name ++ "_init")) members in
  let initializers := map
    (fun (mem : string * Gallina.expr_t) =>
      let (name, e) := mem in
      cinit_make
        (idexpr_id scope_none name)
        (mk_move_of (expr_id (idexpr_id scope_none (name ++ "_init"))))) members in
  let cls_constructor := decl_consdesdef
    declspec.none
    (idexpr_id scope_none name)
    (from_list init_formal_args)
    fnqual.none
    attrspec.none
    (funbody_stmts
      (from_list initializers)
      stmts_nil) in
  let overridden_eq_operator :=
    decl_fundef
      declspec.none
      (funtypeexpr_make
        (typeexpr_primitive primtype.bool)
        (from_list (funarg_named (mk_constref_of base_cls_type) "other" :: nil))
        (fnqual.set_noexcept (fnqual.set_const fnqual.none))
        false)
      (idexpr_operator scope_none (genop.binary binop.eq))
      (attrspec.set_override attrspec.none)
      (funbody_stmts (from_list nil)
        (from_list
          (stmt_ifelse
            (condition_decl
              declspec.none (typeexpr_primitive primtype.auto)
              (idexpr_id scope_none "spec")
              attrspec.none
              (expr_dynamic_cast repr_constptr (expr_unop unop.address_of (expr_id (idexpr_id scope_none "other")))))
            (stmt_block (from_list
              (stmt_return (expr_binop binop.eq
                (expr_unop unop.deref (expr_id (idexpr_id scope_none "this")))
                (expr_unop unop.deref (expr_id (idexpr_id scope_none "spec"))))
              :: nil)))
            (stmt_block (from_list
              (stmt_return (expr_id (idexpr_id scope_none "false")) :: nil))) ::
          nil))) in
  let member_compare := map
    (fun (mem : string * Gallina.expr_t) =>
      let (name, e) := mem in
      let id := idexpr_id scope_none name in
      expr_binop binop.eq (expr_id id) (expr_memdot (expr_id (idexpr_id scope_none "other")) id))
      members in
  let all_member_eq := fold_right
    (fun (x y : expr_t) => expr_binop binop.logic_and x y)
    (expr_id (idexpr_id scope_none "true")) member_compare in
  let specialized_eq_operator :=
    decl_fundef
      declspec.none
      (funtypeexpr_make
        (typeexpr_primitive primtype.bool)
        (from_list (funarg_named (mk_constref_of repr_cls_type) "other" :: nil))
        (fnqual.set_noexcept (fnqual.set_const fnqual.none))
        false)
      (idexpr_operator scope_none (genop.binary binop.eq))
      attrspec.none
      (funbody_stmts cinits_nil
        (from_list (stmt_return all_member_eq :: nil))) in
  decl_class
    repr_cls_id
    true
    (clsinherits_cons (clsinherit_single false visibility_spec.vis_public base_cls_id) clsinherits_nil)
    (from_list
      ((clsdecl_group
        visibility_spec.vis_public
        (from_list (cls_constructor :: specialized_eq_operator :: overridden_eq_operator :: member_decls))) :: nil)).

(*
  Roughly builds the following:

  if (const as_type* temp_name = dynamic_cast<const as_type*>(base_expr)) {
    call_expr(temp_name->member1, temp_name->member2, ..., temp_name->membern);
  }

  Returns the "condition" and "body" part of the if statement as a pair.

  Can form the above statement as:
    stmt_if condition body
*)
Definition make_single_match_test
    (base_expr : expr_t) (as_type : typeexpr_t) (temp_name : idexpr_t)
    (call_expr : expr_t) (members : list string)
    : condition_t * stmt_t :=
  let target_type := mk_constptr_of as_type in
  let temp_expr := expr_id temp_name in
  let callargs :=
    map
      (fun member =>
        expr_memarrow (expr_id temp_name) (idexpr_id scope_none member))
      members in
  let cond := (condition_decl
      declspec.none mk_type_auto temp_name attrspec.none
      (expr_dynamic_cast target_type base_expr)) in
  let body :=
    stmt_block (
      from_list
        ((stmt_return (expr_call call_expr (from_list callargs))) :: nil)) in
  (cond, body).

(*
  Produce a statement that checks 'base_expr' with match for any of the
  constructor repr classes (via dynamic_cast) and dispatches to the
  corresponding call expression.
*)
Fixpoint make_match_constructors
    (base_expr : expr_t) (branches : list (inductive_constructor.t * expr_t))
    : stmt_t :=
  match branches with
    | nil => stmt_block (from_list (stmt_expr (mk_throw_logic_error "impossible case" ) :: nil))
    | branch :: branches =>
      let sub := make_match_constructors base_expr branches in
      let (cons, call_expr) := branch in
      let cons_name := constructor_reprclass_name cons in
      let cons_members := constructor_reprclass_members cons in
      let member_names := map fst cons_members in
      let as_type := typeexpr_id (idexpr_id scope_none cons_name) in
      let temp_name := idexpr_id scope_none "tmp" in
      let (cond, body) := make_single_match_test base_expr as_type temp_name call_expr member_names in
      stmt_ifelse cond body sub
  end.

Definition make_name_type_pairs (args : list (option string * Gallina.expr_t)) (environ : env.t)
    : list (string * typeexpr_t) :=
  map
    (fun (ca : option string * Gallina.expr_t) =>
      let (n, e) := ca in
      let type := env.to_type e environ in
      let name := match n with
        | Some n => n
        | None => "_"%string
      end in
      (name, type))
    args.

Definition make_match_fntype
    (constructors : list inductive_constructor.t)
    (environ : env.t) : (list tplformarg_t) * funtypeexpr_t :=
  (* special case for empty inductive type (not useful, but...) *)
  match constructors with
    | first_constructor :: _ =>
      (* collect names of all constructors *)
      let consnames := map inductive_constructor.name constructors in
      (* template argument list for match handler functionals *)
      let tplargs := map (fun name => tplformarg_typename ("F" ++ name)%string) consnames in
      (* the formal arguments of our "match" function *)
      let formal_args :=
        from_list (
          map
            (fun name =>
             funarg_named
              (mk_constref_of (mk_named_type ("F" ++ name)))
              ("f" ++ name))
            consnames) in
      (* arguments to first constructor *)
      let pre_callargs := fst (product_unfold (inductive_constructor.type first_constructor)) in
      (* turn them into "declval" constructs *)
      let callargs := map (fun (ca : option string * Gallina.expr_t) =>
        let (_, e) := ca in
        let type := env.to_type e environ in
        mk_declval_of (mk_constref_of type)) pre_callargs in
      let ret_type :=
        typeexpr_decltype (expr_call (expr_id (idexpr_id scope_none ("F" ++ (inductive_constructor.name first_constructor)))) (from_list callargs)) in
      (tplargs, funtypeexpr_make ret_type formal_args fnqual.none true)
    | nil =>
      (nil, funtypeexpr_make (typeexpr_primitive primtype.void) funargs_nil fnqual.none false)
  end.

Definition make_inductive_cons_factory_fn
    (ind_clsname : string) (consname : string) (cons_args : list (string * Gallina.expr_t))
    : decl_t :=
  let environ := env.empty in
  let formal_args :=
    map
      (fun (c : string * Gallina.expr_t) =>
        let (name, e) := c in
        funarg_named (env.to_type e environ) name)
      cons_args in
  let args :=
    map
      (fun (c : string * Gallina.expr_t) =>
        let (name, e) := c in
        mk_move_of (expr_id (idexpr_id scope_none name)))
      cons_args in
  let fntype :=
    funtypeexpr_make
      (mk_named_type ind_clsname)
      (from_list formal_args)
      fnqual.none
      false in
  let repr_instance :=
    mk_make_shared_of
      (mk_named_type (consname ++ "_repr"))
      args in
  let cls_instance :=
    expr_call (expr_id (idexpr_id scope_none ind_clsname)) (from_list (repr_instance :: nil)) in
  decl_fundef
    (declspec.set_static declspec.none)
    fntype
    (idexpr_id scope_none consname)
    attrspec.none
    (funbody_stmts
      cinits_nil
      (from_list (stmt_return cls_instance :: nil))).

Definition oind_convert (oi : one_inductive.t)
    : oind_cpp_impl.t :=
  let environ := env.empty in
  let (name, sig, constructors) := oi in

  (* names of the inductive constructors *)
  let consnames := map (fun (c : inductive_constructor.t) => let (name, _) := c in name) constructors in

  (* template arguments, in case this is a parameterized inductive type *)
  let tpl := collect_tplformargs_from_product sig in

  (* the C++ class name... *)
  let cls_name := name in
  let cls_id := idexpr_id scope_none cls_name in
  let cls_type := typeexpr_id cls_id in

  (* the repr_ member id *)
  let id_repr_m := (idexpr_id scope_none "repr_") in

  (* internal base representation class, abstract *)
  let repr_base_class_def := make_repr_base_class "repr_base" in
  let repr_ptr_type := (mk_shared_ptr_of (mk_named_type "repr_base")) in

  (* internal representation classes, to represent the different
  inductive constructors *)
  let repr_classes := map (fun c => make_constructor_repr_class c environ) constructors in

  (* match function to discriminate between the different constructors *)
  let (match_fn_tplargs, match_fn_type) := make_match_fntype constructors environ in
  let cons_call := map
    (fun (cons : inductive_constructor.t) =>
      (cons, expr_id (idexpr_id scope_none ("f" ++ inductive_constructor.name cons)))) constructors in
  let match_fn_decl :=
    decl_fundef
      declspec.none
      match_fn_type
      (idexpr_id scope_none "match")
      attrspec.none
      (funbody_stmts
        (from_list nil)
        (from_list
          (make_match_constructors (expr_id id_repr_m) cons_call :: nil))) in
  let match_fn_tpldecl := decl_maybe_template (from_list match_fn_tplargs) match_fn_decl in

  (* The static factory functions to create instances of this object
  per each inductive constructor. *)
  let factory_fns :=
    map
      (fun (cons : inductive_constructor.t) =>
        let (consname, _) := cons in
        let members := constructor_reprclass_members cons in
        make_inductive_cons_factory_fn cls_name consname members
      )
      constructors in

  (* C++ class constructors *)
  let primary_constructor :=
    decl_consdesdef
      (declspec.set_explicit declspec.none)
      cls_id
      (from_list ((funarg_named repr_ptr_type "repr") :: nil))
      (fnqual.set_noexcept fnqual.none)
      attrspec.none
      (funbody_stmts
        (cinits_cons (cinit_make (idexpr_id scope_none "repr_") (mk_move_of (expr_id (idexpr_id scope_none "repr")))) cinits_nil)
        (from_list nil)) in
  let copy_constructor :=
    decl_consdesdef
      declspec.none
      cls_id
      (from_list (((funarg_named (mk_constref_of cls_type) "other")) :: nil))
      (fnqual.set_noexcept fnqual.none)
      attrspec.none
      funbody_default in
  let move_constructor :=
    decl_consdesdef
      declspec.none
      cls_id
      (from_list (((funarg_named (typeexpr_rvaluereference cls_type) "other")) :: nil))
      (fnqual.set_noexcept fnqual.none)
      attrspec.none
      funbody_default in
  let default_constructor := mk_deleted_constructor cls_name nil in

  let assign_operator :=
    decl_fundef
      declspec.none
      (funtypeexpr_make
        (typeexpr_reference cls_type)
        (from_list (funarg_named (mk_constref_of cls_type) "other" :: nil))
        (fnqual.set_noexcept fnqual.none)
        false)
      (idexpr_operator scope_none (genop.binary binop.assign))
      attrspec.none
      (funbody_stmts
        (from_list nil)
        (from_list (
          stmt_expr (expr_binop binop.assign (expr_id id_repr_m)
            (expr_memarrow (expr_id (idexpr_id scope_none "other")) id_repr_m)) ::
          stmt_return (expr_unop unop.deref (expr_id (idexpr_id scope_none "this"))) ::
          nil))) in

  let assign_move_operator :=
    decl_fundef
      declspec.none
      (funtypeexpr_make
        (typeexpr_reference cls_type)
        (from_list (funarg_named (typeexpr_rvaluereference cls_type) "other" :: nil))
        (fnqual.set_noexcept fnqual.none)
        false)
      (idexpr_operator scope_none (genop.binary binop.assign))
      attrspec.none
      (funbody_stmts
        (from_list nil)
        (from_list (
          stmt_expr (expr_binop binop.assign (expr_id id_repr_m)
            (mk_move_of (expr_memarrow (expr_id (idexpr_id scope_none "other")) id_repr_m))) ::
          stmt_return (expr_unop unop.deref (expr_id (idexpr_id scope_none "this"))) ::
          nil))) in

  let eq_operator :=
    decl_fundef
      declspec.none
      (funtypeexpr_make
        (typeexpr_primitive primtype.bool)
        (from_list (funarg_named (mk_constref_of cls_type) "other" :: nil))
        (fnqual.set_noexcept (fnqual.set_const fnqual.none))
        false)
      (idexpr_operator scope_none (genop.binary binop.eq))
      attrspec.none
      (funbody_stmts 
        (from_list nil)
        (from_list
          (stmt_return
            (expr_binop binop.eq
              (expr_unop unop.deref (expr_id id_repr_m))
              (expr_unop unop.deref (expr_memdot
                (expr_id (idexpr_id scope_none "other")) id_repr_m))) ::
          nil))) in

  let repr_member_decl := (decl_simple declspec.none repr_ptr_type id_repr_m attrspec.none) in
  let clsdecls :=
    (from_list
      ((clsdecl_group
        visibility_spec.vis_public
        (from_list
          (default_constructor ::
           copy_constructor ::
           move_constructor ::
           assign_operator ::
           assign_move_operator ::
           eq_operator ::
           factory_fns ++
           match_fn_tpldecl ::
           nil))) ::
      (clsdecl_group
        visibility_spec.vis_private
        (from_list (
          repr_base_class_def ::
          repr_classes ++
          primary_constructor ::
          repr_member_decl ::
          nil))) ::
      nil)) in
  oind_cpp_impl.make tpl cls_name clsdecls.

Definition ind_convert (i : inductive.t) : ind_cpp_impl.t :=
  match i with
    | inductive.make oinds =>
      map oind_convert oinds
  end.

Definition oind_gen_fwddecl (r : oind_cpp_impl.t) : decl_t :=
  let (tpl, name, decls) := r in
  let d := decl_class_fwd (mk_sid name) in
  decl_maybe_template tpl d.

Definition oind_gen_clsdecl (r : oind_cpp_impl.t) : decl_t := 
  let (tplformargs, clsname, clsdecls) := r in
  class_strip_member_defs tplformargs clsname clsdecls.

Definition oind_gen_otherdecl (r : oind_cpp_impl.t) : decls_t :=
  let (tplformargs, clsname, clsdecls) := r in
  from_list (class_extract_member_defs tplformargs clsname clsdecls).

Definition ind_gen_fwddecls (r : list oind_cpp_impl.t) : decls_t :=
  from_list (map (oind_gen_fwddecl) r).

Definition ind_gen_clsdecls (r : ind_cpp_impl.t) : decls_t :=
  from_list (map oind_gen_clsdecl r).

Definition ind_gen_otherdecl (r : ind_cpp_impl.t) : decls_t :=
  let decls := decls_nil in
  (fix loop (decls : decls_t) (r : list oind_cpp_impl.t) {struct r} : decls_t :=
    match r with
      | nil => decls
      | cons oind r =>
        let d := oind_gen_otherdecl oind in
        let decls := app decls d in
        loop decls r
    end) decls r.

Definition ind_gen_decls (r : ind_cpp_impl.t) : decls_t :=
  app (app (ind_gen_fwddecls r) (ind_gen_clsdecls r)) (ind_gen_otherdecl r).

(**** EXAMPLES ****)

Example sort_set := (expr_global "Set"%string).

Example oiX :=
  one_inductive.make "X" (expr_prod (arg_named "T"%string sort_set) sort_set) (
    (* constructors *)
    inductive_constructor.make "X0"
      (expr_prod 
        (arg_named "t"%string (expr_local "T"%string 1))
        (expr_app (expr_global "X"%string) (expr_local "T"%string 1))) ::
    inductive_constructor.make "X1"
      (expr_prod
        (arg_named "y"%string (expr_app (expr_global "Y"%string) (expr_local "T"%string 1)))
        (expr_app (expr_global "X"%string) (expr_local "T"%string 1))) ::
    nil).

Example oiY :=
  one_inductive.make "Y" (expr_prod (arg_named "T"%string sort_set) sort_set) (
    (* constructors *)
    inductive_constructor.make "Y0"
      (expr_prod
        (arg_named "t"%string (expr_local "T"%string 1))
        (expr_app (expr_global "Y"%string) (expr_local "T"%string 1))) ::
    inductive_constructor.make "Y1"
      (expr_prod
        (arg_named "x"%string (expr_app (expr_global "X"%string) (expr_local "T"%string 1)))
        (expr_app (expr_global "Y"%string) (expr_local "T"%string 1))) ::
    nil).

Example i1 := inductive.make (oiX :: oiY :: nil).
Example i_cpp := ind_convert i1.

Eval lazy in (serialize_tokens (decls_serialize (ind_gen_decls i_cpp))).

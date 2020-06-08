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
  CPPSynth.CPPAst
  CPPSynth.Monad.

Module cg.
  Definition named_type (s : string) : typeexpr_t :=
    typeexpr_id (idexpr_id scope_none s).
  Definition shared_ptr_of (type : typeexpr_t) : typeexpr_t :=
  typeexpr_id (
    idexpr_template (scope_id "std" scope_none) (templateid "shared_ptr"
      (tplargs_cons (tplarg_type type) tplargs_nil))).
  Definition constref_of (type : typeexpr_t) : typeexpr_t :=
    typeexpr_reference (typeexpr_const type).
  Definition constptr_of (type : typeexpr_t) : typeexpr_t :=
    typeexpr_pointer (typeexpr_const type).
  Definition declval_of (type : typeexpr_t) :=
    expr_call
      (expr_id (
        idexpr_template (scope_id "std" scope_none) (templateid "declval"
          (tplargs_cons (tplarg_type type) tplargs_nil))))
      callargs_nil.
  Definition fn_type (result_type : typeexpr_t) (args : list (option string * typeexpr_t)) : typeexpr_t :=
    let fnargs := make_funargs (map (fun (a : option string * typeexpr_t) => let (name, type) := a in
      match name with
        | None => funarg_anon type
        | Some n => funarg_named type n
      end) args) in
    typeexpr_function (funtypeexpr result_type fnargs false).
  Definition type_auto := typeexpr_primitive primtype.auto.
End cg.

Fixpoint make_tplargs (args : list tplarg_t) : tplargs_t :=
  match args with
    | nil => tplargs_nil
    | arg :: args => tplargs_cons arg (make_tplargs args)
  end.

Fixpoint make_tplformargs (args : list tplformarg_t) : tplformargs_t :=
  match args with
    | nil => tplformargs_nil
    | arg :: args => tplformargs_cons arg (make_tplformargs args)
  end.

Module env.
  Inductive t : Set :=
    | empty : t.

  Fixpoint to_type (e : expr.t) (this : t) : typeexpr_t :=
    match e with
      | expr.global n => cg.named_type n
      | expr.local n _ => cg.named_type n
      | expr.app fn arg =>
        to_type_template_apply fn ((to_type arg this) :: nil) this
      | expr.product argname argtype body =>
        to_type_fn_abstract ((argname, to_type argtype this) :: nil) body this
    end
  with to_type_template_apply (fn : expr.t) (args : list typeexpr_t) (this : t) : typeexpr_t :=
    match fn with
      | expr.global n =>
        let tplargs := make_tplargs (map (fun arg => tplarg_type arg) args) in
        typeexpr_id (idexpr_template scope_none (templateid n tplargs))
      | expr.app fn arg =>
        to_type_template_apply fn (args ++ (to_type arg this) :: nil) this
      | _ => typeexpr_primitive (primtype.auto)
    end
  with to_type_fn_abstract (args : list (option string * typeexpr_t)) (body : expr.t) (this : t) : typeexpr_t :=
    match body with
      | expr.product argname argtype body =>
        to_type_fn_abstract (args ++ ((argname, to_type argtype this) :: nil)) body this
      | expr.global n =>
        let result_type := cg.named_type n in
        cg.fn_type result_type args
      | expr.local n _ =>
        let result_type := cg.named_type n in
        cg.fn_type result_type args
      | expr.app fn arg =>
        let result_type := to_type_template_apply fn ((to_type arg this) :: nil) this in
        cg.fn_type result_type args
    end.
  Definition to_tplformarg (name : string) (e : expr.t) (this : t) : tplformarg_t :=
    match e with
      | expr.global ename =>
        if string_dec ename "Set" then
          tplformarg_typename name
        else if string_dec ename "Type" then
          tplformarg_typename name
        else
          tplformarg_value (to_type e this) name
      | _ => tplformarg_value (to_type e this) name
    end.
End env.

Definition product_unfold (e : expr.t) : list (option string * expr.t) * expr.t :=
  let head := nil in
  (fix loop (e : expr.t) (head : list (option string * expr.t)) 
      : list (option string * expr.t) * expr.t :=
    match e with
      | expr.product argname argtype body =>
        let head := head ++ (argname, argtype) :: nil in
        loop body head
      | _ =>
        (head, e)
    end) e head.

(* make choice of prefix name configurable *)
Definition product_named_unfold (e : expr.t) : list (string * expr.t) * expr.t :=
  let head := nil in
  let index := 0 in
  (fix loop (e : expr.t) (index : nat) (head : list (string * expr.t)) 
      : list (string * expr.t) * expr.t :=
    match e with
      | expr.product argname argtype body =>
        let name :=
          match argname with
            | Some name => name
            | None => ("x" ++ (string_of_nat index))%string
          end in
        let head := head ++ (name, argtype) :: nil in
        loop body (S index) head
      | _ =>
        (head, e)
    end) e index head.

Fixpoint collect_tplformargs_from_product
    (e : expr.t) : tplformargs_t :=
  match e with
    | expr.product maybe_name argtype body =>
      let name :=
        match maybe_name with
          | Some name => name
          | None => "T"%string
        end in
      tplformargs_cons (tplformarg_typename name) (collect_tplformargs_from_product body)
    | _ => tplformargs_nil
  end.

Definition maybe_template (tpl : tplformargs_t) (d : decl_t) : decl_t :=
  match tpl with
    | tplformargs_nil => d
    | _ => decl_templated tpl d
  end.

Definition one_inductive_fwd (oi : one_inductive.t) : decl_t :=
  let (name, sig, constructors) := oi in
  let tpl := collect_tplformargs_from_product sig in
  let d := decl_class_fwd (idexpr_id scope_none name) in
  maybe_template tpl d.

Definition inductive_fwd (i : inductive.t) : list decl_t :=
  let (ois) := i in
  map (one_inductive_fwd) ois.

Definition gen_virtual_destructor_decldef (name : string) : decl_t :=
  decl_consdesdef
    (decl_specifier.ds_virtual :: nil)
    (idexpr_destructor scope_none name)
    (make_funargs nil)
    nil
    (make_stmts nil).

Definition make_repr_base_class (name : string) :=
  decl_class 
    (idexpr_id scope_none name) 
    false 
    clsinherits_nil
    (clsdecls_cons
      (clsdecl_group
        visibility_spec.vis_public
        (decls_cons
          (gen_virtual_destructor_decldef name)
          decls_nil))
      clsdecls_nil).

Definition constructor_reprclass_name (cons : inductive_constructor.t) : string :=
  ((inductive_constructor.name cons) ++ "_repr").

Definition constructor_reprclass_members (cons : inductive_constructor.t)
    : list (string * expr.t) :=
  fst (product_named_unfold (inductive_constructor.type cons)).

Definition make_constructor_repr_class
    (cons : inductive_constructor.t)
    (environ : env.t) :=
  let name := constructor_reprclass_name cons in
  let members := constructor_reprclass_members cons in
  let member_decls := map
    (fun (mem : string * expr.t) =>
      let (name, e) := mem in
      let type := env.to_type e environ in
      decl_simple nil type (idexpr_id scope_none name) nil) members in
  decl_class 
    (idexpr_id scope_none (name))
    true
    (clsinherits_cons (clsinherit false visibility_spec.vis_public (idexpr_id scope_none "repr_base")) clsinherits_nil)
    (make_clsdecls 
      ((clsdecl_group
        visibility_spec.vis_public
        (make_decls member_decls)) :: nil)).

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
  let target_type := cg.constptr_of as_type in
  let temp_expr := expr_id temp_name in
  let callargs := 
    map 
      (fun member => 
        expr_memarrow (expr_id temp_name) (idexpr_id scope_none member))
      members in
  let cond := (condition_decl
      nil (cg.type_auto) temp_name nil
      (expr_dynamic_cast target_type base_expr)) in
  let body := 
    stmt_block (
      make_stmts 
        ((stmt_return (expr_call call_expr (make_callargs callargs))) :: nil)) in
  (cond, body).

(*
  Produce a statement that checks 'base_expr' with match for any of the
  constructor repr classes (via dynamic_cast) and dispatches to the
  corresponding call expression.
*)
Fixpoint match_constructors
    (base_expr : expr_t) (branches : list (inductive_constructor.t * expr_t))
    : stmt_t :=
  match branches with
    | nil => stmt_block (make_stmts nil) (* XXX: should be std::logic_error *)
    | branch :: branches =>
      let sub := match_constructors base_expr branches in
      let (cons, call_expr) := branch in
      let cons_name := constructor_reprclass_name cons in
      let cons_members := constructor_reprclass_members cons in
      let member_names := map fst cons_members in
      let as_type := typeexpr_id (idexpr_id scope_none cons_name) in
      let temp_name := idexpr_id scope_none "tmp" in 
      let (cond, body) := make_single_match_test base_expr as_type temp_name call_expr member_names in
      stmt_ifelse cond body sub
  end.

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
        make_funargs (
          map 
            (fun name =>
             funarg_named 
              (cg.constref_of (cg.named_type ("F" ++ name)))
              ("f" ++ name))
            consnames) in
      (* arguments to first constructor *)
      let pre_callargs := fst (product_unfold (inductive_constructor.type first_constructor)) in
      (* turn them into "declval" constructs *)
      let callargs := map (fun (ca : option string * expr.t) =>
        let (_, e) := ca in
        let type := env.to_type e environ in
        cg.declval_of (cg.constref_of type)) pre_callargs in
      let ret_type :=
        typeexpr_decltype (expr_call (expr_id (idexpr_id scope_none ("F" ++ (inductive_constructor.name first_constructor)))) (make_callargs callargs)) in
      (tplargs, funtypeexpr ret_type formal_args true)
    | nil =>
      (nil, funtypeexpr (typeexpr_primitive primtype.void) funargs_nil false)
  end.

Definition one_inductive_decl (oi : one_inductive.t) : decl_t :=
  let environ := env.empty in
  let (name, sig, constructors) := oi in
  let tpl := collect_tplformargs_from_product sig in
  let consnames := map (fun (c : inductive_constructor.t) => let (name, _) := c in name) constructors in
  let (match_fn_tplargs, match_fn_type) := make_match_fntype constructors environ in
  let cons_call := map
    (fun (cons : inductive_constructor.t) => 
      (cons, expr_id (idexpr_id scope_none ("f" ++ inductive_constructor.name cons)))) constructors in
  let match_fn_decl := 
    decl_fundef 
      nil 
      match_fn_type
      (idexpr_id scope_none "match") 
      nil 
      (stmts_cons (match_constructors (expr_id (idexpr_id scope_none "repr_")) cons_call) stmts_nil) in
  let match_fn_tpldecl := maybe_template (make_tplformargs match_fn_tplargs) match_fn_decl in
  let constructor_classes := map (fun c => make_constructor_repr_class c environ) constructors in
  let d := decl_class (idexpr_id scope_none name) false clsinherits_nil
    (make_clsdecls 
      ((clsdecl_group
        visibility_spec.vis_public
        (make_decls
          (match_fn_tpldecl :: nil))) ::
      (clsdecl_group
        visibility_spec.vis_private
        (make_decls (
          make_repr_base_class "repr_base" ::
          (decl_simple nil (cg.shared_ptr_of (cg.named_type "repr_base")) (idexpr_id scope_none "repr_") nil) ::
          constructor_classes))) ::
      nil)) in
  maybe_template tpl d.

Fixpoint to_decls (l : list decl_t) :=
  match l with
    | nil => decls_nil
    | cons d l => decls_cons d (to_decls l)
  end.

Example sort_set := (expr.global "Set"%string).

Example oiX :=
  one_inductive.make "X" (expr.product (Some "T"%string) sort_set sort_set) (
    (* constructors *)
    inductive_constructor.make "X0" 
      (expr.product (Some "t"%string)
        (expr.local "T"%string 1) 
        (expr.app (expr.global "X"%string) (expr.local "T"%string 1))) ::
    inductive_constructor.make "X1" 
      (expr.product (Some "y"%string)
        (expr.app (expr.global "Y"%string) (expr.local "T"%string 1))
        (expr.app (expr.global "X"%string) (expr.local "T"%string 1))) ::
    nil).

Example i1 := inductive.make (oiX :: nil).

Eval lazy in (collect_tplformargs_from_product (let (_, sig, _) := oiX in sig)).

Eval lazy in (serialize_tokens (decls_serialize (to_decls (inductive_fwd i1)))).

Eval lazy in (serialize_tokens (decl_serialize (one_inductive_decl oiX))).

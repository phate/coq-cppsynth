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
  let d := decl_class_fwd (idexpr_id name) in
  maybe_template tpl d.

Definition inductive_fwd (i : inductive.t) : list decl_t :=
  let (ois) := i in
  map (one_inductive_fwd) ois.

Definition gen_virtual_destructor_decldef (name : string) : decl_t :=
  decl_consdesdef
    (decl_specifier.ds_virtual :: nil)
    (idexpr_destructor name)
    (make_funargs nil)
    nil
    (make_stmts nil).

Definition make_repr_base_class (name : string) :=
  decl_class (idexpr_id name)
    (clsdecls_cons
      (clsdecl_group
        visibility_spec.vis_public
        (decls_cons
          (gen_virtual_destructor_decldef name)
          decls_nil))
      clsdecls_nil).

Definition one_inductive_decl (oi : one_inductive.t) : decl_t :=
  let (name, sig, constructors) := oi in
  let tpl := collect_tplformargs_from_product sig in
  decl_class (idexpr_id name) 
    (clsdecls_cons 
      (clsdecl_group
        visibility_spec.vis_public
        (decls_cons
          (make_repr_base_class "repr")
          decls_nil))
      clsdecls_nil).

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
      (expr.product None 
        (expr.local "T"%string 1) 
        (expr.app (expr.global "X"%string) (expr.local "T"%string 1))) ::
    inductive_constructor.make "X1" 
      (expr.product None
        (expr.app (expr.global "Y"%string) (expr.local "T"%string 1))
        (expr.app (expr.global "X"%string) (expr.local "T"%string 1))) ::
    nil).

Example i1 := inductive.make (oiX :: nil).


Eval lazy in (collect_tplformargs_from_product (let (_, sig, _) := oiX in sig)).

Eval lazy in (serialize_tokens (decls_serialize (to_decls (inductive_fwd i1)))).

Eval lazy in (serialize_tokens (decl_serialize (one_inductive_decl oiX))).

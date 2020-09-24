Require Import
  Coq.Lists.List
  Coq.Strings.String
  CPPSynth.CPPAst
  CPPSynth.CPPAstBase
  CPPSynth.CPPAstHelper
  CPPSynth.CoqAstBase
  CPPSynth.Gallina
  CPPSynth.ListClass.


Module env.
  Inductive t : Set :=
    | empty : t.

  Fixpoint to_type (e : expr_t) (this : t) {struct e} : typeexpr_t :=
    match e with
      | expr_global n => mk_named_type n
      | expr_local n _ => mk_named_type n
      | expr_app fn arg =>
        to_type_template_apply fn ((to_type arg this) :: nil) this
      | expr_prod arg body =>
        to_type_fn_abstract (to_arg arg this :: nil) body this
      | _ =>
        (typeexpr_primitive primtype.auto) (* unsupported *)
    end
  with to_arg (arg : arg_t) (this : t) {struct arg} : (option string * typeexpr_t) :=
    match arg with
      | arg_named name type => (Some (name.to_string name), to_type type this)
      | arg_anonymous type => (None, to_type type this)
    end
  with to_type_template_apply (fn : expr_t) (args : list typeexpr_t) (this : t) {struct fn} : typeexpr_t :=
    match fn with
      | expr_global n =>
        let tplargs := from_list (map (fun arg => tplarg_type arg) args) in
        typeexpr_id (idexpr_template scope_none (templateid_make n tplargs))
      | expr_app fn arg =>
        to_type_template_apply fn (args ++ (to_type arg this) :: nil) this
      | _ => typeexpr_primitive (primtype.auto)
    end
  with to_type_fn_abstract (args : list (option string * typeexpr_t)) (body : expr_t) (this : t) {struct body} : typeexpr_t :=
    match body with
      | expr_prod arg body =>
        to_type_fn_abstract (args ++ (to_arg arg this :: nil)) body this
      | expr_global n =>
        let result_type := mk_named_type n in
        mk_fn_type result_type args
      | expr_local n _ =>
        let result_type := mk_named_type n in
        mk_fn_type result_type args
      | expr_app fn arg =>
        let result_type := to_type_template_apply fn ((to_type arg this) :: nil) this in
        mk_fn_type result_type args
      | _ =>
        (typeexpr_primitive primtype.auto) (* unsupported *)
    end.
  Definition to_tplformarg (name : string) (e : expr_t) (this : t) : tplformarg_t :=
    match e with
      | expr_global ename =>
        if string_dec ename "Set" then
          tplformarg_typename name
        else if string_dec ename "Type" then
          tplformarg_typename name
        else
          tplformarg_value (to_type e this) name
      | _ => tplformarg_value (to_type e this) name
    end.
End env.


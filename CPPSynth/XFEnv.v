Require Import
  Coq.Lists.List
  Coq.Strings.String
  CPPSynth.CPPAst
  CPPSynth.CPPAstBase
  CPPSynth.CPPAstHelper
  CPPSynth.Gallina
  CPPSynth.ListClass.


Module env.
  Inductive t : Set :=
    | empty : t.

  Fixpoint to_type (e : expr.t) (this : t) : typeexpr_t :=
    match e with
      | expr.global n => mk_named_type n
      | expr.local n _ => mk_named_type n
      | expr.app fn arg =>
        to_type_template_apply fn ((to_type arg this) :: nil) this
      | expr.product argname argtype body =>
        to_type_fn_abstract ((argname, to_type argtype this) :: nil) body this
    end
  with to_type_template_apply (fn : expr.t) (args : list typeexpr_t) (this : t) : typeexpr_t :=
    match fn with
      | expr.global n =>
        let tplargs := from_list (map (fun arg => tplarg_type arg) args) in
        typeexpr_id (idexpr_template scope_none (templateid_make n tplargs))
      | expr.app fn arg =>
        to_type_template_apply fn (args ++ (to_type arg this) :: nil) this
      | _ => typeexpr_primitive (primtype.auto)
    end
  with to_type_fn_abstract (args : list (option string * typeexpr_t)) (body : expr.t) (this : t) : typeexpr_t :=
    match body with
      | expr.product argname argtype body =>
        to_type_fn_abstract (args ++ ((argname, to_type argtype this) :: nil)) body this
      | expr.global n =>
        let result_type := mk_named_type n in
        mk_fn_type result_type args
      | expr.local n _ =>
        let result_type := mk_named_type n in
        mk_fn_type result_type args
      | expr.app fn arg =>
        let result_type := to_type_template_apply fn ((to_type arg this) :: nil) this in
        mk_fn_type result_type args
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


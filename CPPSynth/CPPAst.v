Require Import
  List
  String
  CPPSynth.ListClass
  CPPSynth.CPPAstBase.

(* declarators, namespace / class level *)
Inductive decls_t : Set :=
  | decls_nil : decls_t
  | decls_cons : decl_t -> decls_t -> decls_t

(* note: much more restrictive than what C++ allows, one declarator per block etc. *)
with decl_t : Set :=
  | decl_simple : (* covers function and variable declarations *)
    forall (ds : declspec.t),
    forall (type : typeexpr_t),
    forall (id : idexpr_t),
    forall (attrs : attrspec.t),
    decl_t
  | decl_initdef : (* covers definition via assignment initialization *)
    forall (ds : declspec.t),
    forall (type : typeexpr_t),
    forall (id : idexpr_t),
    forall (attrs : attrspec.t),
    forall (value : expr_t),
    decl_t
  | decl_fundef : (* strict function definitions *)
    forall (ds : declspec.t),
    forall (type : funtypeexpr_t),
    forall (id : idexpr_t),
    forall (attrs : attrspec.t),
    forall (body : funbody_t),
    decl_t
  | decl_consdesdecl : (* constructor and destructor declarations *)
    forall (ds : declspec.t),
    forall (id : idexpr_t),
    forall (args : funargs_t),
    forall (qual : fnqual.t),
    forall (attrs : attrspec.t),
    decl_t
  | decl_consdesdef : (* constructor and destructor definitions *)
    forall (ds : declspec.t),
    forall (id : idexpr_t),
    forall (args : funargs_t),
    forall (qual : fnqual.t),
    forall (attrs : attrspec.t),
    forall (body : funbody_t),
    decl_t
  | decl_class_fwd :
    forall (id : idexpr_t),
    decl_t
  | decl_class :
    forall (id : idexpr_t),
    forall (is_final : bool),
    forall (inherits : clsinherits_t),
    forall (body : clsdecls_t),
    decl_t
  | decl_templated : (* template any of the above -- should not template templates *)
    forall (args : tplformargs_t),
    forall (decl : decl_t),
    decl_t

with clsdecls_t : Set :=
  | clsdecls_nil : clsdecls_t
  | clsdecls_cons : clsdecl_t -> clsdecls_t -> clsdecls_t

with clsdecl_t : Set :=
  | clsdecl_group :
    forall (visibility : visibility_spec.t),
    forall (decls : decls_t),
    clsdecl_t

with clsinherits_t : Set :=
  | clsinherits_nil : clsinherits_t
  | clsinherits_cons : clsinherit_t -> clsinherits_t -> clsinherits_t

with clsinherit_t : Set :=
  | clsinherit_single :
    forall (is_virtual : bool),
    forall (visibility : visibility_spec.t),
    forall (base : idexpr_t),
    clsinherit_t

with funbody_t : Set :=
  | funbody_abstract :
    funbody_t
  | funbody_default :
    funbody_t
  | funbody_delete :
    funbody_t
  | funbody_stmts :
    forall (init : cinits_t),
    forall (stmts : stmts_t),
    funbody_t

with cinits_t : Set :=
  | cinits_nil : cinits_t
  | cinits_cons : cinit_t -> cinits_t -> cinits_t

with cinit_t : Set :=
  | cinit_make :
    forall (id : idexpr_t),
    forall (expr : expr_t),
    cinit_t

(* expression for an id -- needs to include namespace and template-ids, then *)
with idexpr_t : Set :=
  | idexpr_id :
    forall (scope : scope_t),
    forall (id : string),
    idexpr_t
  | idexpr_destructor :
    forall (scope : scope_t),
    forall (id : string),
    idexpr_t
  | idexpr_template :
    forall (scope : scope_t),
    forall (tpl : templateid_t),
    idexpr_t
  | idexpr_operator :
    forall (scope : scope_t),
    forall (op : genop.t),
    idexpr_t

with scope_t : Set :=
  | scope_none : scope_t
  | scope_id :
    forall (id : string),
    forall (sub_scope : scope_t),
    scope_t
  | scope_template :
    forall (tpl : templateid_t),
    forall (sub_scope : scope_t),
    scope_t

with templateid_t : Set :=
  | templateid_make :
    forall (name : string),
    forall (args : tplargs_t),
    templateid_t

(* template formal arguments *)
with tplformargs_t : Set :=
  | tplformargs_nil : tplformargs_t
  | tplformargs_cons : tplformarg_t -> tplformargs_t -> tplformargs_t

with tplformarg_t : Set :=
  | tplformarg_typename :
    forall (id : string),
    tplformarg_t
  | tplformarg_value :
    forall (type : typeexpr_t),
    forall (id : string),
    tplformarg_t

(* template actual arguments *)

with tplargs_t : Set :=
  | tplargs_nil : tplargs_t
  | tplargs_cons : tplarg_t -> tplargs_t -> tplargs_t

with tplarg_t : Set :=
  | tplarg_type :
    forall (type : typeexpr_t),
    tplarg_t
  | tplarg_expr :
    forall (expr : expr_t),
    tplarg_t

(* terms representing types *)
with typeexpr_t : Set :=
  | typeexpr_primitive :
    forall (prim : primtype.t),
    typeexpr_t
  | typeexpr_id :
    forall (id : idexpr_t),
    typeexpr_t
  | typeexpr_const :
    forall (type : typeexpr_t),
    typeexpr_t
  | typeexpr_volatile :
    forall (type : typeexpr_t),
    typeexpr_t
  | typeexpr_pointer :
    forall (type : typeexpr_t),
    typeexpr_t
  | typeexpr_reference :
    forall (type : typeexpr_t),
    typeexpr_t
  | typeexpr_rvaluereference :
    forall (type : typeexpr_t),
    typeexpr_t
  | typeexpr_array :
    forall (type : typeexpr_t),
    forall (size : expr_t),
    typeexpr_t
  | typeexpr_unspec_array :
    forall (type : typeexpr_t),
    typeexpr_t
  | typeexpr_function :
    forall (type : funtypeexpr_t),
    typeexpr_t
  | typeexpr_decltype :
    forall (expr : expr_t),
    typeexpr_t

with funtypeexpr_t : Set :=
  | funtypeexpr_make :
    forall (ret_type : typeexpr_t),
    forall (args : funargs_t),
    forall (q : fnqual.t),
    forall (postfix_form : bool),
    funtypeexpr_t

with funargs_t : Set :=
  | funargs_nil : funargs_t
  | funargs_cons : funarg_t -> funargs_t -> funargs_t

with funarg_t : Set :=
  | funarg_named :
    forall (type : typeexpr_t),
    forall (name : String.string),
    funarg_t
  | funarg_anon :
    forall (type : typeexpr_t),
    funarg_t

(* Statements (function-level) *)

with stmts_t : Set :=
  | stmts_nil : stmts_t
  | stmts_cons : stmt_t -> stmts_t -> stmts_t

with stmt_t : Set :=
  | stmt_decl :
    forall (decl : decl_t),
    stmt_t
  | stmt_expr :
    forall (expr : expr_t),
    stmt_t
  | stmt_block :
    forall (body : stmts_t),
    stmt_t
  | stmt_if :
    forall (cond : condition_t),
    forall (then_body : stmt_t),
    stmt_t
  | stmt_ifelse :
    forall (cond : condition_t),
    forall (then_body : stmt_t),
    forall (else_body : stmt_t),
    stmt_t
  | stmt_while :
    forall (cond : condition_t),
    forall (body : stmt_t),
    stmt_t
  | stmt_do_while :
    forall (body : stmt_t),
    forall (cond : condition_t),
    stmt_t
  | stmt_for :
    forall (init : condition_t),
    forall (cond : condition_t),
    forall (cont : expr_t),
    forall (body : stmt_t),
    stmt_t
  | stmt_for_range :
    forall (type : typeexpr_t),
    forall (id : idexpr_t),
    forall (range : expr_t),
    forall (body : stmt_t),
    stmt_t
  | stmt_return :
    forall (expr : expr_t), stmt_t

with condition_t : Set :=
  | condition_expr :
    forall (e : expr_t), condition_t
  | condition_decl :
    (* this is the same as decl_initdef -- maybe merge? *)
    forall (ds : declspec.t),
    forall (type : typeexpr_t),
    forall (id : idexpr_t),
    forall (attrs : attrspec.t),
    forall (value : expr_t),
    condition_t

(* expressions *)

with expr_t : Set :=
  | expr_id :
    forall (id : idexpr_t), expr_t
  | expr_literal :
    forall (value : literal.t), expr_t
  | expr_unop :
    forall (op : unop.t) (arg : expr_t), expr_t
  | expr_binop :
    forall (op : binop.t) (arg1 : expr_t) (arg2 : expr_t), expr_t
  | expr_ternop :
    forall (cond : expr_t) (then_expr : expr_t) (else_expr : expr_t), expr_t
  | expr_call :
    forall (fn : expr_t) (args : callargs_t), expr_t
  | expr_lambda :
    forall (binders : binders_t),
    forall (args : funargs_t),
    forall (body : stmts_t),
    expr_t
  | expr_lambda_rettype :
    forall (binders : binders_t),
    forall (args : funargs_t),
    forall (ret_type : typeexpr_t),
    forall (body : stmts_t),
    expr_t
  | expr_dynamic_cast :
    forall (target_type : typeexpr_t) (value : expr_t),
    expr_t
  | expr_memdot :
    forall (structure : expr_t) (member : idexpr_t), expr_t
  | expr_memarrow :
    forall (structure : expr_t) (member : idexpr_t), expr_t
  | expr_throw :
    forall (expr : expr_t), expr_t

with callargs_t : Set :=
  | callargs_nil : callargs_t
  | callargs_cons : expr_t -> callargs_t -> callargs_t

with binders_t : Set :=
  | binders_nil : binders_t
  | binders_cons : binder_t -> binders_t -> binders_t

with binder_t : Set :=
  | binder_allcopy : binder_t
  | binder_allref : binder_t
  | binder_copy :
    forall (id : String.string),
    binder_t
  | binder_ref :
    forall (id : String.string),
    binder_t
  | binder_generic :
    forall (id : String.string),
    forall (expr : expr_t),
    binder_t.

#[refine] Instance decls_list : ListClass decl_t decls_t :=
{|
  to_list := decls_t_rec (fun _ => list decl_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => decls_t) decls_nil (fun n l nl => decls_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

#[refine] Instance clsdecls_list : ListClass clsdecl_t clsdecls_t :=
{|
  to_list := clsdecls_t_rec (fun _ => list clsdecl_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => clsdecls_t) clsdecls_nil (fun n l nl => clsdecls_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

#[refine] Instance cinits_list : ListClass cinit_t cinits_t :=
{|
  to_list := cinits_t_rec (fun _ => list cinit_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => cinits_t) cinits_nil (fun n l nl => cinits_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

#[refine] Instance clsinherits_list : ListClass clsinherit_t clsinherits_t :=
{|
  to_list := clsinherits_t_rec (fun _ => list clsinherit_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => clsinherits_t) clsinherits_nil (fun n l nl => clsinherits_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

#[refine] Instance tplformargs_list : ListClass tplformarg_t tplformargs_t :=
{|
  to_list := tplformargs_t_rec (fun _ => list tplformarg_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => tplformargs_t) tplformargs_nil (fun n l nl => tplformargs_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

#[refine] Instance tplargs_list : ListClass tplarg_t tplargs_t :=
{|
  to_list := tplargs_t_rec (fun _ => list tplarg_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => tplargs_t) tplargs_nil (fun n l nl => tplargs_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

#[refine] Instance funargs_list : ListClass funarg_t funargs_t :=
{|
  to_list := funargs_t_rec (fun _ => list funarg_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => funargs_t) funargs_nil (fun n l nl => funargs_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

#[refine] Instance stmts_list : ListClass stmt_t stmts_t :=
{|
  to_list := stmts_t_rec (fun _ => list stmt_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => stmts_t) stmts_nil (fun n l nl => stmts_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

#[refine] Instance callargs_list : ListClass expr_t callargs_t :=
{|
  to_list := callargs_t_rec (fun _ => list expr_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => callargs_t) callargs_nil (fun n l nl => callargs_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

#[refine] Instance binders_list : ListClass binder_t binders_t :=
{|
  to_list := binders_t_rec (fun _ => list binder_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => binders_t) binders_nil (fun n l nl => binders_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

Definition bool_eq_dec (x y : bool) : {x=y} + {x<>y}.
  decide equality.
Defined.

Definition typeexpr_eq_dec (x y : typeexpr_t) : {x=y} + {x<>y}.
  refine ((
    fix decls_eq_dec (x y : decls_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | decls_nil, decls_nil => left _ _
        | decls_cons decl1 decls1, decls_cons decl2 decls2 =>
          if decl_eq_dec decl1 decl2 then
            if decls_eq_dec decls1 decls2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with decl_eq_dec (x y : decl_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | decl_simple ds1 type1 id1 attrs1, decl_simple ds2 type2 id2 attrs2 =>
          if declspec.eq_dec ds1 ds2 then
            if typeexpr_eq_dec type1 type2 then
              if idexpr_eq_dec id1 id2 then
                if attrspec.eq_dec attrs1 attrs2 then left _ _ else right _ _
              else right _ _
            else right _ _
          else right _ _
        | decl_initdef ds1 type1 id1 attrs1 value1, decl_initdef ds2 type2 id2 attrs2 value2 =>
          if declspec.eq_dec ds1 ds2 then
            if typeexpr_eq_dec type1 type2 then
              if idexpr_eq_dec id1 id2 then
                if attrspec.eq_dec attrs1 attrs2 then
                  if expr_eq_dec value1 value2 then left _ _ else right _ _
                else right _ _
              else right _ _
            else right _ _
          else right _ _
        | decl_fundef ds1 type1 id1 attrs1 body1, decl_fundef ds2 type2 id2 attrs2 body2 =>
          if declspec.eq_dec ds1 ds2 then
            if funtypeexpr_eq_dec type1 type2 then
              if idexpr_eq_dec id1 id2 then
                if attrspec.eq_dec attrs1 attrs2 then
                  if funbody_eq_dec body1 body2 then left _ _ else right _ _
                else right _ _
              else right _ _
            else right _ _
          else right _ _
        | decl_consdesdecl ds1 id1 args1 qual1 attrs1, decl_consdesdecl ds2 id2 args2 qual2 attrs2 =>
          if declspec.eq_dec ds1 ds2 then
            if idexpr_eq_dec id1 id2 then
              if funargs_eq_dec args1 args2 then
                if fnqual.eq_dec qual1 qual2 then
                  if attrspec.eq_dec attrs1 attrs2 then left _ _ else right _ _
                else right _ _
              else right _ _
            else right _ _
          else right _ _
        | decl_consdesdef ds1 id1 args1 qual1 attrs1 body1, decl_consdesdef ds2 id2 args2 qual2 attrs2 body2 =>
          if declspec.eq_dec ds1 ds2 then
            if idexpr_eq_dec id1 id2 then
              if funargs_eq_dec args1 args2 then
                if fnqual.eq_dec qual1 qual2 then
                  if attrspec.eq_dec attrs1 attrs2 then
                    if funbody_eq_dec body1 body2 then left _ _ else right _ _
                  else right _ _
                else right _ _
              else right _ _
            else right _ _
          else right _ _
        | decl_class_fwd id1, decl_class_fwd id2 =>
          if idexpr_eq_dec id1 id2 then left _ _ else right _ _
        | decl_class id1 is_final1 inherits1 body1, decl_class id2 is_final2 inherits2 body2 =>
          if idexpr_eq_dec id1 id2 then
            if bool_eq_dec is_final1 is_final2 then
              if clsinherits_eq_dec inherits1 inherits2 then
                if clsdecls_eq_dec body1 body2 then left _ _ else right _ _
              else right _ _
            else right _ _
          else right _ _
        | decl_templated args1 decl1, decl_templated args2 decl2 =>
          if tplformargs_eq_dec args1 args2 then
            if decl_eq_dec decl1 decl2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with clsdecls_eq_dec (x y : clsdecls_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | clsdecls_nil, clsdecls_nil => left _ _
        | clsdecls_cons decl1 decls1, clsdecls_cons decl2 decls2 =>
          if clsdecl_eq_dec decl1 decl2 then
            if clsdecls_eq_dec decls1 decls2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with clsdecl_eq_dec (x y : clsdecl_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | clsdecl_group visibility1 decls1, clsdecl_group visibility2 decls2 =>
          if visibility_spec.eq_dec visibility1 visibility2 then
            if decls_eq_dec decls1 decls2 then left _ _ else right _ _
          else right _ _
      end
    with clsinherits_eq_dec (x y : clsinherits_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | clsinherits_nil, clsinherits_nil => left _ _
        | clsinherits_cons inh1 inhs1, clsinherits_cons inh2 inhs2 =>
          if clsinherit_eq_dec inh1 inh2 then
            if clsinherits_eq_dec inhs1 inhs2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with clsinherit_eq_dec (x y : clsinherit_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | clsinherit_single is_virtual1 visibility1 base1, clsinherit_single is_virtual2 visibility2 base2 =>
          if bool_eq_dec is_virtual1 is_virtual2 then
            if visibility_spec.eq_dec visibility1 visibility2 then
              if idexpr_eq_dec base1 base2 then left _ _ else right _ _
            else right _ _
          else right _ _
      end
    with cinit_eq_dec (x y : cinit_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | cinit_make id1 expr1, cinit_make id2 expr2 =>
          if idexpr_eq_dec id1 id2 then
            if expr_eq_dec expr1 expr2 then left _ _ else right _ _
          else right _ _
      end
    with funbody_eq_dec (x y : funbody_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | funbody_abstract, funbody_abstract => left _ _
        | funbody_default, funbody_default => left _ _
        | funbody_delete, funbody_delete => left _ _
        | funbody_stmts init1 stmts1, funbody_stmts init2 stmts2 =>
          if cinits_eq_dec init1 init2 then
            if stmts_eq_dec stmts1 stmts2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with cinits_eq_dec (x y : cinits_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | cinits_nil, cinits_nil => left _ _
        | cinits_cons init1 inits1, cinits_cons init2 inits2 =>
          if cinit_eq_dec init1 init2 then
            if cinits_eq_dec inits1 inits2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with cinit_eq_dec (x y : cinit_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | cinit_make id1 expr1, cinit_make id2 expr2 =>
          if idexpr_eq_dec id1 id2 then
            if expr_eq_dec expr1 expr2 then left _ _ else right _ _
          else right _ _
      end
    with idexpr_eq_dec (x y : idexpr_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | idexpr_id scope1 id1, idexpr_id scope2 id2 =>
          if scope_eq_dec scope1 scope2 then
            if string_dec id1 id2 then left _ _ else right _ _ 
          else right _ _
        | idexpr_destructor scope1 id1, idexpr_destructor scope2 id2 =>
          if scope_eq_dec scope1 scope2 then
            if string_dec id1 id2 then left _ _ else right _ _ 
          else right _ _
        | idexpr_template scope1 tpl1, idexpr_template scope2 tpl2 =>
          if scope_eq_dec scope1 scope2 then
            if templateid_eq_dec tpl1 tpl2 then left _ _ else right _ _
          else right _ _
        | idexpr_operator scope1 op1, idexpr_operator scope2 op2 =>
          if scope_eq_dec scope1 scope2 then
            if genop.eq_dec op1 op2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with scope_eq_dec (x y : scope_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | scope_none, scope_none => left _ _
        | scope_id id1 sub_scope1, scope_id id2 sub_scope2 =>
          if string_dec id1 id2 then
            if scope_eq_dec sub_scope1 sub_scope2 then left _ _ else right _ _
          else right _ _
        | scope_template tpl1 sub_scope1, scope_template tpl2 sub_scope2 =>
          if templateid_eq_dec tpl1 tpl2 then
            if scope_eq_dec sub_scope1 sub_scope2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with templateid_eq_dec (x y : templateid_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | templateid_make name1 args1, templateid_make name2 args2 =>
          if string_dec name1 name2 then
            if tplargs_eq_dec args1 args2 then left _ _ else right _ _
          else right _ _
      end
    with tplformargs_eq_dec (x y : tplformargs_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | tplformargs_nil, tplformargs_nil => left _ _
        | tplformargs_cons arg1 args1, tplformargs_cons arg2 args2 =>
          if tplformarg_eq_dec arg1 arg2 then
            if tplformargs_eq_dec args1 args2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with tplformarg_eq_dec (x y : tplformarg_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | tplformarg_typename id1, tplformarg_typename id2 =>
          if string_dec id1 id2 then left _ _ else right _ _
        | tplformarg_value type1 id1, tplformarg_value type2 id2 =>
          if typeexpr_eq_dec type1 type2 then
            if string_dec id1 id2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with tplargs_eq_dec (x y : tplargs_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | tplargs_nil, tplargs_nil => left _ _
        | tplargs_cons arg1 args1, tplargs_cons arg2 args2 =>
          if tplarg_eq_dec arg1 arg2 then
            if tplargs_eq_dec args1 args2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with tplarg_eq_dec (x y : tplarg_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | tplarg_type type1, tplarg_type type2 =>
          if typeexpr_eq_dec type1 type2 then left _ _ else right _ _
        | tplarg_expr expr1, tplarg_expr expr2 =>
          if expr_eq_dec expr1 expr2 then left _ _ else right _ _
        | _, _ => right _ _
      end
    with typeexpr_eq_dec (x y : typeexpr_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | typeexpr_primitive prim1, typeexpr_primitive prim2 =>
          if primtype.eq_dec prim1 prim2 then left _ _ else right _ _
        | typeexpr_id id1, typeexpr_id id2 =>
          if idexpr_eq_dec id1 id2 then left _ _ else right _ _
        | typeexpr_const type1, typeexpr_const type2 =>
          if typeexpr_eq_dec type1 type2 then left _ _ else right _ _
        | typeexpr_volatile type1, typeexpr_volatile type2 =>
          if typeexpr_eq_dec type1 type2 then left _ _ else right _ _
        | typeexpr_pointer type1, typeexpr_pointer type2 =>
          if typeexpr_eq_dec type1 type2 then left _ _ else right _ _
        | typeexpr_reference type1, typeexpr_reference type2 =>
          if typeexpr_eq_dec type1 type2 then left _ _ else right _ _
        | typeexpr_rvaluereference type1, typeexpr_rvaluereference type2 =>
          if typeexpr_eq_dec type1 type2 then left _ _ else right _ _
        | typeexpr_array type1 size1, typeexpr_array type2 size2 =>
          if typeexpr_eq_dec type1 type2 then 
            if expr_eq_dec size1 size2 then left _ _ else right _ _
          else right _ _
        | typeexpr_unspec_array type1, typeexpr_unspec_array type2 =>
          if typeexpr_eq_dec type1 type2 then left _ _ else right _ _
        | typeexpr_function type1, typeexpr_function type2 =>
          if funtypeexpr_eq_dec type1 type2 then left _ _ else right _ _
        | typeexpr_decltype expr1, typeexpr_decltype expr2 =>
          if expr_eq_dec expr1 expr2 then left _ _ else right _ _
        | _, _ => right _ _
      end
    with funtypeexpr_eq_dec (x y : funtypeexpr_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | funtypeexpr_make ret_type1 args1 q1 postfixform1,
          funtypeexpr_make ret_type2 args2 q2 postfixform2 =>
          if typeexpr_eq_dec ret_type1 ret_type2 then
            if funargs_eq_dec args1 args2 then
              if fnqual.eq_dec q1 q2 then
                if bool_eq_dec postfixform1 postfixform2 then left _ _ else right _ _
              else right _ _
            else right _ _
          else right _ _
      end
    with funargs_eq_dec (x y : funargs_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | funargs_nil, funargs_nil => left _ _
        | funargs_cons arg1 args1, funargs_cons arg2 args2 =>
          if funarg_eq_dec arg1 arg2 then
            if funargs_eq_dec args1 args2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with funarg_eq_dec (x y : funarg_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | funarg_named type1 name1, funarg_named type2 name2 =>
          if typeexpr_eq_dec type1 type2 then
            if string_dec name1 name2 then left _ _ else right _ _
          else right _ _
        | funarg_anon type1, funarg_anon type2 =>
          if typeexpr_eq_dec type1 type2 then left _ _ else right _ _
        | _, _ => right _ _
      end
    with stmts_eq_dec (x y : stmts_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | stmts_nil, stmts_nil => left _ _
        | stmts_cons stmt1 stmts1, stmts_cons stmt2 stmts2 =>
          if stmt_eq_dec stmt1 stmt2 then
            if stmts_eq_dec stmts1 stmts2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with stmt_eq_dec (x y : stmt_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | stmt_decl decl1, stmt_decl decl2 =>
          if decl_eq_dec decl1 decl2 then left _ _ else right _ _
        | stmt_expr expr1, stmt_expr expr2 =>
          if expr_eq_dec expr1 expr2 then left _ _ else right _ _
        | stmt_block body1, stmt_block body2 =>
          if stmts_eq_dec body1 body2 then left _ _ else right _ _
        | stmt_if cond1 then_body1, stmt_if cond2 then_body2 =>
          if condition_eq_dec cond1 cond2 then
            if stmt_eq_dec then_body1 then_body2 then left _ _ else right _ _
          else right _ _
        | stmt_ifelse cond1 then_body1 else_body1, stmt_ifelse cond2 then_body2 else_body2 =>
          if condition_eq_dec cond1 cond2 then
            if stmt_eq_dec then_body1 then_body2 then
              if stmt_eq_dec else_body1 else_body2 then left _ _ else right _ _
            else right _ _
          else right _ _
        | stmt_while cond1 body1, stmt_while cond2 body2 =>
          if condition_eq_dec cond1 cond2 then
            if stmt_eq_dec body1 body2 then left _ _ else right _ _
          else right _ _
        | stmt_do_while body1 cond1, stmt_do_while body2 cond2 =>
          if stmt_eq_dec body1 body2 then
            if condition_eq_dec cond1 cond2 then left _ _ else right _ _
          else right _ _
        | stmt_for init1 cond1 cont1 body1, stmt_for init2 cond2 cont2 body2 =>
          if condition_eq_dec init1 init2 then
            if condition_eq_dec cond1 cond2 then
              if expr_eq_dec cont1 cont2 then
                if stmt_eq_dec body1 body2 then left _ _ else right _ _
              else right _ _
            else right _ _
          else right _ _
        | stmt_for_range type1 id1 range1 body1, stmt_for_range type2 id2 range2 body2 =>
          if typeexpr_eq_dec type1 type2 then
            if idexpr_eq_dec id1 id2 then
              if expr_eq_dec range1 range2 then
                if stmt_eq_dec body1 body2 then left _ _ else right _ _
              else right _ _
            else right _ _
          else right _ _
        | stmt_return expr1, stmt_return expr2 =>
          if expr_eq_dec expr1 expr2 then left _ _ else right _ _
        | _, _ => right _ _
      end
    with condition_eq_dec (x y : condition_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | condition_expr e1, condition_expr e2 =>
          if expr_eq_dec e1 e2 then left _ _ else right _ _
        | condition_decl ds1 type1 id1 attrs1 value1, condition_decl ds2 type2 id2 attrs2 value2 =>
          if declspec.eq_dec ds1 ds2 then
            if typeexpr_eq_dec type1 type2 then
              if idexpr_eq_dec id1 id2 then
                if attrspec.eq_dec attrs1 attrs2 then
                  if expr_eq_dec value1 value2 then left _ _ else right _ _
                else right _ _
              else right _ _
            else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with expr_eq_dec (x y : expr_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | expr_id id1, expr_id id2 =>
          if idexpr_eq_dec id1 id2 then left _ _ else right _ _
        | expr_literal value1, expr_literal value2 =>
          if literal.eq_dec value1 value2 then left _ _ else right _ _
        | expr_unop op1 arg1, expr_unop op2 arg2 =>
          if unop.eq_dec op1 op2 then 
            if expr_eq_dec arg1 arg2 then left _ _ else right _ _
          else right _ _
        | expr_binop op1 arg11 arg21, expr_binop op2 arg12 arg22 =>
          if binop.eq_dec op1 op2 then 
            if expr_eq_dec arg11 arg12 then 
              if expr_eq_dec arg21 arg22 then left _ _ else right _ _
            else right _ _
          else right _ _
        | expr_ternop arg11 arg21 arg31, expr_ternop arg12 arg22 arg32 =>
          if expr_eq_dec arg11 arg12 then 
            if expr_eq_dec arg21 arg22 then 
              if expr_eq_dec arg31 arg32 then left _ _ else right _ _
            else right _ _
          else right _ _
        | expr_call fn1 args1, expr_call fn2 args2 =>
          if expr_eq_dec fn1 fn2 then 
            if callargs_eq_dec args1 args2 then left _ _ else right _ _
          else right _ _
        | expr_lambda binders1 args1 body1, expr_lambda binders2 args2 body2 =>
          if binders_eq_dec binders1 binders2 then
            if funargs_eq_dec args1 args2 then
              if stmts_eq_dec body1 body2 then left _ _ else right _ _
            else right _ _
          else right _ _
        | expr_lambda_rettype binders1 args1 rettype1 body1, expr_lambda_rettype binders2 args2 rettype2 body2 =>
          if binders_eq_dec binders1 binders2 then
            if funargs_eq_dec args1 args2 then
              if typeexpr_eq_dec rettype1 rettype2 then
                if stmts_eq_dec body1 body2 then left _ _ else right _ _
              else right _ _
            else right _ _
          else right _ _
        | expr_dynamic_cast target_type1 expr1, expr_dynamic_cast target_type2 expr2 =>
          if typeexpr_eq_dec target_type1 target_type2 then 
            if expr_eq_dec expr1 expr2 then left _ _ else right _ _
          else right _ _
        | expr_memdot structure1 member1, expr_memdot structure2 member2 =>
          if expr_eq_dec structure1 structure2 then
            if idexpr_eq_dec member1 member2 then left _ _ else right _ _
          else right _ _
        | expr_memarrow structure1 member1, expr_memarrow structure2 member2 =>
          if expr_eq_dec structure1 structure2 then
            if idexpr_eq_dec member1 member2 then left _ _ else right _ _
          else right _ _
        | expr_throw expr1, expr_throw expr2 =>
          if expr_eq_dec expr1 expr2 then left _ _ else right _ _
        | _, _ => right _ _
      end
    with callargs_eq_dec (x y : callargs_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | callargs_nil, callargs_nil => left _ _
        | callargs_cons arg1 args1, callargs_cons arg2 args2 =>
          if expr_eq_dec arg1 arg2 then 
            if callargs_eq_dec args1 args2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with binders_eq_dec (x y : binders_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | binders_nil, binders_nil => left _ _
        | binders_cons arg1 args1, binders_cons arg2 args2 =>
          if binder_eq_dec arg1 arg2 then 
            if binders_eq_dec args1 args2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    with binder_eq_dec (x y : binder_t) {struct x} : {x=y} + {x<>y} :=
      match x, y with
        | binder_allcopy, binder_allcopy => left _ _
        | binder_allref, binder_allref => left _ _
        | binder_copy id1, binder_copy id2 =>
          if string_dec id1 id2 then left _ _ else right _ _
        | binder_ref id1, binder_ref id2 =>
          if string_dec id1 id2 then left _ _ else right _ _
        | binder_generic id1 expr1, binder_generic id2 expr2 =>
          if string_dec id1 id2 then 
            if expr_eq_dec expr1 expr2 then left _ _ else right _ _
          else right _ _
        | _, _ => right _ _
      end
    for typeexpr_eq_dec
    ) x y) ; try congruence.
Defined.

Definition funbody_is_stmts (this : funbody_t) : bool :=
  match this with
    | funbody_stmts _ _ => true
    | _ => false
  end.

Definition tplformarg_to_tplarg (this : tplformarg_t) : tplarg_t :=
  match this with
    | tplformarg_typename id =>
      tplarg_type (typeexpr_id (idexpr_id scope_none id))
    | tplformarg_value type id =>
      tplarg_expr (expr_id (idexpr_id scope_none id))
  end.

Fixpoint tplformargs_to_tplargs (this : tplformargs_t ) : tplargs_t :=
  match this with
    | tplformargs_nil => tplargs_nil
    | tplformargs_cons arg args =>
      tplargs_cons (tplformarg_to_tplarg arg) (tplformargs_to_tplargs args)
  end.

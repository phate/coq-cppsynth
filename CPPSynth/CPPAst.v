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

Instance decls_list : ListClass decl_t decls_t :=
{|
  to_list := decls_t_rec (fun _ => list decl_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => decls_t) decls_nil (fun n l nl => decls_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

Instance clsdecls_list : ListClass clsdecl_t clsdecls_t :=
{|
  to_list := clsdecls_t_rec (fun _ => list clsdecl_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => clsdecls_t) clsdecls_nil (fun n l nl => clsdecls_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

Instance cinits_list : ListClass cinit_t cinits_t :=
{|
  to_list := cinits_t_rec (fun _ => list cinit_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => cinits_t) cinits_nil (fun n l nl => cinits_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

Instance clsinherits_list : ListClass clsinherit_t clsinherits_t :=
{|
  to_list := clsinherits_t_rec (fun _ => list clsinherit_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => clsinherits_t) clsinherits_nil (fun n l nl => clsinherits_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

Instance tplformargs_list : ListClass tplformarg_t tplformargs_t :=
{|
  to_list := tplformargs_t_rec (fun _ => list tplformarg_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => tplformargs_t) tplformargs_nil (fun n l nl => tplformargs_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

Instance tplargs_list : ListClass tplarg_t tplargs_t :=
{|
  to_list := tplargs_t_rec (fun _ => list tplarg_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => tplargs_t) tplargs_nil (fun n l nl => tplargs_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

Instance funargs_list : ListClass funarg_t funargs_t :=
{|
  to_list := funargs_t_rec (fun _ => list funarg_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => funargs_t) funargs_nil (fun n l nl => funargs_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

Instance stmts_list : ListClass stmt_t stmts_t :=
{|
  to_list := stmts_t_rec (fun _ => list stmt_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => stmts_t) stmts_nil (fun n l nl => stmts_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

Instance callargs_list : ListClass expr_t callargs_t :=
{|
  to_list := callargs_t_rec (fun _ => list expr_t) nil (fun n nl l => cons n l) ;
  from_list := list_rec (fun _ => callargs_t) callargs_nil (fun n l nl => callargs_cons n nl)
|}.
Proof.
  induction l; simpl ; congruence.
  induction l; simpl ; congruence.
Defined.

Instance binders_list : ListClass binder_t binders_t :=
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

(* template actual arguments *)

Definition simple_id_serialize (id : string) : list token.t :=
  (token.identifier id) :: nil.

(* determine whether residual typeexpr is supposed to be the "head"
part of a declaration that is stated only once at beginning of a declarator
block -- e.g. for "declare x and y as pointer to constant of char"
we need to write: "const char *x, *y;" with "constant of char" being the
head portion *)
Fixpoint typeexpr_is_head (this : typeexpr_t) : bool :=
  match this with
    | typeexpr_primitive _ => true
    | typeexpr_id _ => true
    | typeexpr_const t => typeexpr_is_head t
    | typeexpr_volatile t => typeexpr_is_head t
    | _ => false
  end.

Module typeexpr_prec.
  Inductive t : Set :=
    | none : t
    | left_assoc : t
    | right_assoc : t
    | function : t.

  Definition need_wrap (inner_prec : t) (outer_prec : t) : bool :=
    match inner_prec, outer_prec with
      | none, _ => false
      | left_assoc, left_assoc => false
      | left_assoc, _ => true
      | right_assoc, none => false
      | right_assoc, right_assoc => false
      | right_assoc, _ => true
      | function, _ => false
    end.

  Definition maybe_wrap (inner_prec : t) (outer_prec : t) (tokens : list token.t) : list token.t :=
    if need_wrap inner_prec outer_prec
    then wrap_parens tokens
    else tokens.
End typeexpr_prec.

Module expr_prec.
  (* placeholders, not correct C++ precedences *)
  Inductive t : Set :=
    | none : t
    | unop : t
    | binop : t
    | member_l : t
    | assignment : t
  .

  Definition need_wrap (inner_prec : t) (outer_prec : t) : bool :=
    false.

  Definition maybe_wrap (inner_prec : t) (outer_prec : t) (tokens : list token.t) : list token.t :=
    if need_wrap inner_prec outer_prec
    then wrap_parens tokens
    else tokens.
End expr_prec.

Fixpoint decls_serialize (this : decls_t) : list token.t :=
  match this with
    | decls_nil => nil
    | decls_cons decl decls =>
      decl_serialize decl ++ decls_serialize decls
  end

with decl_serialize (this : decl_t) : list token.t :=
  match this with
    | decl_simple ds type id attrs =>
      declspec.to_tokens ds ++
      typeexpr_serialize (idexpr_serialize id) typeexpr_prec.none false type ++
      attrspec.to_tokens attrs ++
      ((token.symbol token_symbol.semicolon) :: nil)
    | decl_initdef ds type id attrs value =>
      declspec.to_tokens ds ++
      typeexpr_serialize (idexpr_serialize id) typeexpr_prec.none false type ++
      attrspec.to_tokens attrs ++
      ((token.symbol token_symbol.assign) :: nil) ++
      expr_serialize expr_prec.none value ++
      ((token.symbol token_symbol.semicolon) :: nil)
    | decl_fundef ds type id attrs body =>
      declspec.to_tokens ds ++
      funtypeexpr_serialize (idexpr_serialize id) typeexpr_prec.none false type ++
      attrspec.to_tokens attrs ++
      funbody_serialize body
    | decl_consdesdecl ds id args qual attrs =>
      declspec.to_tokens ds ++
      idexpr_serialize id ++
      attrspec.to_tokens attrs ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      funargs_serialize true args ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
      fnqual.to_tokens qual ++
      attrspec.to_tokens attrs ++
      ((token.symbol token_symbol.semicolon) :: nil)
    | decl_consdesdef ds id args qual attrs body =>
      declspec.to_tokens ds ++
      idexpr_serialize id ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      funargs_serialize true args ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
      fnqual.to_tokens qual ++
      attrspec.to_tokens attrs ++
      funbody_serialize body
    | decl_class_fwd id =>
      ((token.keyword token_keyword.kw_class) :: nil) ++
      idexpr_serialize id ++
      ((token.symbol token_symbol.semicolon) :: nil)
    | decl_class id is_final inherits body =>
      ((token.keyword token_keyword.kw_class) :: nil) ++
      idexpr_serialize id ++
      (if is_final then
        (token.keyword token_keyword.kw_final) :: nil
      else nil) ++
      clsinherits_serialize true inherits ++
      ((token.symbol token_symbol.open_brace) :: nil) ++
      clsdecls_serialize body ++
      ((token.symbol token_symbol.close_brace) :: nil) ++
      ((token.symbol token_symbol.semicolon) :: nil)
    | decl_templated args decl =>
      ((token.keyword token_keyword.kw_template) :: nil) ++
      ((token.symbol token_symbol.lt) :: nil) ++
      tplformargs_serialize true args ++
      ((token.symbol token_symbol.gt) :: nil) ++
      decl_serialize decl
  end

with clsdecls_serialize (this : clsdecls_t) : list token.t :=
  match this with
    | clsdecls_nil => nil
    | clsdecls_cons clsdecl clsdecls=>
      clsdecl_serialize clsdecl ++
      clsdecls_serialize clsdecls
  end

with clsdecl_serialize (this : clsdecl_t) : list token.t :=
  match this with
    | clsdecl_group visibility decls =>
      visibility_spec.to_tokens visibility ++
      ((token.symbol token_symbol.colon) :: nil) ++
      decls_serialize decls
  end

with clsinherits_serialize (first : bool) (this : clsinherits_t) : list token.t :=
  match this with
    | clsinherits_nil => nil
    | clsinherits_cons inherit inherits =>
      (if first then
        (token.symbol token_symbol.colon) :: nil
      else
        (token.symbol token_symbol.comma) :: nil) ++
      clsinherit_serialize inherit ++
      clsinherits_serialize false inherits
  end

with clsinherit_serialize (this : clsinherit_t) : list token.t :=
  match this with
    | clsinherit_single is_virtual visibility base =>
      (if is_virtual then
        (token.keyword token_keyword.kw_virtual) :: nil
      else
        nil) ++
      visibility_spec.to_tokens visibility ++
      idexpr_serialize base
  end

with funbody_serialize (this : funbody_t) : list token.t :=
  match this with
    | funbody_abstract =>
      token.symbol token_symbol.assign ::
      token.literal (literal.decimal 0) ::
      token.symbol token_symbol.semicolon ::
      nil
    | funbody_default =>
      token.symbol token_symbol.assign ::
      token.keyword token_keyword.kw_default ::
      token.symbol token_symbol.semicolon ::
      nil
    | funbody_delete =>
      token.symbol token_symbol.assign ::
      token.keyword token_keyword.kw_delete ::
      token.symbol token_symbol.semicolon ::
      nil
    | funbody_stmts init stmts =>
      cinits_serialize true init ++
      token.symbol token_symbol.open_brace :: nil ++
      stmts_serialize stmts ++
      token.symbol token_symbol.close_brace :: nil
  end

with cinits_serialize (first : bool) (this : cinits_t) : list token.t :=
  match this with
    | cinits_nil => nil
    | cinits_cons init inits =>
      (
        if first then
          (token.symbol token_symbol.colon)
        else
          (token.symbol token_symbol.comma)
      ) :: nil ++
      cinit_serialize init ++
      cinits_serialize false inits
  end

with cinit_serialize (this : cinit_t) : list token.t :=
  let (id, expr) := this in
  idexpr_serialize id ++
  (token.symbol token_symbol.open_paren) :: nil ++
  expr_serialize expr_prec.none expr ++
  (token.symbol token_symbol.close_paren) :: nil

with idexpr_serialize (this : idexpr_t) : list token.t :=
  match this with
    | idexpr_id scope id =>
      scope_serialize scope ++
      simple_id_serialize id
    | idexpr_destructor scope id =>
      scope_serialize scope ++
      token.symbol token_symbol.tilda ::
      simple_id_serialize id
    | idexpr_template scope tplid =>
      scope_serialize scope ++
      templateid_serialize tplid
    | idexpr_operator scope op =>
      scope_serialize scope ++
      token.keyword token_keyword.kw_operator ::
      (genop.to_tokens op)
  end

with scope_serialize (this : scope_t) : list token.t :=
  match this with
    | scope_none => nil
    | scope_id id sub =>
      simple_id_serialize id ++
      ((token.symbol token_symbol.scope) :: nil) ++
      scope_serialize sub
    | scope_template tplid sub =>
      templateid_serialize tplid ++
      ((token.symbol token_symbol.scope) :: nil) ++
      scope_serialize sub
  end

with templateid_serialize (this : templateid_t) : list token.t :=
  match this with
    | templateid_make name args =>
      simple_id_serialize name ++
      ((token.symbol token_symbol.lt) :: nil) ++
      tplargs_serialize true args ++
      ((token.symbol token_symbol.gt) :: nil)
  end

with tplformargs_serialize (first : bool) (this : tplformargs_t) : list token.t :=
  match this with
    | tplformargs_nil => nil
    | tplformargs_cons arg args =>
      (if first then nil else ((token.symbol token_symbol.comma) :: nil)) ++
      tplformarg_serialize arg ++ tplformargs_serialize false args
  end

with tplformarg_serialize (this : tplformarg_t) : list token.t :=
  match this with
    | tplformarg_typename name => token.keyword token_keyword.kw_typename :: token.identifier name :: nil
    | tplformarg_value type name =>
      let inner := token.identifier name :: nil in
      typeexpr_serialize inner typeexpr_prec.none false type
  end

with tplargs_serialize (first : bool) (this : tplargs_t) : list token.t :=
  match this with
    | tplargs_nil => nil
    | tplargs_cons arg args =>
      (if first then nil else ((token.symbol token_symbol.comma) :: nil)) ++
      tplarg_serialize arg ++ tplargs_serialize false args
  end

with tplarg_serialize (this : tplarg_t) : list token.t :=
  match this with
    | tplarg_type type =>
      typeexpr_serialize nil typeexpr_prec.none false type
    | tplarg_expr expr =>
      expr_serialize expr_prec.none expr
  end

with typeexpr_serialize
    (inner : list token.t)  (inner_prec : typeexpr_prec.t)
    (skip_head : bool) (this : typeexpr_t) : list token.t :=
  match this with
    | typeexpr_primitive prim =>
      if skip_head
      then inner
      else primtype.to_tokens prim ++ inner
    | typeexpr_id id =>
      if skip_head
      then inner
      else idexpr_serialize id ++ inner
    | typeexpr_const type =>
      if typeexpr_is_head type
      then
        if skip_head
        then inner
        else
          ((token.keyword token_keyword.kw_const) :: nil) ++
          typeexpr_serialize inner typeexpr_prec.none skip_head type
      else
        let inner := ((token.keyword token_keyword.kw_const) :: nil) ++ inner in
        typeexpr_serialize inner typeexpr_prec.left_assoc skip_head type
    | typeexpr_volatile type =>
      if typeexpr_is_head type
      then
        if skip_head
        then inner
        else
          ((token.keyword token_keyword.kw_volatile) :: nil) ++
          typeexpr_serialize inner typeexpr_prec.left_assoc skip_head type
      else
        let inner := ((token.keyword token_keyword.kw_volatile) :: nil) ++ inner in
        typeexpr_serialize inner typeexpr_prec.none skip_head type
    | typeexpr_pointer type =>
      let inner := ((token.symbol token_symbol.star) :: nil) ++ inner in
      typeexpr_serialize inner typeexpr_prec.left_assoc skip_head type
    | typeexpr_reference type =>
      let inner := ((token.symbol token_symbol.amp) :: nil) ++ inner in
      typeexpr_serialize inner typeexpr_prec.left_assoc skip_head type
    | typeexpr_rvaluereference type =>
      let inner := ((token.symbol token_symbol.double_amp) :: nil) ++ inner in
      typeexpr_serialize inner typeexpr_prec.left_assoc skip_head type
    | typeexpr_array type expr =>
      let inner := typeexpr_prec.maybe_wrap inner_prec typeexpr_prec.right_assoc inner in
      let inner :=
        inner ++
        ((token.symbol token_symbol.open_bracket) :: nil) ++
        expr_serialize expr_prec.none expr ++
        ((token.symbol token_symbol.close_bracket) :: nil) in
      typeexpr_serialize inner typeexpr_prec.right_assoc skip_head type
    | typeexpr_unspec_array type =>
      let inner := typeexpr_prec.maybe_wrap inner_prec typeexpr_prec.right_assoc inner in
      let inner :=
        inner ++
        ((token.symbol token_symbol.open_bracket) :: nil) ++
        ((token.symbol token_symbol.close_bracket) :: nil) in
      typeexpr_serialize inner typeexpr_prec.right_assoc skip_head type
    | typeexpr_function type =>
      funtypeexpr_serialize inner inner_prec skip_head type
    | typeexpr_decltype expr =>
      ((token.keyword token_keyword.kw_decltype) :: (token.symbol token_symbol.open_paren) :: nil) ++
      expr_serialize expr_prec.none expr ++ ((token.symbol token_symbol.close_paren) :: nil)
  end

with funtypeexpr_serialize
    (inner : list token.t)  (inner_prec : typeexpr_prec.t)
    (skip_head : bool) (this : funtypeexpr_t) : list token.t :=
  match this with
    | funtypeexpr_make ret_type args qual postfix =>
      if postfix then
        let inner := typeexpr_prec.maybe_wrap inner_prec typeexpr_prec.function inner in
        let inner :=
          inner ++
          ((token.symbol token_symbol.open_paren) :: nil) ++
          funargs_serialize true args ++
          ((token.symbol token_symbol.close_paren) :: nil) in
        let inner :=
          ((token.keyword token_keyword.kw_auto) :: nil) ++ inner in
        let post :=
          typeexpr_serialize nil typeexpr_prec.none false ret_type in
        inner ++
        fnqual.to_tokens qual ++
        ((token.symbol token_symbol.arrow) :: nil) ++
        post
      else
        let inner := typeexpr_prec.maybe_wrap inner_prec typeexpr_prec.function inner in
        let inner :=
          inner ++
          ((token.symbol token_symbol.open_paren) :: nil) ++
          funargs_serialize true args ++
          ((token.symbol token_symbol.close_paren) :: nil) ++
          fnqual.to_tokens qual in
        typeexpr_serialize inner typeexpr_prec.function skip_head ret_type
  end

with funargs_serialize
    (first : bool) (this : funargs_t) : list token.t :=
  match this with
    | funargs_nil => nil
    | funargs_cons arg args =>
      (if first then nil else ((token.symbol token_symbol.comma) :: nil)) ++
      funarg_serialize arg ++
      funargs_serialize false args
  end

with funarg_serialize
    (this : funarg_t) : list token.t :=
  match this with
    | funarg_named type id =>
      typeexpr_serialize (simple_id_serialize id) typeexpr_prec.none false type
    | funarg_anon type =>
      typeexpr_serialize nil typeexpr_prec.none false type
  end

with stmts_serialize
    (this : stmts_t) : list token.t :=
  match this with
    | stmts_nil => nil
    | stmts_cons stmt stmts =>
      stmt_serialize stmt ++
      stmts_serialize stmts
  end

with stmt_serialize (this : stmt_t) : list token.t :=
  match this with
    | stmt_decl decl => decl_serialize decl
    | stmt_expr expr => expr_serialize expr_prec.none expr ++ ((token.symbol token_symbol.semicolon) :: nil)
    | stmt_block body =>
      ((token.symbol token_symbol.open_brace) :: nil) ++
      stmts_serialize body ++
      ((token.symbol token_symbol.close_brace) :: nil)
    | stmt_if cond then_body =>
      ((token.keyword token_keyword.kw_if) :: nil) ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      condition_serialize cond ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
      stmt_serialize then_body
    | stmt_ifelse cond then_body else_body =>
      ((token.keyword token_keyword.kw_if) :: nil) ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      condition_serialize cond ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
      stmt_serialize then_body ++
      ((token.keyword token_keyword.kw_else) :: nil) ++
      stmt_serialize else_body
    | stmt_while cond body =>
      ((token.keyword token_keyword.kw_while) :: nil) ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      condition_serialize cond ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
      stmt_serialize body
    | stmt_do_while body cond =>
      ((token.keyword token_keyword.kw_do) :: nil) ++
      stmt_serialize body ++
      ((token.keyword token_keyword.kw_while) :: nil) ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      condition_serialize cond ++
      ((token.symbol token_symbol.close_paren) :: nil)
    | stmt_for init cond cont body =>
      ((token.keyword token_keyword.kw_for) :: nil) ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      condition_serialize cond ++
      ((token.symbol token_symbol.semicolon) :: nil) ++
      condition_serialize cond ++
      ((token.symbol token_symbol.semicolon) :: nil) ++
      expr_serialize expr_prec.none cont ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
      stmt_serialize body
    | stmt_for_range type id range body =>
      ((token.keyword token_keyword.kw_for) :: nil) ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      typeexpr_serialize
        (idexpr_serialize id)
        typeexpr_prec.none false
        type ++
      ((token.symbol token_symbol.colon) :: nil) ++
      expr_serialize expr_prec.none range ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
      stmt_serialize body
    | stmt_return expr =>
      ((token.keyword token_keyword.kw_return) :: nil) ++
      expr_serialize expr_prec.none expr ++
      ((token.symbol token_symbol.semicolon) :: nil)
  end

with condition_serialize (this : condition_t) : list token.t :=
  match this with
    | condition_expr expr => expr_serialize expr_prec.none expr
    | condition_decl ds type id attrs value =>
      (* this is a copy of decl_initdef serialization *)
      declspec.to_tokens ds ++
      typeexpr_serialize (idexpr_serialize id) typeexpr_prec.none false type ++
      attrspec.to_tokens attrs ++
      ((token.symbol token_symbol.assign) :: nil) ++
      expr_serialize expr_prec.none value
  end

with expr_serialize (outer_prec : expr_prec.t) (this : expr_t) : list token.t :=
  match this with
    | expr_id id =>
      idexpr_serialize id
    | expr_literal literal =>
      (token.literal literal) :: nil
    | expr_unop op arg => (* XXX: pre-/postfix unary operator *)
      let e := expr_serialize expr_prec.unop arg in
      let opsyms := (token.symbol (unop.to_symbol op)) :: nil in
      if unop.is_prefix op then
        opsyms ++ e
      else
        e ++ opsyms
    | expr_binop op arg1 arg2 =>
      let e1 := expr_serialize expr_prec.binop arg1 in
      let e2 := expr_serialize expr_prec.binop arg2 in
      e1 ++
      (token.symbol (binop.to_symbol op)) :: nil ++
      e2
    | expr_ternop cond then_expr else_expr =>
      expr_serialize expr_prec.binop cond ++
      ((token.symbol token_symbol.question) :: nil) ++
      expr_serialize expr_prec.binop then_expr ++
      ((token.symbol token_symbol.colon) :: nil) ++
      expr_serialize expr_prec.binop else_expr
    | expr_call fn args =>
      expr_serialize expr_prec.binop fn ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      callargs_serialize true args ++
      ((token.symbol token_symbol.close_paren) :: nil)
    | expr_lambda binders args body =>
      ((token.symbol token_symbol.open_bracket) :: nil) ++
      binders_serialize true binders ++
      ((token.symbol token_symbol.close_bracket) :: nil) ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      funargs_serialize true args ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
      ((token.symbol token_symbol.open_brace) :: nil) ++
      stmts_serialize body ++
      ((token.symbol token_symbol.close_brace) :: nil)
    | expr_lambda_rettype binders args rettype body =>
      ((token.symbol token_symbol.open_bracket) :: nil) ++
      binders_serialize true binders ++
      ((token.symbol token_symbol.close_bracket) :: nil) ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      funargs_serialize true args ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
      ((token.symbol token_symbol.arrow) :: nil) ++
      typeexpr_serialize nil typeexpr_prec.none false rettype ++
      ((token.symbol token_symbol.open_brace) :: nil) ++
      stmts_serialize body ++
      ((token.symbol token_symbol.close_brace) :: nil)
    | expr_dynamic_cast target_type value =>
      ((token.keyword token_keyword.kw_dynamic_cast) :: nil) ++
      ((token.symbol token_symbol.lt) :: nil) ++
      typeexpr_serialize nil typeexpr_prec.none false target_type ++
      ((token.symbol token_symbol.gt) :: nil) ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      expr_serialize expr_prec.none value ++
      ((token.symbol token_symbol.close_paren) :: nil)
    | expr_memdot structure member =>
      expr_serialize expr_prec.member_l structure ++
      ((token.symbol token_symbol.dot) :: nil) ++
      idexpr_serialize member
    | expr_memarrow structure member =>
      expr_serialize expr_prec.member_l structure ++
      ((token.symbol token_symbol.arrow) :: nil) ++
      idexpr_serialize member
    | expr_throw expr =>
      ((token.keyword token_keyword.kw_throw) :: nil) ++
      expr_serialize expr_prec.assignment expr
  end

with callargs_serialize (first : bool) (this : callargs_t) : list token.t :=
  match this with
    | callargs_nil => nil
    | callargs_cons arg args =>
      (if first then nil else ((token.symbol token_symbol.comma) :: nil)) ++
      expr_serialize expr_prec.none arg ++  (* XXX: comma precedence *)
      callargs_serialize false args
  end

with binders_serialize (first : bool) (this : binders_t) : list token.t :=
  match this with
    | binders_nil => nil
    | binders_cons binder binders =>
      (if first then nil else ((token.symbol token_symbol.comma) :: nil)) ++
      binder_serialize binder ++
      binders_serialize false binders
  end

with binder_serialize (this : binder_t) : list token.t :=
  match this with
    | binder_allcopy => token.symbol token_symbol.eq :: nil
    | binder_allref => token.symbol token_symbol.amp :: nil
    | binder_copy id => token.identifier id :: nil
    | binder_ref id => token.symbol token_symbol.amp :: token.identifier id :: nil
    | binder_generic id expr =>
      token.identifier id :: token.symbol token_symbol.open_brace :: nil ++
      expr_serialize expr_prec.none expr ++
      token.symbol token_symbol.close_brace :: nil
  end.

(*
Fixpoint make_stmts (stmts : list stmt_t) : stmts_t :=
  match stmts with
    | nil => stmts_nil
    | cons stmt stmts => stmts_cons stmt (make_stmts stmts)
  end.

Fixpoint make_funargs (args : list funarg_t) : funargs_t :=
  match args with
    | nil => funargs_nil
    | cons arg args => funargs_cons arg (make_funargs args)
  end.

Fixpoint make_decls (decls : list decl_t) : decls_t :=
  match decls with
    | nil => decls_nil
    | cons decl decls => decls_cons decl (make_decls decls)
  end.

Fixpoint make_clsdecls (clsdecls : list clsdecl_t) : clsdecls_t :=
  match clsdecls with
    | nil => clsdecls_nil
    | cons clsdecl clsdecls => clsdecls_cons clsdecl (make_clsdecls clsdecls)
  end.

Fixpoint make_callargs (args : list expr_t) : callargs_t :=
  match args with
    | nil => callargs_nil
    | cons arg args => callargs_cons arg (make_callargs args)
  end.*)

Example ex_decl_array_of_pointers :=
    decl_simple
      declspec.none
      (typeexpr_array (typeexpr_pointer (typeexpr_primitive primtype.int)) (expr_literal (literal.decimal 32)))
      (idexpr_id scope_none "foo")
      attrspec.none.
Eval lazy in (serialize_tokens (decl_serialize ex_decl_array_of_pointers)).
Example ex_decl_pointer_of_array :=
    decl_simple
      declspec.none
      (typeexpr_pointer (typeexpr_array (typeexpr_primitive primtype.int) (expr_literal (literal.decimal 32))))
      (idexpr_id scope_none "foo")
      attrspec.none.
Eval lazy in (serialize_tokens (decl_serialize ex_decl_pointer_of_array)).
Example ex_decl_fun :=
    decl_simple
      declspec.none
      (typeexpr_function
        (funtypeexpr_make
          (typeexpr_pointer (typeexpr_primitive primtype.int))
          (from_list (funarg_named (typeexpr_primitive primtype.int) "a" :: funarg_named (typeexpr_primitive primtype.char) "b" :: nil))
          fnqual.none
          false))
      (idexpr_id scope_none "foo")
      attrspec.none.
Eval lazy in (serialize_tokens (decl_serialize ex_decl_fun)).
Example ex_decl_class :=
    decl_class
      (idexpr_id scope_none "foo") false clsinherits_nil
      (from_list (
        (clsdecl_group
          visibility_spec.vis_public
          (from_list
            (ex_decl_fun ::
            nil)) ::
        nil))).
Eval lazy in (serialize_tokens (decl_serialize ex_decl_class)).
Example ex_decl_tpl_class :=
    decl_templated
      (tplformargs_cons (tplformarg_typename "T") tplformargs_nil)
      ex_decl_class.
Eval lazy in (serialize_tokens (decl_serialize ex_decl_tpl_class)).
Example ex_decl_funptr :=
    decl_simple
      declspec.none
      (typeexpr_pointer (
        (typeexpr_function
          (funtypeexpr_make
            (typeexpr_pointer (typeexpr_primitive primtype.int))
            (funargs_nil)
            fnqual.none
            false))))
      (idexpr_id scope_none "foo")
      attrspec.none.
Eval lazy in (serialize_tokens (decl_serialize ex_decl_funptr)).

Example ex_decl_fundef :=
  decl_fundef
    declspec.none
    (funtypeexpr_make
      (typeexpr_primitive primtype.int)
      (from_list (
        funarg_named (typeexpr_primitive primtype.int) "a" ::
        funarg_named (typeexpr_primitive primtype.char) "b" ::
        nil))
      fnqual.none
      false)
    (idexpr_id scope_none "foo")
    attrspec.none
    (funbody_stmts
      cinits_nil
      (from_list (
        (stmt_return (expr_literal (literal.decimal 32))) ::
        nil)
      )
    ).
Eval lazy in (serialize_tokens (decl_serialize ex_decl_fundef)).

Example ex_hello_world_main :=
  decl_fundef
    declspec.none
    (funtypeexpr_make
      (typeexpr_primitive primtype.int)
      (from_list (
        funarg_named (typeexpr_primitive primtype.int) "args" ::
        funarg_named (typeexpr_pointer (typeexpr_pointer (typeexpr_primitive primtype.char))) "argv" ::
        nil))
      fnqual.none
      false)
    (idexpr_id scope_none "main")
    attrspec.none
    (funbody_stmts
      cinits_nil
      (from_list (
        (stmt_expr
          (expr_call
            (expr_id (idexpr_id scope_none "printf"))
            (from_list (
              expr_literal (literal.str "Hello world!") ::
              nil)))) ::
        (stmt_return (expr_literal (literal.decimal 0))) ::
        nil)
      )
    ).
Eval lazy in (serialize_tokens (decl_serialize ex_hello_world_main)).

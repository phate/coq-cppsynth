Require Import
  List
  String
  CPPSynth.CPPAstBase
  CPPSynth.CPPAst
  CPPSynth.ListClass.

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

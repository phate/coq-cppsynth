Require Import String Ascii List.
Require CPPSynth.StringUtil.

Module literal.
  Inductive t : Set :=
    | decimal : nat -> t
    | str : string -> t.

  Definition to_string (this : t) : String.string :=
    match this with
      | decimal value => CPPSynth.StringUtil.string_of_nat value
      | str value => ("""" ++ CPPSynth.StringUtil.escape_string value ++ """")%string
    end.
End literal.

Module token_symbol.
  Inductive t : Set :=
    | assign : t
    | plus : t
    | minus : t
    | star : t
    | tilda : t
    | exclam : t
    | amp : t
    | eq : t
    | ne : t
    | lt : t
    | gt : t
    | open_paren : t
    | close_paren : t
    | open_brace : t
    | close_brace : t
    | open_bracket : t
    | close_bracket : t
    | comma : t
    | colon : t
    | semicolon : t
    | scope : t
    | question : t
  .

  Definition to_string (this : t) : String.string :=
    match this with
      | assign => "="
      | amp => "&"
      | plus => "+"
      | minus => "-"
      | star => "*"
      | tilda => "~"
      | exclam => "!"
      | eq => "=="
      | ne => "!="
      | lt => "<"
      | gt => ">"
      | open_brace => "{"
      | close_brace => "}"
      | open_bracket => "["
      | close_bracket => "]"
      | open_paren => "("
      | close_paren => ")"
      | comma => ","
      | colon => ":"
      | semicolon => ";"
      | scope => "::"
      | question => "?"
    end.
End token_symbol.

Module token_keyword.
  Inductive t : Set :=
    | kw_return : t
    | kw_template : t
    | kw_class : t
    | kw_const : t
    | kw_volatile : t
    | kw_if : t
    | kw_else : t
    | kw_for : t
    | kw_void : t
    | kw_signed : t
    | kw_unsigned : t
    | kw_auto : t
    | kw_char : t
    | kw_short : t
    | kw_int : t
    | kw_private : t
    | kw_protected : t
    | kw_public : t
    | kw_typename : t
    | kw_register : t
    | kw_static : t
    | kw_thread_local : t
    | kw_extern : t
    | kw_mutable : t
    | kw_inline : t
    | kw_virtual : t
    | kw_explicit : t
    | kw_friend : t
    | kw_constexpr : t
    | kw_override : t
    | kw_noexcept : t
  .

  Definition to_string (this : t) : String.string :=
    match this with
      | kw_return => "return"
      | kw_template => "template"
      | kw_class => "class"
      | kw_const => "const"
      | kw_volatile => "volatile"
      | kw_if => "if"
      | kw_else => "else"
      | kw_for => "for"
      | kw_void => "void"
      | kw_signed => "signed"
      | kw_unsigned => "unsigned"
      | kw_auto => "auto"
      | kw_char => "char"
      | kw_short => "short"
      | kw_int => "int"
      | kw_private => "private"
      | kw_protected => "protected"
      | kw_public => "public"
      | kw_typename => "typename"
      | kw_register => "register"
      | kw_static => "static"
      | kw_thread_local => "thread_local"
      | kw_extern => "extern"
      | kw_mutable => "mutable"
      | kw_inline => "inline"
      | kw_virtual => "virtual"
      | kw_explicit => "explicit"
      | kw_friend => "friend"
      | kw_constexpr => "constexpr"
      | kw_override => "override"
      | kw_noexcept => "noexcept"
    end.

End token_keyword.

Module token.
  Inductive t :=
    | identifier :
      forall (s : string), t
    | keyword : forall (kw : token_keyword.t), t
    | symbol : forall (sym : token_symbol.t), t
    | literal : forall (lit : literal.t), t.

  Definition to_string (this : t) : String.string :=
    match this with
      | identifier id => id
      | keyword kw => token_keyword.to_string kw
      | symbol sym => token_symbol.to_string sym
      | literal lit => literal.to_string lit
    end.

End token.

Definition wrap_parens (inner : list token.t) : list token.t :=
  ((token.symbol token_symbol.open_paren) :: nil) ++
  inner ++
  ((token.symbol token_symbol.close_paren) :: nil).

Definition serialize_tokens (tokens : list token.t) : string :=
  String.concat " " (List.map token.to_string tokens).

Module unop.
  Inductive t :=
    | negate
    | bitnot
    | logicnot
    | deref
    | addrof.

  Definition to_symbol (unop : t) : token_symbol.t :=
    match unop with
      | negate => token_symbol.minus
      | bitnot => token_symbol.tilda
      | logicnot => token_symbol.exclam
      | deref => token_symbol.star
      | addrof => token_symbol.amp
    end.
End unop.

Module binop.
  Inductive t :=
    | assign
    | plus
    | minus
    | eq
    | ne.
  Definition to_symbol (binop : t) : token_symbol.t :=
    match binop with
      | assign => token_symbol.assign
      | plus => token_symbol.plus
      | minus => token_symbol.minus
      | eq => token_symbol.eq
      | ne => token_symbol.ne
    end.
End binop.

Module primtype.
  Inductive t :=
    | void
    | auto
    | char
    | unsigned_char
    | signed_char
    | int
    | signed_int.

  Definition to_tokens (type : t) : list token.t :=
    match type with
      | void => (token.keyword token_keyword.kw_void) :: nil
      | auto => (token.keyword token_keyword.kw_auto) :: nil
      | char => (token.keyword token_keyword.kw_char) :: nil
      | unsigned_char => (token.keyword token_keyword.kw_unsigned) :: (token.keyword token_keyword.kw_char) :: nil
      | signed_char => (token.keyword token_keyword.kw_signed) :: (token.keyword token_keyword.kw_char) :: nil
      | int => (token.keyword token_keyword.kw_int) :: nil
      | unsigned_int => (token.keyword token_keyword.kw_unsigned) :: (token.keyword token_keyword.kw_int) :: nil
    end.
End primtype.

Module visibility_spec.
  Inductive t :=
    | vis_private : t
    | vis_protected : t
    | vis_public : t.

  Definition to_tokens (this : t) : list token.t :=
    match this with
      | vis_private => (token.keyword token_keyword.kw_private) :: nil
      | vis_protected => (token.keyword token_keyword.kw_protected) :: nil
      | vis_public => (token.keyword token_keyword.kw_public) :: nil
    end.
End visibility_spec.

Module decl_specifier.
  Inductive t : Set :=
    | ds_register : t
    | ds_static : t
    | ds_thread_local : t
    | ds_extern : t
    | ds_mutable : t
    | ds_inline : t
    | ds_virtual : t
    | ds_explicit : t
    | ds_friend : t
    | ds_constexpr : t
  .

  Definition to_tokens (this : t) : list token.t :=
    match this with
      | ds_register => (token.keyword token_keyword.kw_register) :: nil
      | ds_static => (token.keyword token_keyword.kw_static) :: nil
      | ds_thread_local => (token.keyword token_keyword.kw_thread_local) :: nil
      | ds_extern => (token.keyword token_keyword.kw_extern) :: nil
      | ds_mutable => (token.keyword token_keyword.kw_mutable) :: nil
      | ds_inline => (token.keyword token_keyword.kw_inline) :: nil
      | ds_virtual => (token.keyword token_keyword.kw_virtual) :: nil
      | ds_explicit => (token.keyword token_keyword.kw_explicit) :: nil
      | ds_friend => (token.keyword token_keyword.kw_friend) :: nil
      | ds_constexpr => (token.keyword token_keyword.kw_constexpr) :: nil
    end.
End decl_specifier.

Module decl_specifiers.
  Definition t := list decl_specifier.t.

  Definition to_tokens (this : t) : list token.t :=
    let result := nil in
    fold_left (fun result ds => result ++ (decl_specifier.to_tokens ds)) this result.
End decl_specifiers.

Module attr_specifier.
  Inductive t : Set :=
    | as_override : t
    | as_noexcept : t.
  Definition to_tokens (this : t) : list token.t :=
    match this with
      | as_override => (token.keyword token_keyword.kw_override) :: nil
      | as_noexcept => (token.keyword token_keyword.kw_noexcept) :: nil
    end.
End attr_specifier.

Module attr_specifiers.
  Definition t := list attr_specifier.t.

  Definition to_tokens (this : t) : list token.t :=
    let result := nil in
    fold_left (fun result ds => result ++ (attr_specifier.to_tokens ds)) this result.
End attr_specifiers.

(* declarators, namespace / class level *)
Inductive decls_t : Set :=
  | decls_nil : decls_t
  | decls_cons : decl_t -> decls_t -> decls_t

(* note: much more restrictive than what C++ allows, one declarator per block etc. *)
with decl_t : Set :=
  | decl_simple : (* covers function and variable declarations *)
    forall (ds : decl_specifiers.t) (type : typeexpr_t) (id : idexpr_t) (attrs : attr_specifiers.t), decl_t
  | decl_initdef : (* covers definition via assignment initialization *)
    forall (ds : decl_specifiers.t) (type : typeexpr_t) (id : idexpr_t) (attrs : attr_specifiers.t) (value : expr_t), decl_t
  | decl_fundef : (* strict function definitions *)
    forall (ds : decl_specifiers.t) (type : funtypeexpr_t) (id : idexpr_t) (attrs : attr_specifiers.t) (body : stmts_t), decl_t
  | decl_consdesdecl : (* constructor and destructor declarations *)
    forall (ds : decl_specifiers.t) (id : idexpr_t) (args : funargs_t) (attrs : attr_specifiers.t), decl_t
  | decl_consdesdef : (* constructor and destructor definitions *)
    forall (ds : decl_specifiers.t) (id : idexpr_t) (args : funargs_t) (attrs : attr_specifiers.t) (body : stmts_t), decl_t
  | decl_class_fwd :
    forall (id : idexpr_t), decl_t
  | decl_class :
    forall (id : idexpr_t) (body : clsdecls_t), decl_t
  | decl_templated : (* template any of the above -- should not template templates *)
    forall (args : tplformargs_t) (decl : decl_t), decl_t

with clsdecls_t : Set :=
  | clsdecls_nil : clsdecls_t
  | clsdecls_cons : clsdecl_t -> clsdecls_t -> clsdecls_t

with clsdecl_t : Set :=
  | clsdecl_group :
    forall (visibility : visibility_spec.t) (decls : decls_t), clsdecl_t

(* expression for an id -- needs to include namespace and template-ids, then *)
with idexpr_t : Set :=
  | idexpr_id : string -> idexpr_t
  | idexpr_destructor : string -> idexpr_t

(* template formal arguments *)
with tplformargs_t : Set :=
  | tplformargs_nil : tplformargs_t
  | tplformargs_cons : tplformarg_t -> tplformargs_t -> tplformargs_t

with tplformarg_t : Set :=
  | tplformarg_typename : string -> tplformarg_t
  | tplformarg_value : typeexpr_t -> string -> tplformarg_t

(* terms representing types *)
with typeexpr_t : Set :=
  | typeexpr_primitive : primtype.t -> typeexpr_t
  | typeexpr_id : idexpr_t -> typeexpr_t
  | typeexpr_const : typeexpr_t -> typeexpr_t
  | typeexpr_volatile : typeexpr_t -> typeexpr_t
  | typeexpr_pointer : typeexpr_t -> typeexpr_t
  | typeexpr_reference : typeexpr_t -> typeexpr_t
  | typeexpr_array : typeexpr_t -> expr_t -> typeexpr_t
  | typeexpr_unspec_array : typeexpr_t -> typeexpr_t
  | typeexpr_function : funtypeexpr_t -> typeexpr_t

with funtypeexpr_t : Set :=
  | funtypeexpr :
    forall (ret_type : typeexpr_t) (args : funargs_t), funtypeexpr_t

with funargs_t : Set :=
  | funargs_nil : funargs_t
  | funargs_cons : funarg_t -> funargs_t -> funargs_t

with funarg_t : Set :=
  | funarg_named :
    forall (type : typeexpr_t) (name : String.string), funarg_t
  | funarg_anon :
    forall (type : typeexpr_t), funarg_t

(* Statements (function-level) *)

with stmts_t : Set :=
  | stmts_nil : stmts_t
  | stmts_cons : stmt_t -> stmts_t -> stmts_t

with stmt_t : Set :=
  | stmt_decl :
    forall (decl : decl_t), stmt_t
  | stmt_expr :
    forall (expr : expr_t), stmt_t
  | stmt_block :
    forall (body : stmts_t), stmt_t
  | stmt_if :
    forall (cond : expr_t) (then_body : stmt_t), stmt_t
  | stmt_ifelse :
    forall (cond : expr_t) (then_body : stmt_t) (else_body : stmt_t), stmt_t
  | stmt_return :
    forall (expr : expr_t), stmt_t

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

with callargs_t : Set :=
  | callargs_nil : callargs_t
  | callargs_cons : expr_t -> callargs_t -> callargs_t

with binders_t : Set :=
  | binders_nil : binders_t
  | binders_cons : binder_t -> binders_t -> binders_t

with binder_t : Set :=
  | binder_allcopy : binder_t
  | binder_allref : binder_t
  | binder_copy : String.string -> binder_t
  | binder_ref : String.string -> binder_t
  | binder_generic : String.string -> expr_t -> binder_t.

Definition simple_id_serialize (id : string) : list token.t :=
  (token.identifier id) :: nil.

(* XXX: to be part of mutual fix when accounting for templates *)
Definition idexpr_serialize (this : idexpr_t) : list token.t :=
  match this with
    | idexpr_id id => simple_id_serialize id
    | idexpr_destructor id =>
      token.symbol token_symbol.tilda ::
      simple_id_serialize id
  end.


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
    | binop : t.

  Definition need_wrap (inner_prec : t) (outer_prec : t) : bool :=
    false.

  Definition maybe_wrap (inner_prec : t) (outer_prec : t) (tokens : list token.t) : list token.t :=
    if need_wrap inner_prec outer_prec
    then wrap_parens tokens
    else tokens.
End expr_prec.

(* XXX: placeholders *)
Fixpoint binders_serialize (this : binders_t) : list token.t := nil.

Fixpoint decls_serialize (this : decls_t) : list token.t :=
  match this with
    | decls_nil => nil
    | decls_cons decl decls =>
      decl_serialize decl ++ decls_serialize decls
  end

with decl_serialize (this : decl_t) : list token.t :=
  match this with
    | decl_simple ds type id attrs =>
      decl_specifiers.to_tokens ds ++
      typeexpr_serialize (idexpr_serialize id) typeexpr_prec.none false type ++
      attr_specifiers.to_tokens attrs ++
      ((token.symbol token_symbol.semicolon) :: nil)
    | decl_initdef ds type id attrs value =>
      decl_specifiers.to_tokens ds ++
      typeexpr_serialize (idexpr_serialize id) typeexpr_prec.none false type ++
      attr_specifiers.to_tokens attrs ++
      ((token.symbol token_symbol.assign) :: nil) ++
      expr_serialize expr_prec.none value ++
      ((token.symbol token_symbol.semicolon) :: nil)
    | decl_fundef ds type id attrs body =>
      decl_specifiers.to_tokens ds ++
      funtypeexpr_serialize (idexpr_serialize id) typeexpr_prec.none false type ++
      attr_specifiers.to_tokens attrs ++
      ((token.symbol token_symbol.open_brace) :: nil) ++
      stmts_serialize body ++
      ((token.symbol token_symbol.close_brace) :: nil)
    | decl_consdesdecl ds id args attrs =>
      decl_specifiers.to_tokens ds ++
      idexpr_serialize id ++
      attr_specifiers.to_tokens attrs ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      funargs_serialize true args ++
      ((token.symbol token_symbol.close_paren) :: nil)
    | decl_consdesdef ds id args attrs body =>
      decl_specifiers.to_tokens ds ++
      idexpr_serialize id ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      funargs_serialize true args ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
      attr_specifiers.to_tokens attrs ++
      ((token.symbol token_symbol.open_brace) :: nil) ++
      stmts_serialize body ++
      ((token.symbol token_symbol.close_brace) :: nil)
    | decl_class_fwd id =>
      ((token.keyword token_keyword.kw_class) :: nil) ++
      idexpr_serialize id ++
      ((token.symbol token_symbol.semicolon) :: nil)
    | decl_class id body =>
      ((token.keyword token_keyword.kw_class) :: nil) ++
      idexpr_serialize id ++
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

with clsdecl_serialize (this : clsdecl_t) : list token.t :=
  match this with
    | clsdecl_group visibility decls =>
      visibility_spec.to_tokens visibility ++
      ((token.symbol token_symbol.colon) :: nil) ++
      decls_serialize decls
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
  end

with funtypeexpr_serialize
    (inner : list token.t)  (inner_prec : typeexpr_prec.t)
    (skip_head : bool) (this : funtypeexpr_t) : list token.t :=
  match this with
    | funtypeexpr ret_type args =>
      let inner := typeexpr_prec.maybe_wrap inner_prec typeexpr_prec.function inner in
      let inner :=
        inner ++
        ((token.symbol token_symbol.open_paren) :: nil) ++
        funargs_serialize true args ++
        ((token.symbol token_symbol.close_paren) :: nil) in
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

with stmt_serialize
    (this : stmt_t) : list token.t :=
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
      expr_serialize expr_prec.none cond ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
      stmt_serialize then_body
    | stmt_ifelse cond then_body else_body =>
      ((token.keyword token_keyword.kw_if) :: nil) ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      expr_serialize expr_prec.none cond ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
      stmt_serialize then_body ++
      ((token.keyword token_keyword.kw_else) :: nil) ++
      stmt_serialize else_body
    | stmt_return expr =>
      ((token.keyword token_keyword.kw_return) :: nil) ++
      expr_serialize expr_prec.none expr ++
      ((token.symbol token_symbol.semicolon) :: nil)
  end

with expr_serialize (outer_prec : expr_prec.t) (this : expr_t) : list token.t :=
  match this with
    | expr_id id =>
      idexpr_serialize id
    | expr_literal literal =>
      (token.literal literal) :: nil
    | expr_unop op arg => (* XXX: pre-/postfix unary operator *)
      let e := expr_serialize expr_prec.unop arg in
      (token.symbol (unop.to_symbol op)) :: nil ++
      e
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
      binders_serialize binders ++
      ((token.symbol token_symbol.close_bracket) :: nil) ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      funargs_serialize true args ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
      ((token.symbol token_symbol.open_brace) :: nil) ++
      stmts_serialize body ++
      ((token.symbol token_symbol.close_brace) :: nil)
  end

with callargs_serialize (first : bool) (this : callargs_t) : list token.t :=
  match this with
    | callargs_nil => nil
    | callargs_cons arg args =>
      (if first then nil else ((token.symbol token_symbol.comma) :: nil)) ++
      expr_serialize expr_prec.none arg ++  (* XXX: comma precedence *)
      callargs_serialize false args
  end.

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
  end.

Example ex_decl_array_of_pointers :=
    decl_simple
      nil
      (typeexpr_array (typeexpr_pointer (typeexpr_primitive primtype.int)) (expr_literal (literal.decimal 32)))
      (idexpr_id "foo")
      nil.
Eval lazy in (serialize_tokens (decl_serialize ex_decl_array_of_pointers)).
Example ex_decl_pointer_of_array :=
    decl_simple
      nil
      (typeexpr_pointer (typeexpr_array (typeexpr_primitive primtype.int) (expr_literal (literal.decimal 32))))
      (idexpr_id "foo")
      nil.
Eval lazy in (serialize_tokens (decl_serialize ex_decl_pointer_of_array)).
Example ex_decl_fun :=
    decl_simple
      nil
      (typeexpr_function
        (funtypeexpr
          (typeexpr_pointer (typeexpr_primitive primtype.int))
          (make_funargs (funarg_named (typeexpr_primitive primtype.int) "a" :: funarg_named (typeexpr_primitive primtype.char) "b" :: nil))))
      (idexpr_id "foo")
      nil.
Eval lazy in (serialize_tokens (decl_serialize ex_decl_fun)).
Example ex_decl_class :=
    decl_class
      (idexpr_id "foo")
      (make_clsdecls (
        (clsdecl_group
          visibility_spec.vis_public
          (make_decls
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
      nil
      (typeexpr_pointer (
        (typeexpr_function
          (funtypeexpr
            (typeexpr_pointer (typeexpr_primitive primtype.int))
            (funargs_nil)))))
      (idexpr_id "foo")
      nil.
Eval lazy in (serialize_tokens (decl_serialize ex_decl_funptr)).

Example ex_decl_fundef :=
  decl_fundef
    nil
    (funtypeexpr
      (typeexpr_primitive primtype.int)
      (make_funargs (
        funarg_named (typeexpr_primitive primtype.int) "a" ::
        funarg_named (typeexpr_primitive primtype.char) "b" ::
        nil)))
    (idexpr_id "foo")
    nil
    (make_stmts (
      (stmt_return (expr_literal (literal.decimal 32))) ::
      nil)
    ).
Eval lazy in (serialize_tokens (decl_serialize ex_decl_fundef)).

Example ex_hello_world_main :=
  decl_fundef
    nil
    (funtypeexpr
      (typeexpr_primitive primtype.int)
      (make_funargs (
        funarg_named (typeexpr_primitive primtype.int) "args" ::
        funarg_named (typeexpr_pointer (typeexpr_pointer (typeexpr_primitive primtype.char))) "argv" ::
        nil)))
    (idexpr_id "main")
    nil
    (make_stmts (
      (stmt_expr
        (expr_call
          (expr_id (idexpr_id "printf"))
          (make_callargs (
            expr_literal (literal.str "Hello world!") ::
            nil)))) ::
      (stmt_return (expr_literal (literal.decimal 0))) ::
      nil)
    ).
Eval lazy in (serialize_tokens (decl_serialize ex_hello_world_main)).

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
    | slash : t
    | percent : t
    | tilda : t
    | exclam : t
    | amp : t
    | pipe : t
    | wedge : t
    | eq : t
    | ne : t
    | lt : t
    | le : t
    | gt : t
    | ge : t
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
    | arrow : t
    | dot : t
    | double_amp : t
    | double_pipe : t
    | double_lt : t
    | double_gt : t
    | double_plus : t
    | double_minus : t
    | assign_plus : t
    | assign_minus : t
    | assign_mul : t
    | assign_div : t
    | assign_modulo : t
    | assign_and : t
    | assign_or : t
    | assign_xor : t
    | assign_shl : t
    | assign_shr : t
  .

  Definition to_string (this : t) : String.string :=
    match this with
      | assign => "="
      | amp => "&"
      | pipe => "|"
      | wedge => "^"
      | plus => "+"
      | minus => "-"
      | star => "*"
      | slash => "/"
      | percent => "%"
      | tilda => "~"
      | exclam => "!"
      | eq => "=="
      | ne => "!="
      | lt => "<"
      | le => "<="
      | gt => ">"
      | ge => ">="
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
      | arrow => "->"
      | dot => "."
      | double_amp => "&&"
      | double_pipe => "||"
      | double_lt => "<<"
      | double_gt => ">>"
      | double_plus => "++"
      | double_minus => "--"
      | assign_plus => "+="
      | assign_minus => "-="
      | assign_mul => "*="
      | assign_div => "/="
      | assign_modulo => "%="
      | assign_and => "&="
      | assign_or => "|="
      | assign_xor => "^="
      | assign_shl => "<<="
      | assign_shr => ">>="
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
    | kw_decltype : t
    | kw_dynamic_cast : t
    | kw_final : t
    | kw_throw : t
    | kw_default : t
    | kw_delete : t
    | kw_new : t
    | kw_operator : t
    | kw_do : t
    | kw_while : t
    | kw_switch : t
    | kw_case : t
    | kw_bool : t
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
      | kw_decltype => "decltype"
      | kw_dynamic_cast => "dynamic_cast"
      | kw_final => "final"
      | kw_throw => "throw"
      | kw_default => "default"
      | kw_delete => "delete"
      | kw_new => "new"
      | kw_operator => "operator"
      | kw_do => "do"
      | kw_while => "while"
      | kw_switch => "switch"
      | kw_case => "case"
      | kw_bool => "bool"
    end.

End token_keyword.

Module token.
  Inductive t : Set :=
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
  Inductive t : Set :=
    | negate
    | bit_not
    | logic_not
    | deref
    | address_of
    | pre_inc
    | post_inc
    | pre_dec
    | post_dec
  .

  Definition to_symbol (unop : t) : token_symbol.t :=
    match unop with
      | negate => token_symbol.minus
      | bit_not => token_symbol.tilda
      | logic_not => token_symbol.exclam
      | deref => token_symbol.star
      | address_of => token_symbol.amp
      | pre_inc => token_symbol.double_plus
      | post_inc => token_symbol.double_plus
      | pre_dec => token_symbol.double_minus
      | post_dec => token_symbol.double_minus
    end.
  Definition is_prefix (unop : t) : bool :=
    match unop with
      | post_inc => false
      | post_dec => false
      | _ => true
    end.
End unop.

Module binop.
  Inductive t : Set :=
    | assign
    | plus
    | minus
    | mul
    | div
    | modulo
    | eq
    | ne
    | lt
    | le
    | gt
    | ge
    | logic_and
    | logic_or
    | bitwise_and
    | bitwise_or
    | bitwise_xor
    | assign_plus
    | assign_minus
    | assign_mul
    | assign_div
    | assign_modulo
    | assign_and
    | assign_or
    | assign_xor
    | assign_shl
    | assign_shr
  .
  Definition to_symbol (binop : t) : token_symbol.t :=
    match binop with
      | assign => token_symbol.assign
      | plus => token_symbol.plus
      | minus => token_symbol.minus
      | mul => token_symbol.star
      | div => token_symbol.slash
      | modulo => token_symbol.percent
      | eq => token_symbol.eq
      | ne => token_symbol.ne
      | lt => token_symbol.lt
      | le => token_symbol.le
      | gt => token_symbol.gt
      | ge => token_symbol.ge
      | logic_and => token_symbol.double_amp
      | logic_or => token_symbol.double_pipe
      | bitwise_and => token_symbol.amp
      | bitwise_or => token_symbol.pipe
      | bitwise_xor => token_symbol.wedge
      | assign_plus => token_symbol.assign_plus
      | assign_minus => token_symbol.assign_minus
      | assign_mul => token_symbol.assign_mul
      | assign_div => token_symbol.assign_div
      | assign_modulo => token_symbol.assign_modulo
      | assign_and => token_symbol.assign_and
      | assign_or => token_symbol.assign_or
      | assign_xor => token_symbol.assign_xor
      | assign_shl => token_symbol.assign_shl
      | assign_shr => token_symbol.assign_shr
    end.
End binop.

Module primtype.
  Inductive t : Set :=
    | void
    | auto
    | bool
    | char
    | unsigned_char
    | signed_char
    | int
    | signed_int.

  Definition to_tokens (type : t) : list token.t :=
    match type with
      | void => (token.keyword token_keyword.kw_void) :: nil
      | auto => (token.keyword token_keyword.kw_auto) :: nil
      | bool => (token.keyword token_keyword.kw_bool) :: nil
      | char => (token.keyword token_keyword.kw_char) :: nil
      | unsigned_char => (token.keyword token_keyword.kw_unsigned) :: (token.keyword token_keyword.kw_char) :: nil
      | signed_char => (token.keyword token_keyword.kw_signed) :: (token.keyword token_keyword.kw_char) :: nil
      | int => (token.keyword token_keyword.kw_int) :: nil
      | unsigned_int => (token.keyword token_keyword.kw_unsigned) :: (token.keyword token_keyword.kw_int) :: nil
    end.
End primtype.

Module visibility_spec.
  Inductive t : Set :=
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

Module declspec.
  Inductive t : Set :=
    | make :
      forall (is_register : bool),
      forall (is_static : bool),
      forall (is_thread_local : bool),
      forall (is_extern : bool),
      forall (is_mutable : bool),
      forall (is_inline : bool),
      forall (is_virtual : bool),
      forall (is_explicit : bool),
      forall (is_friend : bool),
      forall (is_constexpr : bool),
      t.

  Definition none : t := make false false false false false false false false false false.

  Definition set_register (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make true is_static is_thread_local is_extern is_mutable is_inline is_virtual is_explicit is_friend is_constexpr.
  Definition set_static (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register true is_thread_local is_extern is_mutable is_inline is_virtual is_explicit is_friend is_constexpr.
  Definition set_thread_local (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static true is_extern is_mutable is_inline is_virtual is_explicit is_friend is_constexpr.
  Definition set_extern (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local true is_mutable is_inline is_virtual is_explicit is_friend is_constexpr.
  Definition set_mutable (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local is_extern true is_inline is_virtual is_explicit is_friend is_constexpr.
  Definition set_inline  (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local is_extern is_mutable true is_virtual is_explicit is_friend is_constexpr.
  Definition set_virtual (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local is_extern is_mutable is_inline true is_explicit is_friend is_constexpr.
  Definition set_explicit (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local is_extern is_mutable is_inline is_virtual true is_friend is_constexpr.
  Definition set_friend (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local is_extern is_mutable is_inline is_virtual is_explicit true is_constexpr.
  Definition set_constexpr (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local is_extern is_mutable is_inline is_virtual is_explicit is_friend true.

  Definition clear_register (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make false is_static is_thread_local is_extern is_mutable is_inline is_virtual is_explicit is_friend is_constexpr.
  Definition clear_static (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register false is_thread_local is_extern is_mutable is_inline is_virtual is_explicit is_friend is_constexpr.
  Definition clear_thread_local (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static false is_extern is_mutable is_inline is_virtual is_explicit is_friend is_constexpr.
  Definition clear_extern (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local false is_mutable is_inline is_virtual is_explicit is_friend is_constexpr.
  Definition clear_mutable (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local is_extern false is_inline is_virtual is_explicit is_friend is_constexpr.
  Definition clear_inline  (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local is_extern is_mutable false is_virtual is_explicit is_friend is_constexpr.
  Definition clear_virtual (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local is_extern is_mutable is_inline false is_explicit is_friend is_constexpr.
  Definition clear_explicit (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local is_extern is_mutable is_inline is_virtual false is_friend is_constexpr.
  Definition clear_friend (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local is_extern is_mutable is_inline is_virtual is_explicit false is_constexpr.
  Definition clear_constexpr (this : t) : t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    make is_register is_static is_thread_local is_extern is_mutable is_inline is_virtual is_explicit is_friend false.

  Definition is_register (this : t) : bool :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    is_register.
  Definition is_static (this : t) : bool :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    is_static.
  Definition is_thread_local (this : t) : bool :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    is_thread_local.
  Definition is_extern (this : t) : bool :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    is_extern.
  Definition is_mutable (this : t) : bool :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    is_mutable.
  Definition is_inline (this : t) : bool :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    is_inline.
  Definition is_virtual (this : t) : bool :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    is_virtual.
  Definition is_explicit (this : t) : bool :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    is_explicit.
  Definition is_friend (this : t) : bool :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    is_friend.
  Definition is_constexpr (this : t) : bool :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    is_constexpr.

  Definition maybe_keyword (use : bool) (kw : token_keyword.t) : list token.t :=
    if use then
      token.keyword kw :: nil
    else nil.

  Definition to_tokens (this : t) : list token.t :=
    let (is_register, is_static, is_thread_local, is_extern, is_mutable, is_inline, is_virtual, is_explicit, is_friend, is_constexpr) := this in
    maybe_keyword is_register token_keyword.kw_register ++
    maybe_keyword is_static token_keyword.kw_static ++
    maybe_keyword is_thread_local token_keyword.kw_thread_local ++
    maybe_keyword is_extern token_keyword.kw_extern ++
    maybe_keyword is_mutable token_keyword.kw_mutable ++
    maybe_keyword is_inline token_keyword.kw_inline ++
    maybe_keyword is_virtual token_keyword.kw_virtual ++
    maybe_keyword is_explicit token_keyword.kw_explicit ++
    maybe_keyword is_friend token_keyword.kw_friend ++
    maybe_keyword is_constexpr token_keyword.kw_constexpr.

End declspec.

Module attrspec.
  Inductive t : Set :=
    | make :
      forall (is_override : bool),
      t.
  Definition none := make false.
  Definition set_override (this : t) :=
    make true.
  Definition to_tokens (this : t) : list token.t :=
    let (is_override) := this in
    if is_override then
      token.keyword token_keyword.kw_override :: nil
    else
      nil.
End attrspec.

Module overloadable_operator.
  Inductive t : Set :=
    | unary : forall (op : unop.t), t
    | binary : forall (op : binop.t), t
    | call : t
    | arrow : t
  .

  Definition to_tokens (this : t) : list token.t :=
    match this with
      | unary op =>
        token.symbol (unop.to_symbol op) :: nil
      | binary op =>
        token.symbol (binop.to_symbol op) :: nil
      | call =>
        token.symbol token_symbol.open_paren ::
        token.symbol token_symbol.close_paren ::
        nil
      | arrow =>
        token.symbol token_symbol.arrow ::
        nil
    end.
End overloadable_operator.

Module fnqual.
  Inductive t : Set :=
    | make :
      forall (is_const : bool),
      forall (is_volatile : bool),
      forall (is_noexcept : bool),
      t.

  Definition none : t := make false false false.
  Definition set_const (this : t) :=
    let (is_const, is_volatile, is_noexcept) := this in
    make true is_volatile is_noexcept.
  Definition set_volatile (this : t) :=
    let (is_const, is_volatile, is_noexcept) := this in
    make is_const true is_noexcept.
  Definition set_noexcept (this : t) :=
    let (is_const, is_volatile, is_noexcept) := this in
    make is_const is_volatile true.

  Definition to_tokens (this : t) : list token.t :=
    let (is_const, is_volatile, is_noexcept) := this in
    (if is_const then
      token.keyword token_keyword.kw_const :: nil
    else
      nil) ++
    (if is_volatile then
      token.keyword token_keyword.kw_volatile :: nil
    else
      nil) ++
    (if is_noexcept then
      token.keyword token_keyword.kw_noexcept :: nil
    else
      nil).
End fnqual.

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
    forall (op : overloadable_operator.t),
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
      (overloadable_operator.to_tokens op)
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
      binders_serialize true binders ++
      ((token.symbol token_symbol.close_bracket) :: nil) ++
      ((token.symbol token_symbol.open_paren) :: nil) ++
      funargs_serialize true args ++
      ((token.symbol token_symbol.close_paren) :: nil) ++
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
          (make_funargs (funarg_named (typeexpr_primitive primtype.int) "a" :: funarg_named (typeexpr_primitive primtype.char) "b" :: nil))
          fnqual.none
          false))
      (idexpr_id scope_none "foo")
      attrspec.none.
Eval lazy in (serialize_tokens (decl_serialize ex_decl_fun)).
Example ex_decl_class :=
    decl_class
      (idexpr_id scope_none "foo") false clsinherits_nil
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
      (make_funargs (
        funarg_named (typeexpr_primitive primtype.int) "a" ::
        funarg_named (typeexpr_primitive primtype.char) "b" ::
        nil))
      fnqual.none
      false)
    (idexpr_id scope_none "foo")
    attrspec.none
    (funbody_stmts
      cinits_nil
      (make_stmts (
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
      (make_funargs (
        funarg_named (typeexpr_primitive primtype.int) "args" ::
        funarg_named (typeexpr_pointer (typeexpr_pointer (typeexpr_primitive primtype.char))) "argv" ::
        nil))
      fnqual.none
      false)
    (idexpr_id scope_none "main")
    attrspec.none
    (funbody_stmts
      cinits_nil
      (make_stmts (
        (stmt_expr
          (expr_call
            (expr_id (idexpr_id scope_none "printf"))
            (make_callargs (
              expr_literal (literal.str "Hello world!") ::
              nil)))) ::
        (stmt_return (expr_literal (literal.decimal 0))) ::
        nil)
      )
    ).
Eval lazy in (serialize_tokens (decl_serialize ex_hello_world_main)).






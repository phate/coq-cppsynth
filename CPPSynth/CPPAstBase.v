Require Import String Ascii Peano_dec List.
Require CPPSynth.StringUtil.

Module literal.
  Inductive t : Set :=
    | decimal : nat -> t
    | str : string -> t.

  Definition eq_dec (x y : t) : {x=y} + {x<>y}.
    refine (
      match x, y with
        | decimal v1, decimal v2 => if eq_nat_dec v1 v2 then left _ _ else right _ _
        | str s1, str s2 => if string_dec s1 s2 then left _ _ else right _ _
        | _, _ => right _ _
      end) ; try congruence.
  Defined.

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

  Definition eq_dec : forall (x y : t), {x=y} + {x<>y}.
    decide equality.
  Defined.

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

  Definition eq_dec : forall (x y : t), {x=y} + {x<>y}.
    decide equality.
  Defined.

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

Module genop.
  Inductive t : Set :=
    | unary : forall (op : unop.t), t
    | binary : forall (op : binop.t), t
    | call : t
    | arrow : t
  .

  Definition eq_dec : forall (x y : t), {x=y} + {x<>y}.
    decide equality ; try apply unop.eq_dec ; try apply binop.eq_dec.
  Defined.

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
End genop.

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

  Definition eq_dec : forall (x y : t), {x=y} + {x<>y}.
    decide equality.
  Defined.

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

  Definition eq_dec : forall (x y : t), {x=y} + {x<>y}.
    decide equality.
  Defined.

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

  Definition eq_dec : forall (x y : t), {x=y} + {x<>y}.
    repeat decide equality.
  Defined.

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
  Definition eq_dec : forall (x y : t), {x=y} + {x<>y}.
    repeat decide equality.
  Defined.

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

Module fnqual.
  Inductive t : Set :=
    | make :
      forall (is_const : bool),
      forall (is_volatile : bool),
      forall (is_noexcept : bool),
      t.

  Definition eq_dec : forall (x y : t), {x=y} + {x<>y}.
    repeat decide equality.
  Defined.

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

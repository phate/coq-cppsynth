Require Import 
  Coq.Strings.Ascii
  Coq.Strings.String
  Coq.Lists.List
  CPPSynth.Exception
  CPPSynth.Monad
  CPPSynth.StringUtil
  CPPSynth.StateMonad.

(** Serialize a terminal symbol. *)
Definition serialize_terminal (s : String.string) : String.string :=
  match s with
    | EmptyString => "\0" % string
    | _ => StringUtil.escape_string s
  end.

(** 
Tokenizer for S-Expression parser
 *)
Module sexpr_tokenizer.
  Module token.
    Inductive t : Set :=
      | open : t
      | close : t
      | id : forall (id : String.string), t.
  End token.

  Module tokstate.
    Inductive t : Set :=
      | none : t
      | open : t
      | close : t
      | id : t
      | id_escape : t.
    Definition is_id_escape (this : t) : bool :=
      match this with | id_escape => true | _ => false end.
    Definition is_id (this : t) : bool :=
      match this with | id => true | _ => false end.
  End tokstate.

  Inductive t : Set :=
    | make : 
      forall (state : tokstate.t) (id : String.string), t.

  Definition get_state (this : t) :=
    let (state, _) := this in state.
  Definition get_id (this : t) :=
    let (_, id) := this in id.
  Definition set_state (state : tokstate.t) (this : t) :=
    let (_, id) := this in make state id.
  Definition set_id (id : String.string) (this : t) :=
    let (state, _) := this in make state id.

  Definition new : t :=
    make tokstate.none "".

  Definition flush_token (this : t) : option token.t :=
    let (state, id) := this in
    match state with
      | tokstate.none => None
      | tokstate.open => Some token.open
      | tokstate.close => Some token.close
      | tokstate.id => Some (token.id (get_id this))
      | tokstate.id_escape => None
    end.

  Definition process (c : ascii) (this : t) : (option token.t) * t :=
    let (state, id) := this in
    if tokstate.is_id_escape state then
      let id :=
        if Ascii.eqb c "0" then 
          id
        else 
          String.append id (String c EmptyString) in
      let this := make tokstate.id id in
      (None, this)
    else
      if Ascii.eqb c " " then
        let tok := flush_token this in
        (tok, make tokstate.none "")
      else if Ascii.eqb c "(" then
        let tok := flush_token this in
        (tok, make tokstate.open "")
      else if Ascii.eqb c ")" then
        let tok := flush_token this in
        (tok, make tokstate.close "")
      else if Ascii.eqb c "\" then
        if tokstate.is_id state then
          (None, make tokstate.id_escape id)
        else
          let tok := flush_token this in
          (tok, make tokstate.id_escape "")
      else
        let c := (String c EmptyString) in
        if tokstate.is_id state then
          let id := String.append id c in
          (None, make tokstate.id id)
        else
          let tok := flush_token this in
          (tok, make tokstate.id c).

  Definition finish (this : t)
      : (option token.t) * t :=
    let tok := flush_token this in
    (tok, make tokstate.none "").

End sexpr_tokenizer.

Definition sexpr_tokenize_string_front (s : string) : sexpr_tokenizer.t * list sexpr_tokenizer.token.t :=
  let tokenizer := sexpr_tokenizer.new in
  let tokens := @nil sexpr_tokenizer.token.t in
  let (tokenizer, tokens) := string_iterate
    (fun c (state : sexpr_tokenizer.t * list sexpr_tokenizer.token.t) =>
      let (tokenizer, tokens) := state in
      let (tok, tokenizer) := sexpr_tokenizer.process c tokenizer in
      let (tokenizer, tokens) := 
        match tok with
          | Some tok =>
            let tokens := tokens ++ (tok :: nil) in
            (tokenizer, tokens)
          | None =>
            (tokenizer, tokens)
        end in
      (tokenizer, tokens))
    (tokenizer, tokens)
    s in
  (tokenizer, tokens).

Definition sexpr_tokenize_string (s : string) : list sexpr_tokenizer.token.t :=
  let (tokenizer, tokens) := sexpr_tokenize_string_front s in
  let (tok, tokenizer) := sexpr_tokenizer.finish tokenizer in
  let tokens := 
    match tok with
      | Some tok => tokens ++ (tok :: nil)
      | None => tokens
    end in
  tokens.

Definition sexpr_serialize_tokens (tokens : list sexpr_tokenizer.token.t) : string :=
  let s := EmptyString in
  let need_sep := false in
  let (need_sep, s) := List.fold_left
    (fun (state : bool * string) tok =>
      let (need_sep, s) := state in
      let (need_sep, s) :=
        match tok with
          | sexpr_tokenizer.token.open =>
            (false, String.append s "(")
          | sexpr_tokenizer.token.close =>
            (false, String.append s ")")
          | sexpr_tokenizer.token.id id =>
            let s := if need_sep then String.append s " " else s in
            (true, String.append s (serialize_terminal id))
        end in
      (need_sep, s))
      tokens (need_sep, s) in
  s.

(** 

S-expressions

An S-expression has the form: 
  s-expr := terminal | "(" terminal [s-expr...] ")"
where terminal is an identifier (any character except whitespace
and braces) and may nest arbitrarily. Conventionally, the
terminal at the head of an s-expr specifies the type / semantic
of the expression.

**)
Module sexpr.

  Inductive t : Set :=
    | terminal : forall (id : String.string), t
    | expr : forall (kind : String.string) (args : list_t), t
  with list_t : Set :=
    | list_nil : list_t
    | list_cons : t -> list_t -> list_t.

  Fixpoint list_from (l : list t) : list_t :=
    match l with
      | nil => list_nil
      | cons e l => list_cons e (list_from l)
    end.

  Fixpoint list_app (e : t) (l : list_t) : list_t :=
    match l with
      | list_nil => list_cons e list_nil
      | list_cons x l => list_cons x (list_app e l)
    end.

  (** Recursively append sexpr to given string *)
  Fixpoint app_str (this : t) (state : String.string) : String.string :=
    match this with
      | terminal s => String.append state (serialize_terminal s)
      | expr kind exprs =>
        let state := String.append state "("%string in
        let state := String.append state (serialize_terminal kind) in
        let state := list_app_str exprs state in
        let state := String.append state ")"%string in
        state
    end
  (** Recursively append sexpr list to given string *)
  with list_app_str (this : list_t) (state : String.string) : String.string :=
    match this with
      | list_nil => state
      | list_cons e exprs =>
        let state := String.append state " " in
        let state := app_str e state in
        list_app_str exprs state
    end.

  Definition to_string (this : t) : String.string :=
      app_str this "".

(*
Definition sexpr_from_list (l : list sexpr_t) : ExceptionOr sexpr_t :=
  match l with
    | head :: tail =>
      match head with
        | sexpr_terminal s => Okay (sexpr_expr s (sexprlist_from_list tail))
        | _ => Exception "ParseError"%string "Leading term of s-expr must be a terminal."
      end
    | _ => Exception "ParseError"%string "s-expr cannot be empty."
  end.
*)

End sexpr.

(*
Module sexpr_parse_stack.
  (** Nesting stack of partially parsed s-expressions.
      During parsing, the depth of the stack corresponds to the
      number of opening "(" and not yet closed ")" s-expressions
      encountered at parse point. At each level, it represents
      the partially finished s-expression sequences already
      completed to the left of the parse point. The stack is
      "inverted" with respect to s-expression nesting: the lowest
      nesting level is at the top of the stack. **)
  Inductive t : Set :=
    | make : forall (layers : list sexpr.t), t.

  (** New stack with first layer initialized. **)
  Definition new : t := make nil.

  (** Push a new empty layer to stack. **)
  Definition push (head : String.string) (args : sexpr.list_t) (this : t) : t :=
    make None sexpr.list_nil (layer_cons
    layer nil (Some stack).

  (** Append s-expression to top layer of stack. **)
  Definition app (e : sexpr.t) (stack : t) : t :=
    match stack with
      | layer exprs stack => layer (exprs ++ e :: nil) stack
    end.

  (** Pop top-most layer of stack, try to form it into a proper
      s-expression, and append it to the next lower layer of stack
      (which now becomes the top layer). **)
  Definition pop (stack : t) : ExceptionOr t :=
    match stack with
      | layer exprs (Some stack) =>
        do e <-- sexpr._from_proto_exprs exprs ;
        return_ _ (app e stack)
      | empty => Exception "ParseError" "Empty expression stack"
    end.
End sexpr_parse_stack.
*)

Module sexpr_parser.
  Inductive state_kind : Set :=
    | ERROR : state_kind
    | EXPRESSION : state_kind
    | HEAD : state_kind.

  Inductive layer_t : Set :=
    | make_layer :
      forall (head : String.string),
      forall (args : sexpr.list_t),
      layer_t.

  Inductive t : Set :=
    | make : 
      forall (kind : state_kind),
      forall (stack : list layer_t),
      t.

  Definition new := make EXPRESSION (make_layer "" sexpr.list_nil :: nil).

  Definition process_token (tok : sexpr_tokenizer.token.t) (this : t) : t :=
    let (kind, stack) := this in
    match kind with
      | ERROR => this
      | EXPRESSION =>
        match tok with
          | sexpr_tokenizer.token.open =>
            make HEAD stack
          | sexpr_tokenizer.token.close =>
            match stack with
              | (make_layer t_head t_args) :: (make_layer n_head n_args) :: stack =>
                let e := sexpr.expr t_head t_args in
                let n_args := sexpr.list_app e n_args in
                let stack := (make_layer n_head n_args) :: stack in
                make EXPRESSION stack
              | _ => make ERROR nil
            end
          | sexpr_tokenizer.token.id id =>
            match stack with
              | (make_layer n_head n_args) :: stack =>
                let e := sexpr.terminal id in
                let n_args := sexpr.list_app e n_args in
                let stack := (make_layer n_head n_args) :: stack in
                make EXPRESSION stack
              | _ => make ERROR nil
            end
        end
      | HEAD =>
        match tok with
          | sexpr_tokenizer.token.id id =>
            let stack := (make_layer id sexpr.list_nil) :: stack in
            make EXPRESSION stack
          | _ => make ERROR nil
        end
    end.

  (** Finalize a state: Try to return unique s-expr from top
      of parse stack which must be exactly 1 layer deep. **)
  Definition finalize (state : t) : option sexpr.t :=
    let (kind, stack) := state in
    match kind with
      | ERROR => None
      | HEAD => None
      | EXPRESSION =>
        match stack with
          | top :: nil =>
            let (_, exprs) := top in
            match exprs with
              | sexpr.list_cons e sexpr.list_nil =>
                Some e
              | _ => None
            end
          | _ => None
        end
    end.

  Definition process_token_sequence
      (seq : list sexpr_tokenizer.token.t)
      (this : t) : t :=
    List.fold_left (fun s e => process_token e s) seq this.

End sexpr_parser.

(** Try to parse s-expr from string. **)
Definition sexpr_from_string (s : string) : option sexpr.t :=
  let tokenizer := sexpr_tokenizer.new in
  let parser := sexpr_parser.new in
  let (tokenizer, parser) := string_iterate
    (fun c (state : sexpr_tokenizer.t * sexpr_parser.t) =>
      let (tokenizer, parser) := state in
      let (tok, tokenizer) := sexpr_tokenizer.process c tokenizer in
      match tok with
        | Some tok =>
          let parser := sexpr_parser.process_token tok parser in
          (tokenizer, parser)
        | _ =>
          (tokenizer, parser)
      end)
    (tokenizer, parser)
    s in
  let (tok, tokenizer) := sexpr_tokenizer.finish tokenizer in
  match tok with
    | Some tok =>
      let parser := sexpr_parser.process_token tok parser in
      sexpr_parser.finalize parser
    | _ =>
      sexpr_parser.finalize parser
  end.

Example test_expr1_to_string
    : let expr1 :=
        sexpr.expr 
          "Definition"
          (sexpr.list_from (sexpr.terminal "foo" :: sexpr.terminal "bar" :: nil)) in
      sexpr.to_string expr1 = "(Definition foo bar)"%string
  := eq_refl.

Example test_string1_to_expr
    : sexpr_from_string ("(Definition abc (foo bar))"%string) = 
      Some
        (sexpr.expr "Definition" (sexpr.list_from (
          sexpr.terminal "abc" ::
          sexpr.expr "foo" (sexpr.list_from (sexpr.terminal "bar" :: nil)) :: nil
        )))
  := eq_refl.

Example test_parse_empty_terminal
    : sexpr_from_string ("\0" % string) =
      Some (sexpr.terminal "")
  := eq_refl.

Example test_parse_escape
    : sexpr_from_string ("(Def\(ini\)\ tion abc (foo bar))"%string) = 
      Some
        (sexpr.expr "Def(ini) tion" (sexpr.list_from (
          sexpr.terminal "abc" ::
          sexpr.expr "foo" (sexpr.list_from (sexpr.terminal "bar" :: nil)) :: nil
        )))
  := eq_refl.

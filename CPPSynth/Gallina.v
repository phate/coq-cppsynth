Require Import 
  Coq.Arith.Compare_dec
  Coq.Numbers.DecimalString
  Coq.Numbers.DecimalNat
  Coq.Strings.String
  Coq.Lists.List
  CPPSynth.SExpression
  CPPSynth.StringUtil
  CPPSynth.Exception
  CPPSynth.Monad.

Module name.
  Definition t := String.string.

  Definition to_string (name : t) : String.string :=
    name.

  Definition eqb (x y : t) : bool :=
    String.eqb x y.

  Definition eq_dec : forall (x y : t), { x = y } + { x <> y }.
    intros x y ; case_eq (eqb x y) ; intros H.
      apply eqb_eq in H ; left ; auto.
      apply eqb_neq in H ; right ; auto.
  Defined.

  Definition from_sexpr_literal (e : sexpr.t) : option String.string :=
    match e with
      | sexpr.terminal arg => Some arg
      | _ => None
    end.

  (** Given an s-expression of form ("Name", x) with x being a terminal
  symbol, returns x. *)
  Definition from_sexpr (e : sexpr.t) : option String.string :=
    match e with
      | sexpr.expr "Name" (sexpr.list_cons e' sexpr.list_nil) => from_sexpr_literal e'
      | _ => None
    end.

  Definition uniquify (name : t) (l : list t) : t :=
    let n := StringUtil.unique_nat_suffix name 0 l in
    (name ++ StringUtil.make_underscores n) % string.

  Lemma uniquify_spec :
      forall name l,
      ~ In (uniquify name l) l.
    unfold uniquify ; intros ; apply StringUtil.unique_nat_suffix_spec with (k := 0) ; auto.
  Qed.

  (* generate a renaming function that remaps every name in "names" to
  a new name if it would otherwise collide with "taboo" *)
  Fixpoint make_rename_fn (names : list t) (taboo : list t) : (t -> t) :=
    match names with
      | nil => @id t
      | n :: names' =>
        let new_n :=
          if In_dec eq_dec n taboo
          then uniquify n taboo
          else n in
        let fn := make_rename_fn names' (new_n :: taboo) in
        (fun (x : t) => if eqb x n then new_n else fn x)
    end.

End name.

Module identifier.
  Inductive t : Set :=
    | local : name.t -> t
    | global : name.t -> t.

  Definition to_string (id : t) : String.string :=
    match id with
      | local n => n
      | global n => n
    end.

  Definition eqb (x y : t) : bool :=
    match x, y return bool with
      | local x, local y => String.eqb x y
      | global x, global y => String.eqb x y
      | _, _ => false
    end.
End identifier.


Module expr.
  Inductive t : Set :=
    | global :
      forall (name : name.t), t
    | local :
      forall (name : name.t) (index : nat), t
    | product : 
      forall (argname : option String.string) (argtype : t) (expr : t), t
    | app :
      forall (fn : t) (arg : t), t.

  (** Generic subterm visitor: check term whether we want to descend,
  push/pop state on descending and transform bottom-up. *)
  Fixpoint generic_visit
      (state_t : Set) 
      (down : t -> state_t -> bool * state_t)
      (xform : t -> state_t -> t * state_t)
      (up : t -> state_t -> state_t)
      (e : t)
      (state : state_t)
      { struct e }
      : t * state_t :=
    let (descend, state) := down e state in
    if descend then
      let (e, state) := match e with
        | product argname argtype body =>
          let (argtype, state) := generic_visit _ down xform up argtype state in
          let (body, state) := generic_visit _ down xform up body state in
          (product argname argtype body, state)
        | app fn arg =>
          let (fn, state) := generic_visit _ down xform up fn state in
          let (arg, state) := generic_visit _ down xform up arg state in
          (app fn arg, state)
        | _ => (e, state)
      end in
      let (e, state) := xform e state in
      let state := up e state in
      (e, state)
    else
      (e, state).

  Definition sexpr_args_split3 (args : sexpr.list_t) : ExceptionOr (sexpr.t * sexpr.t * sexpr.t) :=
    match args with
      | sexpr.list_cons arg1 (sexpr.list_cons arg2 (sexpr.list_cons arg3 sexpr.list_nil)) =>
        Okay (arg1, arg2, arg3)
      | _ => Exception "ParseError" "Expected 3 arguments"
    end.

  Definition sexpr_single_lit (args : sexpr.list_t) : ExceptionOr string :=
    match args with
      | sexpr.list_cons arg sexpr.list_nil =>
        match arg with
          | sexpr.terminal s => Okay s
          | _ => Exception "ParseError" "Expected single literal"
        end
      | _ => Exception "ParseError" "Expected single literal"
    end.

  Fixpoint from_sexpr (e : sexpr.t) : ExceptionOr t :=
    match e with
      | sexpr.terminal t => Exception "ParseError" "Compound expression expected"
      | sexpr.expr kind args =>
        if string_dec kind "Sort" then
          do name <-- sexpr_single_lit args ;
          return_ _ (global name)
        else if string_dec kind "Global" then
          do name <-- sexpr_single_lit args ;
          return_ _ (global name)
        else if string_dec kind "Local" then
          do name <-- sexpr_single_lit args ;
          return_ _ (local name 0)
        else if string_dec kind "Prod" then
          match args with
            | sexpr.list_cons argname (sexpr.list_cons argtype (sexpr.list_cons body sexpr.list_nil)) =>
              do argtype' <-- from_sexpr argtype ;
              do body' <-- from_sexpr body ;
              return_ _ (product None argtype' body')
            | _ => Exception "ParseError" "Product requires 3 arguments"
          end
        else
          Exception "ParseError" ("Unknown kind " ++ kind)
    end.

End expr.

Module inductive_constructor.
  Inductive t :=
    | make :
      forall (name : string) (type : expr.t), t.

  Definition from_sexpr (e : sexpr.t) : ExceptionOr t :=
    match e with
      | sexpr.terminal t => Exception "ParseError" "Compound expression expected"
      | sexpr.expr kind args =>
        if string_dec kind "Constructor" then
          match args with
            | sexpr.list_cons (sexpr.terminal name) (sexpr.list_cons type sexpr.list_nil) =>
              do type' <-- expr.from_sexpr type ;
              return_ _ (make name type')
            | _ => Exception "ParseError" "Product requires 3 arguments"
          end
        else
          Exception "ParseError" ("Expected constructor, received " ++ kind)
    end.

  Definition from_sexpr_list (l : sexpr.list_t) : ExceptionOr (list t) :=
    let result := nil in
    (fix loop (l : sexpr.list_t) (result : list t) : ExceptionOr (list t) :=
      match l with
        | sexpr.list_cons e l =>
          do c <-- from_sexpr e ;
          let result := result ++ c :: nil in
          loop l result
        | sexpr.list_nil => Okay result
      end) l result.

  Definition type (this : t) : expr.t := let (name, type) := this in type.
  Definition name (this : t) : string := let (name, type) := this in name.

End inductive_constructor.

Module one_inductive.
  Inductive t : Set :=
    | make :
      forall (name : string),
      forall (type : expr.t),
      forall (cons : list inductive_constructor.t),
      t.

  Definition from_sexpr_list (l : sexpr.list_t) : ExceptionOr t :=
    match l with
      | sexpr.list_cons (sexpr.terminal name) (sexpr.list_cons type constrs) =>
        let name' := name in
        do type' <-- expr.from_sexpr type ;
        do constrs' <-- inductive_constructor.from_sexpr_list constrs ;
        return_ _ (make name type' constrs')
      | _ => Exception "ParseError" "OneInductive needs name, type and constructors"
    end.

  Definition from_sexpr (e : sexpr.t) : ExceptionOr t :=
    match e with
      | sexpr.expr kind l =>
        if string_dec kind "OneInductive" then
          from_sexpr_list l
        else
          Exception "ParseError" ("Expected OneInducitev, got " ++ kind)
      | _ => Exception "ParseError" "OneInductive needs name, type and constructors"
    end.

End one_inductive.

Module inductive.
  Inductive t : Set :=
    | make :
      forall (parts : list one_inductive.t),
      t.

  Definition from_sexpr_list (l : sexpr.list_t) : ExceptionOr t :=
    let result := nil in
    do result <--
      (fix loop (l : sexpr.list_t) (result : list one_inductive.t) : ExceptionOr (list one_inductive.t) :=
        match l with
          | sexpr.list_cons e l =>
            do oi <-- one_inductive.from_sexpr e ;
            let result := result ++ oi :: nil in
            loop l result
          | sexpr.list_nil => Okay result
        end) l result ;
    return_ _ (make result).

End inductive.

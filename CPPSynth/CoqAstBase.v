Require Import 
  Coq.Lists.List
  Coq.Strings.String
  CPPSynth.SExpression
  CPPSynth.StringUtil.

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


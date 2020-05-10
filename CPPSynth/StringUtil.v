(* Various utilizies for strings. *)

Require Import 
  Coq.Arith.Compare_dec
  Coq.Arith.Peano_dec
  Coq.Arith.PeanoNat
  Coq.Lists.List
  Coq.Numbers.DecimalString
  Coq.Numbers.DecimalNat
  Coq.Strings.Ascii
  Coq.Strings.String
  Omega.

Module char.
  Open Scope char_scope.
  Definition t := ascii.
  Definition eqb (x y : t) : bool :=
    Coq.Strings.Ascii.eqb x y.
  Definition leb (x y : t) : bool :=
    if le_dec (nat_of_ascii x) (nat_of_ascii y) then true else false.
  Definition ltb (x y : t) : bool :=
    leb x y && negb (eqb x y).
  Definition gtb (x y : t) : bool :=
    negb (leb y x).
  Definition geb (x y : t) : bool :=
    negb (ltb y x).
  Definition is_whitespace (x : t) : bool :=
    eqb x " " || eqb x "008" || eqb x "012" || eqb x "015".
  Definition is_upper (x : t) : bool :=
    leb "A" x && leb x "Z".
  Definition is_lower (x : t) : bool :=
    leb "a" x && leb x "z".
  Definition is_alpha (x : t) : bool :=
    is_upper x || is_lower x.
  Definition is_digit (x : t) : bool :=
    leb "0" x && leb x "9".
  Definition is_alnum (x : t) : bool :=
    is_alpha x || is_digit x.
  Definition to_string (x : t) : String.string :=
    String x EmptyString.
  Definition nl : t := "010".
  Definition tab : t := "008".
  Theorem eqb_eq : forall x y, eqb x y = true <-> x = y.
    apply Coq.Strings.Ascii.eqb_eq.
  Qed.
End char.

Theorem string_app_ex :
    forall (c : char.t) (s : string),
    exists c' s', String c s = (s' ++ (char.to_string c'))%string.
  intros c s ; revert c.
  induction s as [|c s IHs] ; [
    intros c ; exists c ; exists ""%string ; auto
    |
    intros x ;
    specialize IHs with c ;
    destruct IHs as [c' [s' H]] ;
    exists c' ; exists (String x s') ;
    simpl ; congruence
  ].
Qed.

Theorem string_app_length :
    forall (c : char.t) (s : string),
    String.length (s ++ (char.to_string c)) = S (String.length s).
  intros c s ; induction s ; [| simpl ; rewrite IHs] ; auto.
Qed.

Theorem string_appchar_ind
  : forall (P : string -> Prop),
    P EmptyString ->
    (forall (c : char.t) (s : string), P s -> P (String.append s (char.to_string c))) ->
    forall s, P s.
  intros P H0 H s.
  remember (String.length s) as n.
  assert (Hl : String.length s <= n) by (omega).
  clear Heqn.
  revert s Hl.
  induction n ; intros s Hl.
    destruct s ; simpl in * ; auto ; omega.

    destruct s as [|a s] ; [auto|] ;
    destruct (string_app_ex a s) as [a' [s' H']] ;
    rewrite H' in * ;
    rewrite string_app_length in * ;
    apply H ; apply IHn ; omega.
Qed.

Definition string_empty (s : string) : bool :=
  match s with
    |	EmptyString => true
    | _ => false
  end.

Fixpoint string_iterate {S : Type} (f : ascii -> S -> S) (state : S) (s : string) : S :=
  match s with
    | EmptyString => state
    | String c s =>
      let state := f c state in
      string_iterate f state s
  end.

Definition nl := String char.nl EmptyString.
Definition tab := String char.tab EmptyString.

Fixpoint escape_string (s : String.string) : String.string :=
  match s with
    | String c s =>
      if (char.is_alnum c || char.eqb c "."%char)%bool then
        String c (escape_string s)
      else
        String "\" (String c (escape_string s))
    | StringEmpty => StringEmpty
  end.

(* remove "prefix" from "name" and return the residual suffix; returns
None if "prefix" is not actually a prefix of "name" *)
Fixpoint get_suffix (name : String.string) (prefix : String.string) : option String.string :=
  match name, prefix with
    | _, EmptyString => Some name
    | String x name', String y prefix' =>
      if Ascii.eqb x y then get_suffix name' prefix' else None
    | _, _ => None
  end.
Lemma get_suffix_spec :
    forall name prefix suffix,
    get_suffix name prefix = Some suffix <->
    name = (prefix ++ suffix) % string.
  induction name ; simpl.
    destruct prefix ; intros suffix ; apply conj ; intros H ; simpl in * ; [
      inversion H ; simpl ; auto |
      congruence |
      discriminate |
      discriminate].

    destruct prefix ; intros suffix ; apply conj ; intros H ; simpl in * ; [
      congruence |
      congruence |

      case_eq (Ascii.eqb a a0) ; intros H1 ; rewrite H1 in * ; try discriminate ;
      apply Ascii.eqb_eq in H1 ;
      apply IHname in H ;
      congruence |

      injection H ; intros H1 H2 ;
      apply Ascii.eqb_eq in H2 ; rewrite H2 ;
      apply IHname ; auto ;
      case_eq (Ascii.eqb a0 a0) ;
      destruct (Ascii.eqb a0 a0)
    ].
Qed.

Lemma string_append_rev :
    forall x y z, (x ++ y = x ++ z) % string -> y = z.
  induction x.
    auto.
    intros y z H. apply IHx ; simpl in H ; inversion H ; auto.
Qed.

Fixpoint make_underscores (n : nat) : String.string :=
  match n with
    | S n => String "_" (make_underscores n)
    | O => EmptyString
  end.

Theorem make_underscores_rev :
    forall x y, make_underscores x = make_underscores y -> x = y.
  induction x.
    destruct y ; intros ; tauto || discriminate.

    destruct y ; [intros ; tauto || discriminate|].
    simpl ; intros H ; injection H ; clear H ; intros H.
    apply IHx in H ; congruence.
Qed.


Fixpoint count_underscores (s : String.string) : option nat :=
  match s with
    | String c s =>
      if char.eqb "_"%char c
      then 
        match count_underscores s with
          | Some n => Some (S n)
          | None => None
        end
      else
        None
    | EmptyString => Some O
  end.

Lemma underscores_spec :
    forall s n, make_underscores n = s <-> count_underscores s = Some n.
  induction s.
    destruct n ; [tauto| apply conj ; discriminate].

    destruct n ; [
      unfold count_underscores ; fold count_underscores ; destruct (char.eqb _ _) ;
      destruct (count_underscores _) ; apply conj ; discriminate
    |].

    unfold count_underscores ; fold count_underscores.
    unfold make_underscores ; fold make_underscores.
    case_eq (char.eqb "_"%char a) ; intros H.
      apply conj.
        intros X ; inversion X.
        rewrite H2 ; apply IHs in H2 ; rewrite H2 ; auto.

        intros X.
        apply char.eqb_eq in H ; rewrite <- H in *.
        case_eq (count_underscores s) ; [|intros Z ; rewrite Z in * ; discriminate].
        intros k Z ; rewrite Z in X.
        apply IHs in Z ; rewrite <- Z. congruence.

      apply conj ; intros X ; inversion X as [H1] ; rewrite <- H1 in * ; simpl in * ; unfold char.eqb in * ; simpl in * ; discriminate.
Qed.

Definition get_suffix_nat (name : String.string) (prefix : String.string) : option nat :=
  match get_suffix name prefix with
    | Some suffix => count_underscores suffix
    | None => None
  end.
Lemma get_suffix_nat_spec :
    forall name prefix n,
    get_suffix_nat name prefix = Some n <->
    name = (prefix ++ make_underscores n) % string.
  intros name prefix n ; unfold get_suffix_nat.
  case_eq (get_suffix name prefix) ; [intros suffix H1 | intros H1] ; apply conj ; intros H2.
    apply get_suffix_spec ; apply underscores_spec in H2 ; congruence.

    apply get_suffix_spec in H1 ; rewrite H1 in H2.
    apply string_append_rev in H2 ; rewrite H2.
    apply underscores_spec ; auto.

    discriminate.

    apply get_suffix_spec in H2 ; congruence.
Qed.

Fixpoint unique_nat_suffix (prefix : String.string) (n : nat) (l : list String.string) : nat :=
  match l with
    | nil => n
    | cons name l' =>
      let sn := get_suffix_nat name prefix in
      let n := match sn with
        | Some sn =>
          if le_dec n sn then S sn else n
        | None => n
      end in
      unique_nat_suffix prefix n l'
  end.
Lemma unique_nat_suffix_le :
    forall prefix n l,
    n <= unique_nat_suffix prefix n l.
  intros prefix n l ; revert n.
  induction l.
    auto.

    simpl. destruct (get_suffix_nat _ _) ; [|auto].
    intros k ; destruct (le_dec _ _) ; [|auto].
    apply le_trans with (m := S n) ; [omega | auto]. 
Qed.

Lemma unique_nat_suffix_le2:
    forall prefix k n l,
    unique_nat_suffix prefix k l <= n -> k <= n.
  intros prefix k n l H.
  generalize (unique_nat_suffix_le prefix k l) ; intros X ; omega.
Qed.

Lemma unique_nat_suffix_monotonic:
    forall prefix k n l,
    k <= n -> unique_nat_suffix prefix k l <= unique_nat_suffix prefix n l.
  intros prefix k n l ; revert k n.
  induction l ; intros k n H.
    auto.

    simpl.
    destruct get_suffix_nat ; repeat destruct (le_dec _ _) ; auto ; apply IHl ; omega.
Qed.

Lemma unique_nat_suffix_spec :
    forall prefix k n l,
    unique_nat_suffix prefix k l <= n ->
    ~ In (prefix ++ make_underscores n) % string l.
  intros prefix k n l ; revert n.
  induction l.
    simpl ; tauto.

    simpl ; intros n.
    intros H1 [H2|H2].
      apply get_suffix_nat_spec in H2.
      rewrite H2 in *.
      apply unique_nat_suffix_le2 in H1.
      destruct (le_dec _ _) ; omega.

      apply IHl in H2 ; auto.
      case_eq (get_suffix_nat a prefix).
        intros j Hj ; rewrite Hj in H1.
        destruct (le_dec _ _) ; [|auto].
        apply le_trans with (m := unique_nat_suffix prefix (S j) l) ; auto ; apply unique_nat_suffix_monotonic ; omega.

        intros X ; rewrite X in H1 ; auto.
Qed.

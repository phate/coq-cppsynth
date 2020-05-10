Class Monad (m : Type -> Type): Type :=
{
  (* the two monadic operations *)
  return_: forall A, A -> m A;
  bind: forall A, m A -> forall B, (A -> m B) -> m B;

  (* equivalence relation -- for most Monads this is equality, for some 
  it is going to be a slightly weaker relation (like point-wise
  functional equality) *)
  equiv : forall A (x y : m A), Prop;
  equiv_refl : forall A x, equiv A x x;
  equiv_sym : forall A x y, equiv A x y -> equiv A y x;
  equiv_trans : forall A x y z, equiv A x y -> equiv A y z -> equiv A x z;
  (* we could make a statement that equivalence is equality under functional
  extensionality:
  equiv_eq : 
    (forall A (B : A -> Type) (f g : forall a, B a), (forall a, f a = g a) -> f = g) ->
    forall A x y, equiv A x y -> x = y; *)

  (* properties under equivalence relation *)
  right_unit: forall A (a : m A), equiv _ a (bind A a A (return_ A));
  left_unit: forall A (a : A) B (f: A -> m B),
             equiv _ (f a) (bind A (return_ A a) B f);
  associativity: forall A (ma : m A) B f C (g : B -> m C),
                 equiv _ (bind A ma C (fun x => bind B (f x) C g)) (bind B (bind A ma B f) C g)
}.

Notation "a >>= f" := (bind _ a _ f) (at level 50, left associativity).
Notation "a >> f" := (a >>= fun _ => f) (at level 50, left associativity).
Notation "'do' a <-- e ; c" := (e >>= (fun a => c)) (at level 60, right associativity).
Notation "'pass'" := (fun t => (tt, t)).

Instance Maybe : Monad option :=
{| 
  return_ := @Some;
  bind := fun A m B f => match m with None => None | Some a => f a end ;
  equiv := fun A x y => x = y
|}.
  (* equiv_refl *) auto.
  (* equiv_sym *) auto.
  (* equiv_trans *) intros ; congruence.
  (* unit_left *) intros A a ; case a ; split.
  (* unit_right *) split.
  (* associativity *) intros A m B f C g ; case m ; split.
Defined.

Require Import List.

Definition foo (x y z : list nat) (i : nat) : option nat :=
  do xi <-- nth_error x i ;
  do yi <-- nth_error y i ;
  do zi <-- nth_error z i ;
  return_ _ (xi + yi + zi).

Require Import Coq.extraction.ExtrOcamlString.
Require Import Coq.extraction.ExtrOcamlBasic.
Require Import Coq.extraction.ExtrOcamlNatInt.

Recursive Extraction foo.


Example q : foo (1 :: 2 :: nil) (2 :: 0 :: nil) (3 :: 4 :: nil) 1 = Some 6 := eq_refl.


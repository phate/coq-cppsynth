Require Import
  CPPSynth.Monad.

(* Example wrapping "optional" into a monad. This is not actually used
   but serves as a demonstrator for the similarly-minded "ExceptionMonad". *)

#[refine] Instance Maybe : CPPSynth.Monad.Monad option :=
{| 
  CPPSynth.Monad.return_ := @Some;
  CPPSynth.Monad.bind := fun A m B f => match m with None => None | Some a => f a end ;
  CPPSynth.Monad.equiv := fun A x y => x = y
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

Example test_list : 
    foo (1 :: 2 :: nil) (2 :: 0 :: nil) (3 :: 4 :: nil) 1 = Some 6
  := eq_refl.

Require Import Coq.extraction.ExtrOcamlString.
Require Import Coq.extraction.ExtrOcamlBasic.
Require Import Coq.extraction.ExtrOcamlNatInt.

Unset Printing Implicit.
Unset Printing Notations.

Print bind.
Print foo.
Recursive Extraction foo.

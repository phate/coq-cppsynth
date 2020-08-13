Require 
  CPPSynth.Monad.

#[refine] Instance StateMonad (S : Type) : CPPSynth.Monad.Monad (fun A => S -> A * S) :=
{|
  CPPSynth.Monad.return_ := fun A (a : A) (s : S) => (a, s) ;
  CPPSynth.Monad.bind := fun A m B f s => let (a, s) := m s in f a s ;
  CPPSynth.Monad.equiv := fun A x y => (forall z, x z = y z)
|}.
  (* equiv_refl *) auto.
  (* equiv_sym *) auto.
  (* equiv_trans *) intros ; congruence.
  (* unit_left *) intros ; simpl ; destruct (a z) ; auto.
  (* unit_right *) intros ; simpl ; auto.
  (* associativity *) intros ; simpl ; destruct (ma z) ; auto.
Defined.

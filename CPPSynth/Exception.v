Require 
  Coq.Strings.String
  CPPSynth.Monad.

(* Wrap a value alternatively allowing an exception to be represented.
   This is conceptually basically the same as "optional", but it can
   carry a description of the error that occurred if no value was produced.
   In extraction, this wrapping will be represented using "proper" exceptions. *)
Inductive ExceptionOr (T : Type) : Type :=
  | Exception : forall (cls msg : String.string), ExceptionOr T
  | Okay : T -> ExceptionOr T.

Arguments Exception {T}.
Arguments Okay {T}.

Instance ExceptionMonad : CPPSynth.Monad.Monad ExceptionOr :=
{| 
  CPPSynth.Monad.return_ := @Okay;
  CPPSynth.Monad.bind := fun A m B f => 
    match m with 
      | Exception cls msg => @Exception B cls msg 
      | Okay a => f a 
    end ;
  CPPSynth.Monad.equiv := fun A x y => x = y
|}.
  (* equiv_refl *) auto.
  (* equiv_sym *) auto.
  (* equiv_trans *) intros ; congruence.
  (* unit_left *) intros A a ; case a ; split.
  (* unit_right *) split.
  (* associativity *) intros A m B f C g ; case m ; split.
Defined.

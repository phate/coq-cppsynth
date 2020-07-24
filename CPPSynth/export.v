Require 
  CPPSynth.SExpression
  CPPSynth.StringUtil.

Require Import ExportSExpr.ExportSExpr.

ExportSExpr CPPSynth.SExpression.

Class C1 (T : Set) : Set :=
{
  method1 : forall (t : T), bool * T ;
  method2 : forall (x : nat) (t : T), T
}.

Inductive acc_state : Set :=
  | acc_state_make : forall (n : nat), acc_state.

Instance C1A : C1 acc_state :=
{|
  method1 := fun (t : acc_state) => match t with | acc_state_make O => (true, t) | _ => (false, t) end ;
  method2 := fun (x : nat) (t : acc_state) => match t with | acc_state_make y => acc_state_make (x + y) end 
|}.

ExportSExpr method1.

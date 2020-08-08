(* Class to establish isomorphism of "open-declared" vs. generic lists
of things. This is used as a convenience to make list ops and theorems
available to open-coded lists, while open-coded lists are used for the
convenience of having fully mutually inductive definitions. *)

Class ListClass (E L : Set) : Set :=
{
  to_list : L -> list E ;
  from_list : list E -> L ;
  from_to : forall l, from_list (to_list l) = l ;
  to_from : forall l, to_list (from_list l) = l
}.

Require Import List.

Definition nth_error {E L : Set} {G : ListClass E L} (l : L) (n : nat) : option E :=
  List.nth_error (to_list l) n.

Definition app {E L : Set} {G : ListClass E L} (l1 l2 : L) : L :=
  from_list (List.app (to_list l1) (to_list l2)).

Definition length {E L : Set} {G : ListClass E L} (l : L) : nat :=
  List.length (to_list l).

Theorem app_length {E L : Set} {G : ListClass E L}
    : forall l1 l2, length (app l1 l2) = length l1 + length l2.
  intros ; unfold app ; unfold length ; rewrite to_from ; apply List.app_length.
Qed.

Theorem nth_error_None {E L : Set} {G : ListClass E L}
    : forall l n, nth_error l n = None <-> length l <= n.
  intros l n ; unfold nth_error.
  apply List.nth_error_None.
Qed.

Theorem nth_error_app1 {E L : Set} {G : ListClass E L}
    : forall l1 l2 n, n < length l1 ->
      nth_error (app l1 l2) n = nth_error l1 n.
  intros l1 l2 n H ; unfold nth_error, app ; rewrite to_from ; apply List.nth_error_app1 ; auto.
Qed.

Theorem nth_error_app2 {E L : Set} {G : ListClass E L}
    : forall l1 l2 n, length l1 <= n ->
      nth_error (app l1 l2) n = nth_error l2 (n - length l1).
  intros l1 l2 n H ; unfold nth_error, app, length in * ; rewrite to_from ; apply List.nth_error_app2 ; auto.
Qed.

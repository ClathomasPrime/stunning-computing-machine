
Inductive bool : Type :=
  | true : bool
  | false : bool
  .

Definition not (b : bool) : bool :=
  match b with
  | true => false
  | false => true
  end.

Example nottrue:
  not true = false.
Proof. simpl. reflexivity. Qed.

Inductive void : Type :=
  .

Inductive list (a : Type) : Type :=
  | Nil : list a
  | Cons : a -> list a -> list a
  .

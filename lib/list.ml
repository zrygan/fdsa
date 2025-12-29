(* List Function Definitions *)

(* Helper Functions *)

(** [lhas l x] checks if an entry [x] exists in the list [l].
    @return [true] if [x] in [l]; [false] otherwise *)
let rec lhas l x =
  match l with [] -> false | h :: t -> if h == x then true else lhas t x

(** [lprn] prints each entry in the list [l]. *)
let rec lprn l =
  match l with
  | [] -> print_newline ()
  | h :: t ->
      print_int h;
      lprn t

(*  These functions are defined in the
    Functional Data Structures and Algorithms (FDSA) by Nipkow
    in Appendix A 'List Library'  
*)

(** [llen l] is the length of [l].
    @see "FDSA" , p. 343
    @return The length of [l] *)
let rec llen l = match l with [] -> 0 | _ :: t -> 1 + llen t

(** [lcon l1 l2] concatenates [l2] to [l1] (i.e., [l1 @ l2])
    @see "FDSA" , p. 343
    @return The list [l1 @ l2]. *)
let rec lcon l1 l2 = match l1 with [] -> l2 | h :: t -> h :: lcon t l2

(** [lset l s] transforms a list [l] to a deduplicated list (set) [s]
    @see "FDSA" , p. 343
    @return A deduplicated list [s] *)
let rec lset l s =
  match l with
  | [] -> s
  | h :: t -> if lhas s h then lset t s else lset t (h :: s)

(** [lfil l f r] filters a list [l] using a filter function [f] then returns a
    filtered list [r].
    @see "FDSA" , p.343
    @return A filtered list [r]. *)
let rec lfil l f =
  match l with [] -> [] | h :: t -> if f h then h :: lfil t f else lfil t f

(** [ltke l n] takes the first [n] elements of [l]. That is, for some list [l]
    we get the slice [l[:n]].
    @see "FDSA" , p. 343 *)
let rec ltke l n =
  match (n, l) with _, [] -> [] | 0, _ -> [] | n, h :: t -> h :: ltke t (n - 1)

(** [ldrp l n] drops the first [n] elements of [l]. That is, for some list [l]
    we get the slice [l[n+1:]].
    @see "FDSA" , p. 343 *)
let rec ldrp l n =
  match (n, l) with _, [] -> [] | 0, l -> l | n, _ :: t -> ldrp t (n - 1)

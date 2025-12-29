(* List Function Definitions *)

(* Helper Functions *)

(** [lhas l x] checks if an entry [x] exists in the list [l].
    @return [true] if [x] in [l]; [false] otherwise
*)
let rec lhas l x =
  match l with [] -> false | h :: t -> if h == x then true else lhas t x

(** [lprn] prints each entry in the list [l].
*)
let rec lprn l =
    match l with
    | [] -> print_newline ()
    | h::t -> print_int h; lprn t;;

(*  These functions are defined in the
    Functional Data Structures and Algorithms (FDSA) by Nipkow
    in Appendix A 'List Library'  
*)

(** [llen l] is the length of [l].
    @see "FDSA" , p. 343
    @return The length of [l]
*)
let rec llen l = match l with [] -> 0 | _ :: t -> 1 + llen t

(** [appd l a] appends an entry [a] to a list [l].
    @see "FDSA" , p. 343
    @return A list with [a] appended
*)
let rec lapp l a = match l with [] -> a :: [] | h :: t -> h :: lapp t a
  
(** [lset l s] transforms a list [l] to a deduplicated list (set) [s]
    @see "FDSA", p. 343
    @return A deduplicated list [s]
*)
let rec lset l s =
  match l with
  | [] -> s
  | h :: t -> if lhas s h then lset t s else lset t (h :: s)

(** [lfil l f r] filters a list [l] using a filter function [f] then
    returns a filtered list [r].
    @see "FDSA", p.343
    @return A filtered list [r].
*)
let rec lfil l f r =
    match l with
    | [] -> r
    | h :: t -> if f h then lfil t f (h::r) else lfil t f r

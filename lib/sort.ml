(* Sorting Algorithms *)

(** [insert_in_sorted x l] inserts an element [x] in a sorted array [l]
    @see "FDSA" , p. 14
    @return
      An array with the element [x] inserted while keeping the array sorted. *)
let rec insert_in_sorted x l =
  match l with
  | [] -> [ x ]
  | h :: t -> if x <= h then x :: h :: t else h :: insert_in_sorted x t

(** [insertion_sort l] sorts an array [l] by insertion
    @see "FDSA" , p. 14
    @return A sorted array. *)
let rec insertion_sort l =
  match l with [] -> [] | h :: t -> insert_in_sorted h (insertion_sort t)

(** [quick_sort l] sorts an array [l] by quicksort
    @see "FDSA" , p. 14
    @return A sorted array. *)
let rec quick_sort l =
  match l with
  | [] -> []
  | h :: t ->
      let left = quick_sort (List.lfil t (fun y -> y < h)) in
      let right = quick_sort (List.lfil t (fun y -> y >= h)) in
      List.lcon (List.lcon left [ h ]) right

(** *)
let rec merge l r =
  match (l, r) with
  | [], r -> r
  | l, [] -> l
  | lh :: lt, rh :: rt ->
      if lh <= rh then lh :: merge lt (rh :: rt) else rh :: merge (lh :: lt) rt

let rec merge_sort l =
  let len = List.llen l in
  if len <= 1 then l
  else
    let left = List.ltke l (len / 2) in
    let right = List.ldrp l (len / 2) in
    merge (merge_sort left) (merge_sort right)

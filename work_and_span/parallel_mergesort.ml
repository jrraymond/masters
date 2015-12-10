(* Mergesort on trees which, if OCaml supported parallelism, could be done
 * in parallel. *)
exception INVARIANT_FAILED;;

type 'a tree = | E | N of 'a * int * 'a tree * 'a tree;;

let string_of_tree prntr t =
  let rec go t i =
    match t with
    | E -> String.make i '|' ^ "E\n"
    | N(x, s, l, r) -> 
        let ls = go l (i + 1) in
        let rs = go r (i + 1) in
        String.make i '|' ^ "N(" ^ prntr x ^ "," ^ prntr s ^ ")\n" ^ ls ^ rs
  in go t 0;;

(* returns the size of a tree, assuming lable is correct.  *)
let size t =
  match t with
  | E -> 0
  | N(_, s, _, _) -> s;;

(* returns min element in tree. Must not be called on empty tree *)
let rec get_min t =
  match t with
  | E -> raise INVARIANT_FAILED
  | N(x, _, E, r) -> (x, r)
  | N(x, s, l, r) ->
      let (m, l') = get_min l in
      (m, N(x, s - 1, l', r))

(* Returns a tuple of a tree of elements <= x and a tree of 
 * elements > x *)
let rec split_at x t =
  match t with
  | E -> (E, E)
  | N(y, _, l, r) when y <= x ->
      let (ltx, gtx) = split_at x r in
      (N(y, 1 + size ltx + size l, l, ltx), gtx)
  | N(y, _, l, r) ->
      let (ltx, gtx) = split_at x l in
      (ltx, N(y, 1 + size gtx + size r, gtx, r));;

(* returns a tree consisting o the values in t1 and t2.
 * Preserves order. *)
let rec merge t1 t2 =
  match t1 with
  | E -> t2
  | N(x, _, l, r) ->
      let (ltx, gtx) = split_at x t2 in
      N(x, size t1 + size t2, merge l ltx, merge r gtx);;

(* returns tree of n elements taken from t and the remaining tree *)
let rec take_and_drop n t =
  match (n, t) with
  | (0, _) -> (E, t)
  | (_, E) -> (E, E)
  | (_, N(x, _, l, r)) -> 
      if n <= size l
      then
        let (t, d) = take_and_drop n l in
        (t, N(x, 1 + size d + size r, d, r))
      else
        let (t, d) = take_and_drop (n - 1 - size l) r in
        (N(x, 1 + size l + size t, l, t), d);;

(* restores balance in tree: abs (size l - size r) <= 1.
 * Preservers order. *)
let rec rebalance t = 
  match t with
  | N(x, s, l, r) ->
      let (taken, dropped) = take_and_drop (s / 2) t in
      (match dropped with
      | E -> taken
      | _ ->
        let (root, dropped') = get_min dropped in
        N(root, s, rebalance taken, rebalance dropped'))
  | _ -> t;;

(* Sorts a tree. A tree is sorted if it is a binary search tree (duplicates allowed) *)
let rec mergesort t =
  match t with
  | E -> E
  | N(x, _, l, r) ->
      let t = rebalance (merge (merge (mergesort l) (mergesort r)) (N(x, 1, E, E))) in
      let _ = Printf.printf "%s\n\n" (string_of_tree string_of_int t) in
      t

let insert_unsorted t x =
  match t with
  | E -> N(x, 1, E, E)
  | N(y, s, l, r) -> N(x, s + 1, t, E)

let tree_of_list = List.fold_left insert_unsorted E

let t0 = tree_of_list [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
let t1 = tree_of_list [9; 8; 7; 6; 5; 4; 3; 2; 1; 0]
let t2 = tree_of_list [5; 15; 25; 35; 45; 50; 40; 30; 20; 10; 0]

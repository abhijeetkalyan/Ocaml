(*Implementation of List.map *)
exception Invalid_argument

let rec map f l = match l with
| [] -> []
| h::t -> (f h) :: map f t

(*Implementation of List.map2 *)
let rec map2 f l l' = if List.length l = List.length l' then 
match l, l' with
| [] , [] -> []
| h::t, h'::t' -> (f h h') :: map2 f t t'
else raise Invalid_argument

(*Implementation of List.fold_left *)
let rec fold_left f a l = match l with
| [] -> a
| h::t -> fold_left f (f a h) t

(*Implementation of List.fold_left2 *)
let rec fold_left2 f a l l' = if List.length l = List.length l' then
match l, l' with
| [], [] -> a
| h::t, h'::t' -> fold_left2 f (f a h h') t t'
else raise Invalid_argument

(*Implementation of List.fold_right*)
let rec fold_right f a l = match l with
| [] -> a
| h::t -> f h (fold_right f a t)

(*Implementation of List.fold_right2 *)
let rec fold_right2 f a l l' = if List.length l = List.length l' then
match l, l' with
| [], [] -> a
| h::t , h'::t' -> f h h' (fold_right2 f a t t')
else raise Invalid_argument

(*Implementation of List.tabulate from SML *)
let rec tabulate f k = match k with
| 0 -> []
| _ -> tabulate f (k-1) @ [(f k)]

(*Implementation of List.find from SML *)
let rec find f l = match l with
| [] -> None
| h::t -> if (f h) = true then Some(h) else find f t

(*Implementation of List.filter from SML *)
let rec filter f l = match l with
| [] -> []
| h::t -> if (f h) = true then h::filter f t else filter f t

(*Implementation of List.partition from SML *)
let rec partition f l = 
 let rec part_helper f l acc1 acc2 = match l with
 | [] -> (acc1, acc2)
 | h::t -> if (f h) = true then part_helper f t (h::acc1) acc2 else part_helper f t acc1 (h::acc2)
in part_helper f l [] [] 

let rec exists f l = match l with
| [] -> false
| h::t -> if (f h) = true then true else exists f t

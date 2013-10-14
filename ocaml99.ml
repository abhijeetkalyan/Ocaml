(*Write a function last : 'a list -> 'a option that returns the last element of a list.*)

let rec last l = match l with
| [] -> None
| h::[] -> Some(h)
| h::t -> last t

(*Find the last but one (last and penultimate) elements of a list.*)

let rec last_two l = match l with
| [] -> None
| h::x::[] -> Some(h,x)
| h::t -> last_two t

(*Find the k'th element of a list.*)

let rec at n l = match n, l with
| _, [] -> None
| 1, h::t -> Some(h)
| k, h::t -> at (k-1) t

(*Find the number of elements of a list.*)

let length l =
 let rec lengthhelper l acc = match l with
 | [] -> acc
 | h::t -> lengthhelper t (acc+1)
in lengthhelper l 0	

(*Reverse a list.*)

let rec rev l = match l with
| [] -> []
| h::t -> rev t @ [h]

(*Tail-recursive version*)
let rec rev_tr l = 
 let rec rev_tr' l acc = match l, acc with
 | [], [] -> []
 | [], h::t -> acc
 | h::t, _ -> rev_tr' t (h::acc)
in rev_tr' l []  

(*Find out whether a list is a palindrome.*)

let is_palindrome l = 
if l = rev l then true else false

(*IMPORTANT NOTE - in OCaml, use '=' not '==' to compare*)


(*Flatten a nested list structure.*)
type 'a node = | One of 'a | Many of 'a node list;;

let rec flatten l = match l with
| [] -> []
| (One x)::t -> x:: flatten t
| (Many xs)::t -> flatten xs @ flatten t


(*Eliminate consecutive duplicates of list elements.*)

let compress l = 
 let rec compresshelper l acc = match l, acc with
| [], acc -> acc 
| h::h'::t, _ -> if h <> h' then compresshelper (h'::t) (acc@[h])  else compresshelper (h'::t) (acc)
| h::[], _ -> compresshelper [] (acc@[h])
in compresshelper l []

(*Pack consecutive duplicates of list elements into sublists.*)

let rec pack l = match l with
| [] -> [[]]
| h::t -> [[]]

(*pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;*)

type 'a tree =  Empty | Node of 'a * 'a tree * 'a tree;;

let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;

let num_tree =  Node (3, Node (2, Node (1, Empty, Empty), Empty),
 Node (5, Empty, Node (7, Empty, Empty)));;

let rec numNodes t = match t with
| Empty -> 0
| Node(x,l,r) -> 1 + numNodes l + numNodes r 

let rec sum t = match t with 
| Empty -> 0
| Node(x,l,r) -> x + sum l + sum r

(*Construct a binary search tree from a list of integer numbers.*)
let rec insert t x = match t with
| Empty -> Node(x, Empty, Empty)
| Node(a,l,r) -> if x > a then Node(a,l,(insert r x)) else Node (a, (insert l x), r)

let construct l = List.fold_left insert Empty l


(*Count the leaves of a binary tree *)
let rec numLeaves t = match t with
|Empty -> 0
|Node(_, Empty, Empty) -> 1
|Node(_, l, r) -> numLeaves l + numLeaves r

(*Collect the leaves of a binary tree in a list *)
let rec leaves t = match t with
| Empty -> []
| Node(x, Empty, Empty) -> [x]
| Node(x, l, r) -> leaves l @ leaves r

(*Collect the internal(non-leaf) nodes of a binary tree in a list*)
let rec internals t = match t with
| Empty -> []
| Node(x, Empty, Empty) -> []
| Node(x,l,r) -> [x] @ internals l @ internals r

(*Collect the nodes at a given level in a list.*)
let rec at_level t c = match t with
| Empty -> []
| Node(x,l,r) -> if c = 1 then [x] else at_level l (c-1) @ at_level r (c-1)
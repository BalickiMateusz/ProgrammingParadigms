
(*Balicki Mateusz*)

(*Zadanie 2*)
let f1 x = List.hd [];;

let rec f2 y = f2 y;;

(*Typ danych dla drzew*)
type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

(*Zadanie 3*)
let breadthBT tree =
  let rec breadthTraversal childQueue =
    match childQueue with
        [] -> []
      | Empty :: t -> breadthTraversal t
      | Node(elem, left, right) :: t -> elem :: breadthTraversal (t @ [left; right])
  in 
    breadthTraversal [tree];;

let t1 = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty));;
let t2 = Node(1,Node(2,Empty,Node(3,Empty,Empty)),Empty);;
let t3 = Node(1, Empty, Empty);;
let t4 = Empty;;

breadthBT t1 = [1; 2; 3; 4; 5; 6];;
breadthBT t2 = [1; 2; 3];;
breadthBT t3 = [1];;
breadthBT t4 = [];;

(*Zadanie 4*)
let internalPath tree =
  let rec internalPath_h (tree, result) = 
    match tree with
        Empty -> 0
      |Node(value, leftSubtree, rightSubtree) -> result + internalPath_h(leftSubtree, result+1) + internalPath_h(rightSubtree, result+1)
  in 
    internalPath_h(tree, 0);;

internalPath t1 = 9;;
internalPath t2 = 3;;
internalPath t3 = 0;;
internalPath t4 = 0;;

let externalPath tree =
  let rec externalPath_h (tree, result) = 
    match tree with
        Empty -> result
      |Node(value, leftSubtree, rightSubtree) -> externalPath_h(leftSubtree, result+1) + externalPath_h(rightSubtree, result+1)
  in 
    externalPath_h(tree, 0);;

externalPath t1 = 21;;
externalPath t2 = 9;;
externalPath t3 = 2;;
externalPath t4 = 0;;

(*Typ danych dla grafów*)
type 'a graph = Graph of ('a -> 'a list);;

(*Zadanie 5*)
let depthSearch (Graph succ) startNode =
  let rec search visited queue =
    match queue with
        []->[]
      |h::t -> if List.mem h visited then search visited t
          else h::search(h::visited) (succ h @ t)
  in search [] [startNode];;

let g1 = Graph
           (function
               0 -> [3]
             | 1 -> [0;2;4]
             | 2 -> [1]
             | 3 -> []
             | 4 -> [0;2]
             | n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist")
           );;

let g2 = Graph
           (function
               0 -> [1;2]
             | 1 -> [2]
             | 2 -> [0;3]
             | 3 -> [3]
             | n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist")
           );;

let g3 = Graph
           (function
               0 -> [0;1]
             | 1 -> [1;0]
             | n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist")
           );;

depthSearch g1 4 = [4; 0; 3; 2; 1];;
depthSearch g1 1 = [1; 0; 3; 2; 4];;
depthSearch g2 2 = [2; 0; 1; 3];;
depthSearch g3 0 = [0; 1];;


(*Balicki Mateusz*)

(*Definicja list leniwych*)
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

(*Funkcje pomocnicze*)
let rec ltake = function 
    (0, _) -> [] 
  | (_, LNil) -> [] 
  | (n, LCons(x, lazy xs)) -> x :: ltake(n-1, xs);; 

let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;

let rec toLazyList = function 
    [] -> LNil 
  | x :: xs -> LCons(x, lazy (toLazyList xs));; 

(*Zadanie 1*)
let rec lrepeat k lxs =
  let rec repeat (k, x, xs)=
    match (k, x) with
        (1, _) -> xs
      |(n, x)-> LCons(x, lazy(repeat(n-1, x, xs)))
  in
    match (k, lxs) with
        (_, LNil) -> LNil
      |(0, _) -> LNil
      |(n, LCons(x, lazy xs)) -> repeat(n, x, LCons(x, lazy(lrepeat n xs)));;

ltake (12, lrepeat 0 (toLazyList ['a'; 'b'; 'c'; 'd'])) = [];;

ltake (12, lrepeat 1 (toLazyList ['a'; 'b'; 'c'; 'd'])) = ['a'; 'b'; 'c'; 'd'];;

ltake (12, lrepeat 3 (toLazyList ['a'; 'b'; 'c'; 'd'])) = ['a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c'; 'c'; 'c'; 'd'; 'd'; 'd'];;

ltake (15, lrepeat 3 (lfrom 1)) = [1; 1; 1; 2; 2; 2; 3; 3; 3; 4; 4; 4; 5; 5; 5];;

ltake (15, lrepeat 3 (toLazyList [])) = [];;

(*Zadanie 2*)
let lfib =
  let rec lfibonacci a b =
    LCons(a+b, lazy(lfibonacci b (a+b)))
  in
    LCons(0, lazy(LCons(1, lazy(lfibonacci 0 1))));;

ltake (5, lfib) = [0;1;1;2;3];;
ltake (0, lfib) = [];;
ltake (10, lfib) = [0;1;1;2;3;5;8;13;21;34];;
ltake (2, lfib) = [0;1];;

(*Definicja polimorficznego leniwego drzewa binarnego*)
type 'a lBT = LEmpty  |  LNode of  'a * (unit ->'a lBT) * (unit -> 'a lBT);;

(*Zadanie 3*)

(*a)*)
let lBreadth ltree =
  let rec lbreadthTraversal = function
      [] -> LNil
    |LEmpty :: t -> lbreadthTraversal t
    |LNode(elem, lleft, lright) :: t -> LCons(elem, lazy(lbreadthTraversal(t @ [lleft(); lright()])));
  in lbreadthTraversal [ltree];;

(*b)*)
let rec lTree n =
  LNode(n, (fun() -> lTree (2*n)), (fun() -> lTree (2*n+1)));;

let x = lTree 1;;
let y = lTree 6;;
let lt3 = LNode(1, (fun() -> LEmpty), (fun() ->LNode(2, (fun() -> LEmpty), (fun() ->LNode(4, (fun() -> LEmpty), (fun() -> LEmpty))))));;

ltake (17, lBreadth x) = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17];;
ltake (8, lBreadth y) = [6; 12; 13; 24; 25; 26; 27; 48];;
ltake (7, lBreadth LEmpty) = [];;
ltake (3, lBreadth lt3) = [1; 2; 4];;


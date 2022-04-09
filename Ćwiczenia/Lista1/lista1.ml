(*Balicki Mateusz*)

(*Zadanie 1*)

let rec flatten1 a=
  if a=[] then []
  else List.hd a @ flatten1(List.tl a);;

flatten1([[4;5];[1;2;3]]);;
flatten1([[4.2;5.];[1.3;2.4;3.2]]);;
flatten1([['a';'b'];['c';'d';'e']]);;
flatten1([["Ala";"Adam"];["Andrzej";"Anastazja";"Apollo"]]);;
flatten1([[];[]]);;
flatten1([[]]);;
flatten1([]);;

(*Zadanie 2*)

let rec count (x, xs)=
  if xs=[] then 0
  else if List.hd xs=x then 1+count(x, List.tl xs)
  else count(x, List.tl xs);;

count('A',['A';'L';'A']);;
count('A',[]);;
count(1,[1;2;3;8;11;12]);;
count("Jan",["Adam"; "Bozena"; "Jan"; "Stefan"; "Jan"]);;
count(5.5, [11.2;12.6;13.5;20.5;5.5;5.5;12.3;5.5]);;
(*count([],['A';'L';'A']);;*)
count([],[]);;
count('A',[]);;

(*Zadanie 3*)

let rec replicate (x, n)=
  if n>0 then x :: replicate(x, n-1)
  else if n=0 then []
  else failwith "Podano ujemna liczbe powtorzen!";;

replicate('a', 3);;
(*replicate('a', -8);;*)
replicate('a', 0);;
replicate("", 5);;
replicate(2, 8);;
replicate(5.5, 8);;
replicate("Ala", 2);;
replicate([], 2);;
(*replicate("Ala", []);;*)
(*replicate([], []);;*)

(*Zadanie 4*)

let rec sqrList (xs)=
  if xs=[] then []
  else (List.hd xs * List.hd xs) :: sqrList(List.tl xs);;

sqrList([1;2;3;8;11;12]);;
sqrList([0;1;-2;-3;-8;-11;-12]);;
sqrList([]);;

(*Zadanie 5*)

let palindrome (xs)=
  if xs=[] then failwith "Brak listy lub pusta lista!"
  else xs=List.rev xs;;

palindrome(['A';'L';'A']);;
palindrome(['A';'L';'F']);;
(*palindrome([]);;*)
palindrome([5;3;5]);;
palindrome([4;3;5]);;
palindrome([5.3;3.6;5.3]);;
palindrome([5.1;3.6;5.3]);;
palindrome(["Adam"; "Bozena";"Adam"]);;
palindrome(["Adam"; "Bozena";"Ala"]);;

(*Zadanie 6*)

let rec listLength (xs)=
  if xs=[] then 0
  else listLength(List.tl xs)+1;;

listLength([5;3;5]);;
listLength([2]);;
listLength([]);;
listLength([5.3;3.6;5.3]);;
listLength(["Adam"; "Bozena";"Adam"]);;
listLength([""]);;
listLength(['A';'L';'F']);;
listLength(['a']);;

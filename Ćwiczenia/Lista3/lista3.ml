(*Balicki Mateusz*)

(*Zadanie 3*)

let sumProd xs=
  List.fold_left (fun (x1,x2) h -> (x1+h, x2*h))(0,1) xs;;

sumProd([1;2;3;4;5]) = (15, 120);;
sumProd([-1;2;3;4;-5]) = (3, 120);;
sumProd([0;0;0;0;0]) = (0, 0);;
sumProd([]) = (0, 1);;



(*Balicki Mateusz*)

(*Zadanie 2*)

let rec fib x=
  if x<0 then failwith "Wrong number!"
  else if x <= 1 then x
  else fib(x-1) + fib(x-2);;

fib(10);;

let fibTail x=
  if x<0 then failwith "Wrong number!"
  else
    let rec fibTailIter(x, y, ac)=
      if x=0 then y
      else if x=1 then ac
      else fibTailIter(x-1, ac, y+ac)
    in fibTailIter(x, 0, 1);;

fib(10) = 55;;
fibTail(42) = 267914296;;
(*fib(-1);;*)

(*Zadanie 4*)

let xsA = [-2;-1;0;1;2];;
let [_; _; x; _; _] = xsA;;
let _::_::x::_ = xsA;;

let xsB = [(1,2);(0,1)];;
let [(_,_);(x,_)] = xsB;;
let _::[(x,_)] = xsB;;*)

(*Zadanie 5*)

let rec initSegment(xs, ys)=
  match (xs, ys) with
      ([],_) -> true
    |(_,[]) -> false
    |(h1::t1, h2::t2) ->
        if h1=h2 then initSegment(t1, t2)
        else false;;

initSegment([1;2;3], [1;2;3;4;5;6]) = true;;
initSegment([1;2;3], [2;2;3;4;5;6]) = false;;
initSegment([], [1;2;3;4;5;6]) = true;;
initSegment([1;2;3], []) = false;;
initSegment([], []) = true;;

(*Zadanie 6*)


let rec replaceNth(xs, n, x)=
  match (xs, n) with
      ([],_) -> []
    |(h::t,0) -> x::t
    |(h::t,_) -> h::replaceNth(t, n-1, x);;

replaceNth(['o';'l';'a';'m';'a';'k';'o';'t';'a'], 1, 's') = ['o';'s';'a';'m';'a';'k';'o';'t';'a'];;
replaceNth(['o';'l';'a';'m';'a';'k';'o';'t';'a'], -2, 's') = ['o';'l';'a';'m';'a';'k';'o';'t';'a'];;
replaceNth([], 1, 's') = [];;
replaceNth(['o';'l';'a';'m';'a';'k';'o';'t';'a'], 15, 's') = ['o';'l';'a';'m';'a';'k';'o';'t';'a'];;









import scala.annotation.tailrec

//Balicki Mateusz

//Zadanie 2
val fib: Int => Int = x =>
  if(x<0) then throw new Exception("Wrong number!")
  else if(x <= 1) then x
  else fib(x-1) + fib(x-2)

fib(10)
//fib(42) == 267914296
//fib(-1)

val fibTail: Int => Int = x =>
  if(x<0) then throw new Exception("Wrong number!")
  else
    @tailrec
    def fibTailIter (x: Int, y: Int, ac: Int): Int=
      if(x == 0) then y
      else if(x == 1) then ac
      else fibTailIter(x-1, ac, y + ac)
    fibTailIter(x, 0, 1)

//fibTail(10) == 55
//fibTail(42) == 267914296
//fibTail(-1)

//Zadanie 4
def matchA[A](k: List[A])=
  val List(_, _, x, _, _) = k
  //val _:: _:: x::_ = k
  x

matchA(List(-2,-1,0,1,2)) == 0

def matchB[A](k: List[A])=
  //val List((_, _), (x, _)) = k
  val _::List((x,_)) = k
  x

matchB(List((1, 2), (0, 1))) == 0

//Zadanie 5
def initSegment[A](xs: List[A], ys: List[A]):Boolean=
  (xs, ys) match
    case (Nil,_) => true
    case (_, Nil) => false
    case (h1::t1, h2::t2) =>
      if(h1 == h2) then initSegment(t1, t2)
      else false

initSegment(List(1,2,3), List(1,2,3,4,5,6)) == true
initSegment(List(1,2,3), List(2,2,3,4,5,6)) == false
initSegment(List(), List(1,2,3,4,5,6)) == true
initSegment(List(1,2,3), List()) == false
initSegment(List(), List()) == true

//Zadanie 6 a
def replaceNth[A](xs: List[A], n: Int, x: A):List[A] =
  (xs, n) match {
    case (Nil, _) => Nil
    case (h::t, 0) => x :: t
    case (h::t, _) => h :: replaceNth(t, n-1, x)
  }

replaceNth(List('o','l','a','m','a','k','o','t','a'), 1, 's') == List('o','s','a','m','a','k','o','t','a')
replaceNth(List('o','l','a','m','a','k','o','t','a'), -2, 's') == List('o','l','a','m','a','k','o','t','a')
replaceNth(List(), 1, 's') == List()
replaceNth(List('o','l','a','m','a','k','o','t','a'), 15, 's') == List('o','l','a','m','a','k','o','t','a')

val przedzial: Int => Int => List[Int] = a => b => if a > b then Nil else b :: (przedzial(a)(b-1))


//Balicki Mateusz

//Zadanie 1

def flatten1[A](xss: List[List[A]]): List[A]=
  if xss==Nil then Nil
  else xss.head ::: flatten1(xss.tail)

flatten1(List(List(5,6), List(1,2,3)))
flatten1(List(List(5.6,6.3), List(1.2,2.5,3.7)))
flatten1(List(List('a','b'), List('c','d','e')))
flatten1(List(List("Ala","Adam"), List("Andrzej","Anastazja","Apollo")))
flatten1(List(List(), List()))
flatten1(List())
flatten1(Nil)

//Zadanie 2

def count[A](x:A,xs: List[A]): Int =
  if xs==Nil then 0
  else if xs.head==x then 1 + count(x, xs.tail)
  else count(x, xs.tail)

count('A', List('A','L','A'))
count('A', List())
count(1, List(1,2,3,8,11,12))
count("Jan", List("Adam", "Bozena", "Jan", "Stefan", "Jan"))
count(5.5, List(11.3,2.8,3.4,5.56,5.5,5.5))
count(Nil, List('A','L','A'))
count(Nil, Nil)
count('A', Nil)

//Zadanie 3

def replicate[A](x:A, n:Int): List[A]=
  if n>0 then x :: replicate(x, n-1)
  else if n==0 then Nil
  else throw new Exception ("Podano ujemna liczbe powtorzen!")

replicate('a', 3)
//replicate('a', -8)
replicate('a', 0)
replicate("", 5)
replicate(2, 8)
replicate(5.5, 8)
replicate("Ala", 2)
replicate(Nil, 2)
//replicate("Ala", Nil)
//replicate(Nil, Nil)

//Zadanie 4 jako metoda

def sqrList(xs:List[Int]): List[Int]=
  if xs==Nil then Nil
  else (xs.head * xs.head) :: sqrList(xs.tail)

sqrList(List(1,2,3,8,11,12))
sqrList(List())
sqrList(Nil)

//Zadanie 4 jako funkcja

val sqrListF = (xs: List[Int]) =>
  if xs==Nil then Nil
  else (xs.head * xs.head) :: sqrList(xs.tail)

sqrListF(List(1,2,3,8,11,12))
sqrListF(List(0,1,-2,-3,-8,-11,-12))
sqrListF(List())
sqrListF(Nil)

//Zadanie 5

def palindrome[A](xs: List[A]): Boolean=
  if xs==Nil then throw new Exception ("Brak listy lub pusta lista!")
  else xs==xs.reverse

palindrome(List('A','L','A'))
palindrome(List('A','L','F'))
//palindrome(List())
palindrome(List(5,3,5))
palindrome(List(4,3,5))
palindrome(List(5.3,3.6,5.3))
palindrome(List(4.2,3.8,5.2))
palindrome(List("Adam", "Bozena", "Adam"))
//palindrome(Nil)

//Zadanie 6

def listLength[A](xs: List[A]): Int=
  if xs==Nil then 0
  else listLength(xs.tail)+1

listLength(List(5,3,5))
listLength(List(2))
listLength(List())
listLength(List(5.3,3.6,5.3))
listLength(List("Adam", "Bozena", "Adam"))
listLength(List(""))
listLength(List('A','L','F'))
listLength(List('a'))
listLength(Nil)

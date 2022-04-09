
//Balicki Mateusz

//Zadanie 1
def lrepeat[A] (k: Int ) (lxs: LazyList[A]): LazyList[A] =
  def  repeat[A] (k: Int, x: A, xs: LazyList[A]): LazyList[A] =
    (k, x) match
      case (1, _) => xs
      case (n, x) => x #:: repeat(n-1, x, xs)
  (k, lxs) match
    case (_, LazyList()) => LazyList()
    case (0, _) => LazyList()
    case (n, (head #:: tail)) => repeat (n, head, head#::(lrepeat (n)(tail)))

lrepeat(0)(LazyList('a','b','c','d')).toList == List()
lrepeat(1)(LazyList('a','b','c','d')).toList == List('a', 'b', 'c', 'd')
lrepeat(3)(LazyList('a','b','c','d')).toList == List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd')
lrepeat(3)(LazyList.from(1)).take(15).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5)
lrepeat(3)(LazyList()).take(15).toList == List()

//Zadanie 2

val lfib: LazyList[Int] =
  def lfibonacci (a: Int, b: Int): LazyList[Int] =
    (a+b) #:: lfibonacci(b, (a+b))
  0 #:: 1 #:: lfibonacci(0, 1)

lfib.take(5).toList == List(0, 1, 1, 2, 3)
lfib.take(0).toList == List()
lfib.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
lfib.take(2).toList == List(0, 1)

//Definicja polimorficznego leniwego drzewa binarnego
sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

//Zadanie 3

//a)
def lBreadth[A] (ltree: lBT[A]): LazyList[A] =
  def lbreadthTraversal(childQueue: List[lBT[A]]): LazyList[A] =
    childQueue match
      case Nil => LazyList()
      case LEmpty::tail => lbreadthTraversal(tail)
      case LNode(elem, lleft, lright)::tail => elem #:: lbreadthTraversal(tail ::: (List(lleft(), lright())))
  lbreadthTraversal(List(ltree))

//b)
val lTree: Int => lBT[Int] = n =>
  LNode(n, (() => lTree(2*n)), (() => lTree(2*n+1)))

val x = lTree(1)
val y = lTree(6)
val lt3 = LNode(1, (() => LEmpty), (() => LNode(2, (() => LEmpty), (() => LNode(4, (() => LEmpty), (() => LEmpty))))));;

lBreadth(x).take(17).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
lBreadth(y).take(8).toList == List(6, 12, 13, 24, 25, 26, 27, 48)
lBreadth(LEmpty).take(8).toList == List()
lBreadth(lt3).take(3).toList == List(1, 2, 4)

def debugName (): Unit = println(this.getClass().getSimpleName())
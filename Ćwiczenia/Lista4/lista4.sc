
//Balicki Mateusz

//Typ danych dla drzew
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

//Zadanie 3
def breadthBT[A] (tree: BT[A]): List[A] =
  def breadthTraversal(childQueue: List[BT[A]]): List[A] =
    childQueue match
      case Nil => Nil
      case Empty :: t => breadthTraversal(t)
      case Node(elem, left, right)::t => elem::breadthTraversal (t ::: List(left, right))
  breadthTraversal (List(tree))

val t1 = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty));;
val t2 = Node(1,Node(2,Empty,Node(3,Empty,Empty)),Empty);;
val t3 = Node(1, Empty, Empty)
val t4 = Empty

breadthBT(t1) == List(1, 2, 3, 4, 5, 6)
breadthBT(t2) == List(1, 2, 3)
breadthBT(t3) == List(1)
breadthBT(t4) == List()

//Zadanie 4
def internalPath[A](tree: BT[A]) =
  def internalPath_h(tree: BT[A], result: Int): Int =
    tree match
      case Empty => 0
      case Node(elem, left, right) => result + internalPath_h(left, result+1) + internalPath_h(right, result+1)
  internalPath_h(tree, 0)

internalPath(t1) == 9
internalPath(t2) == 3
internalPath(t3) == 0
internalPath(t4) == 0

def externalPath[A](tree: BT[A]) =
  def externalPath_h(tree: BT[A], result: Int): Int =
    tree match
      case Empty => result
      case Node(elem, left, right) => externalPath_h(left, result+1) + externalPath_h(right, result+1)
  externalPath_h(tree, 0)

externalPath(t1) == 21
externalPath(t2) == 9
externalPath(t3) == 2
externalPath(t4) == 0

//Typ danych dla grafów
sealed trait Graphs[A]
case class Graph[A](succ: A=>List[A]) extends Graphs[A]

//Zadanie 5
def depthSearch[A] (g: Graph[A]) (startNode: A): List[A] =
  def search(visited: List[A])(successors: List[A]): List[A] =
    successors match
      case Nil => Nil
      case h::t =>
        if visited contains h then search(visited)(t)
        else h::search (h::visited)((g succ h) ::: t)
  search (Nil) (List(startNode))

val g1 = Graph((i: Int) =>
  i match
    case 0 => List(3)
    case 1 => List(0, 2, 4)
    case 2 => List(1)
    case 3 => Nil
    case 4 => List(0, 2)
    case n =>
      throw new Exception(s"Graph g: node $n doesn't exist"))

val g2 = Graph((i: Int) =>
  i match
    case 0 => List(1, 2)
    case 1 => List(2)
    case 2 => List(0, 3)
    case 3 => List(3)
    case n =>
      throw new Exception(s"Graph g: node $n doesn't exist"))

val g3 = Graph((i: Int) =>
  i match
    case 0 => List(0, 1)
    case 1 => List(1, 0)
    case n =>
      throw new Exception(s"Graph g: node $n doesn't exist"))

depthSearch (g1)(4) == List(4, 0, 3, 2, 1)
depthSearch (g1)(1) == List(1, 0, 3, 2, 4)
depthSearch (g2)(2) == List(2, 0, 1, 3)
depthSearch (g3)(0) == List(0, 1)
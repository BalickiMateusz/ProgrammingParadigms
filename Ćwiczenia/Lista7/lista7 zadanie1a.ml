(*Balicki Mateusz*)
(*Zadanie 1a*)

module type QUEUE_FUN=
sig
  type 'a t
  exception Empty of string
  val empty: unit -> 'a t
  val enqueue: 'a * 'a t -> 'a t
  val dequeue: 'a t -> 'a t
  val first: 'a t -> 'a
  val isEmpty: 'a t -> bool
end;;

module QueueA: QUEUE_FUN=
struct
  type 'a t = 'a list
  exception Empty of string

  let empty() = []

  let enqueue(e, q) = q @ [e]

  let dequeue(q) = 
    match q with
        head::tail -> tail
      |[] -> []

  let first(q) = 
    match q with
        head::_ -> head
      |[] -> raise (Empty "module QueueA: first")

  let isEmpty(q) = q = []
end;;

let queue1 = QueueA.enqueue("1", QueueA.enqueue("2", QueueA.empty()));;
let queue2 = QueueA.enqueue("1", QueueA.enqueue("2", QueueA.enqueue("3", QueueA.empty())));;

queue1 = QueueA.dequeue(queue2);;
QueueA.first(queue2) = "3";;
QueueA.isEmpty(queue2) = false;;
QueueA.isEmpty(QueueA.dequeue(QueueA.dequeue(queue1))) = true;;

(*Aksjomaty*)

let q = QueueA.enqueue("en", QueueA.enqueue("ex", QueueA.empty()));;
let empty = QueueA.empty();;

QueueA.isEmpty(QueueA.enqueue("e1", q)) = false;;
QueueA.isEmpty(empty) = true;;
QueueA.dequeue(QueueA.enqueue("e1", QueueA.enqueue("e2", q))) = QueueA.enqueue("e1", QueueA.dequeue(QueueA.enqueue("e2", q)));;
QueueA.dequeue(QueueA.enqueue("e1", empty)) = empty;;
QueueA.dequeue(empty) = empty;;
QueueA.first(QueueA.enqueue("e1", QueueA.enqueue("e2", q))) = QueueA.first(QueueA.enqueue("e2", q));;
QueueA.first(QueueA.enqueue("e1", empty)) = "e1";;
try QueueA.first(QueueA.empty()) with QueueA.Empty "module QueueA: first" -> true;;

(*Zadanie 1b*)

module type QUEUE_FUN=
sig
  type 'a t
  exception Empty of string
  val empty: unit -> 'a t
  val enqueue: 'a * 'a t -> 'a t
  val dequeue: 'a t -> 'a t
  val first: 'a t -> 'a
  val isEmpty: 'a t -> bool
  val equals: 'a t * 'a t -> bool
end;;

module QueueB: QUEUE_FUN=
struct
  type 'a t = 'a list * 'a list
  exception Empty of string

  let empty() = ([], [])

  let enqueue(e, q) = 
    match q with
        ([], []) -> ([e], [])
      |(list1, list2) -> (list1, e::list2)

  let equals(q1, q2) = 
    (fst q1) @ (List.rev (snd q1)) = (fst q2) @ (List.rev (snd q2))

  let dequeue(q) = 
    match q with
        (_::[], list2) -> (List.rev list2, [])
      |(_::tail, list2) -> tail, list2
      |([], []) -> ([], [])
      |([], list2) -> (List.rev list2, [])

  let first(q) = 
    match fst q with
        head::tail -> head
      |[] -> raise (Empty "module Queue': first")

  let isEmpty(q) = q = ([], [])

end;;

let queue1 = QueueB.enqueue("1", QueueB.enqueue("2", QueueB.empty()));;
let queue2 = QueueB.enqueue("1", QueueB.enqueue("2", QueueB.enqueue("3", QueueB.empty())));;

QueueB.equals(queue1, QueueB.dequeue(queue2));;
QueueB.first(queue2) = "3";;
QueueB.isEmpty(queue2) = false;;
QueueB.isEmpty(QueueB.dequeue(QueueB.dequeue(queue1))) = true;;

(*Aksjomaty*)

let q = QueueB.enqueue("e1", QueueB.enqueue("e2", QueueB.empty()));;
let empty = QueueB.empty();;

QueueB.isEmpty(QueueB.enqueue("e1", q)) = false;;
QueueB.isEmpty(empty) = true;;
QueueB.equals(QueueB.dequeue(QueueB.enqueue("e1", QueueB.enqueue("e2", q))), QueueB.enqueue("e1", QueueB.dequeue(QueueB.enqueue("e2", q))));;
QueueB.dequeue(QueueB.enqueue("e1", empty)) = empty;;
QueueB.dequeue(empty) = empty;;
QueueB.first(QueueB.enqueue("e1", QueueB.enqueue("e2", q))) = QueueB.first(QueueB.enqueue("e2", q));;
QueueB.first(QueueB.enqueue("e1", empty)) = "e1";;
try QueueB.first(QueueB.empty()) with QueueB.Empty "module QueueA: first" -> true;;

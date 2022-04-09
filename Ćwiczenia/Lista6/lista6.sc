//Balicki Mateusz
//Zadanie 2
val swap = (tab: Array[Int], i: Int, j: Int) =>
  val aux = tab(i)
  tab(i) = tab(j)
    tab(j) = aux

val choose_pivot = (tab: Array[Int], m: Int, n: Int) =>
  tab((m+n)/2)

val partition = (tab: Array[Int], l: Int, r: Int) =>
  var i = l
  var j = r
  val pivot = choose_pivot (tab, l, r)
  while i <= j do
    while tab (i) < pivot do
      i+=1
    while pivot < tab(j) do
      j-=1
    if i <= j then {swap (tab, i, j); i+=1; j-=1}
  end while
  (i, j)

val quick: (Array[Int], Int, Int) => Unit = (tab, l, r) =>
  if l < r then
    val (i, j) = partition (tab, l, r)
    if j-1 < r-i
    then {quick (tab, l, j); quick (tab, i, r)}
    else {quick (tab, i, r); quick (tab, l, j)}
  else ()

val quicksort = (tab: Array[Int]) =>
  quick (tab, 0, (tab.length-1))

val t1 = Array(4,8,1,12,7,3,1,9)
quicksort(Array(7,2,8,4,3,6,9))
quicksort(t1)
t1



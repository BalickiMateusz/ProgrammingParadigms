//Balicki Mateusz

//Zadanie 3
def sumProd(xs: List[Int]): (Int,Int)=
  xs.foldLeft(0, 1) ((ac, h) => (ac._1 + h, ac._2 *h))

sumProd(List(1,2,3,4,5)) == (15, 120)
sumProd(List(-1,2,3,4,-5)) == (3, 120)
sumProd(List(0,0,0,0,0)) == (0,0)
sumProd(List()) == (0,1)

//Balicki Mateusz

//Zadanie 4
import scala.collection.mutable
object Zadanie4:
  def main(args: Array[String]): Unit =
    var a = mutable.Seq(1, 2, 3)
    var b = mutable.Seq(3, 4)

    copy(a, b)
    System.out.println(a)

  def copy[T](dest: mutable.Seq[T], src: mutable.Seq[T]): Unit =
    (0 until ((src.size).min(dest.size))).foreach(i => dest.update(i, src(i)))


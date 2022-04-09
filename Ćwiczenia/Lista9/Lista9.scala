//Balicki Mateusz

//Zadanie 1
class Time(private var h: Int = 0):
  require(h < 24, s"Invalid parameter h = $h")
  if (h < 0) then h = 0
  def hour: Int = h // Akcesor
  def hour_=(newHour: Int): Unit = // Mutator
    require(newHour < 24, s"Invalid parameter newTime = $newHour")
    if(newHour < 0) then h = 0
    else h = newHour
end Time

//Zadanie 3

class Pojazd(private val manufacturer: String, private val model: String,
             private val productionYear: Int = -1, private var licenseNumber: String = ""):

  def this(manufacturer: String, model: String, licenseNumber: String) =
    this(manufacturer, model, -1, licenseNumber: String)

end Pojazd

object Testy:
  def main(args: Array[String]): Unit =
    val czas = new Time(8)
    System.out.println(czas.hour)
    czas.hour_=(3)
    System.out.println(czas.hour)
    czas.hour_=(-1)
    System.out.println(czas.hour)

    val auto1 = new Pojazd("Audi", "A4")
    val auto2 = new Pojazd("Volkswagen", "Golf", 2000)
    val auto3 = new Pojazd("Opel", "Astra", "WPR UU22")
    val auto4 = new Pojazd("BMW", "3", 2001, "WPR UU11")

//Zadanie 4
object UzycieWyjatkow:
  def main(args: Array[String]): Unit =
    try
      metoda1()
    catch
      case e: Exception =>
        System.err.println(e.getMessage + "\n")
        e.printStackTrace()

  def metoda1() =
    metoda2()

  def metoda2() =
    metoda3()

  def metoda3() =
    throw new Exception("Wyjatek zgloszony w metoda 3")

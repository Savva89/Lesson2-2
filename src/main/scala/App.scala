import scala.io.StdIn.readLine
import scala.annotation.tailrec

object App {
  def GetSalary(salary : Int,bonus: Float, eat: Int, tax: Int) : Float = ((salary *(100 + bonus))/100 +  eat) * (100 - tax) / 100

  def recdegrree(d:Int): Int =
  {
    if (d == 0) 1
    else 2*recdegrree(d-1)
  }


  def recdegrreetale(d: Int): Int =
  {
    @tailrec
    def loop(d: Int, acc: Int): Int =
    {
      if (d == 0) acc
      else loop(d-1,2 * acc)
    }
    loop(d, 1)
  }

  def main(args: Array[String]): Unit = {
    val startstring = "Hello, Scala!"
    println(startstring.reverse)
    println(startstring.toLowerCase())
    println(startstring.replace("!",""))
    println(startstring + " and goodbye python!")

    val tax = 13
    val inflation = 7
    var employees = scala.collection.mutable.ArrayBuffer[Double](100, 150, 200, 80, 120, 75)

    println("Годовой доход")
    val salary : Int = readLine().toInt
    println("Процент премии")
    val bonus : Float = readLine().toFloat
    println("Компенсация питания")
    val eat : Int = readLine().toInt
    val total = GetSalary(salary, bonus, eat, tax)
    println("оклад " + total)

    val AverSalary = employees.sum/employees.size
    //for (value <- employees) println((value-AverSalary)*100/AverSalary)

    employees = employees :+ AverSalary
    employees = employees ++ Array(350, 90)
    employees = employees.sorted
    println()
    println("сортированиый список зарплат")
    for (value <- employees) println(value)

    employees.insert(employees.lastIndexWhere(_<130)+1,130)
    println()
    println("список зарплат с вставленой зарплатой 130")
    for (value <- employees) println(value)

    println()
    println("список номеров сотрудников с зарплатой между 100 и 150")
    for (value <- employees) if ((value >= 100) && (value <= 150)) println(f"middle ${employees.indexOf(value)}")

    employees = employees.map(n => n * 1.07)

    println("Средняя зарплата junior?")
    val Averjunior: Int = readLine().toInt
    println("Средняя зарплата middle?")
    val Avermiddle : Int = readLine().toInt
    println("Средняя зарплата senior?")
    val Aversenior: Int = readLine().toInt

    employees = employees.map(n => if (n < 100) Averjunior else if((n >= 100) && (n <= 150)) Avermiddle else Aversenior)

    println()
    println("список индексированных зарплат")
    for (value <- employees) println(value)

    val workers = Map(
      "Василий Иванов" -> 120,
      "Сергей Петров" -> 170,
      "Андрей Плотников" -> 210,
      "Аркадий Васильев" -> 95
    )

    var minvalue = 99999
    var maxvalue = 0
    println()
    println("список сотрудников и их зарплат")
    for ((key, value) <- workers)
    {
      if (value < minvalue) minvalue = value
      if (value > minvalue) maxvalue = value
      println(key + " - " + value)
    }
    for ((key, value) <- workers)
    {
      if (value == minvalue) println(f"минимальная зарплата у $key")
      if (value == maxvalue) println(f"максимальная зарплата у $key")
    }
    var secret_workers = Map[String,Int]()
    for ((key, value) <- workers) {
      var newkey = key.split(" ")(1).toLowerCase().replaceAll("[аеёиоуыэюяАЕЁИОУЫЭЮЯ]","").reverse
      secret_workers +=(newkey -> value)
    }
    for ((key, value) <- secret_workers)  println(key + " " + value)

    println("Степень двойки")
    val degree: Int = readLine().toInt

    println("Рекурсия " + recdegrree(degree))
    println("Хвостовая рекурсия " + recdegrreetale(degree))
  }
}

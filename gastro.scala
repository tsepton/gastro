import scala.io.Source
import scala.util.Random

object Main {
  def title: String = {
    ("-" * 41) + "\n" + "| Welcome to the gastro menu composer ! |" + "\n" + ("-" * 41)
  }

  def main(args: Array[String]) {
    println(title)

    val products_file: String = "Products.csv"
    val products: List[Product] = GastroExtractor
      .extract_products(products_file)
      // A higher order method is invoked (sortWith)
      // An anonymous function is called inside sortWith
      .sortWith((item_1: Product, item_2: Product) =>
        item_1.calories < item_2.calories
      )

    // Uncomment me to check products
    // products.foreach(product => println(product))

    val person: Human = GastroUser.get_sex
    val meal: Meal = GastroUser.get_meal

    val menu_composer: MenuComposer = new MenuComposer(products, person, meal)
    menu_composer.compose
  }
}

// Here we will define traits
// ------------------------------------------------------------------------
trait Human {
  def sex: String
  def maximum_calories: Int // for a day
}

trait Meal {
  def kind: String
}

// Here we will define classes and case classes
// ------------------------------------------------------------------------
class MenuComposer(products: List[Product], person: Human, meal: Meal) {
  // We have a currying function here TODO
  private def matching_products(
      min_energy: Int,
      max_energy: Int
  ): List[Product] = {
    // Here we use the higher order method filter
    products.filter(min_energy < _.calories).filter(_.calories < max_energy)
  }

  private def random_product(ls: List[Product]): Product = {
    ls((new Random).nextInt.abs % ls.length)
  }

  private def total_calories(ls: List[Product]): Double = {
    // forEach higher order function used here & anonymous function too
    ls.map(_.calories).foldRight(0.0)(_ + _)
  }

  private def not_so_random_products: List[Product] = {
    // For comprehension used here
    val max: Double = GastroUser.calculate_max_calories_per_meal(meal, person)
    val products: List[Product] = (for { i <- 1 to 3 } yield random_product(
      matching_products(0, max.toInt)
    )).toList

    if (total_calories(products) > max) {
      not_so_random_products
    } else {
      products
    }
  }

  def compose: Unit = {
    println

    val temp_1: String =
      s"As a ${person.sex.toLowerCase}, for a ${meal.kind.toLowerCase},"
    val temp_2: String =
      s" you should limit yourself to ${GastroUser.calculate_max_calories_per_meal(meal, person).toInt} calories."
    println(s"$temp_1 $temp_2")

    println("Selected products are...")
    val corresponding_products: List[Product] = not_so_random_products
    corresponding_products.zipWithIndex.foreach {
      case (prod, index) => println(s"${index + 1} : $prod")
    }
    println(
      s"Total amount of calories for the dish : ${total_calories(corresponding_products).toInt}."
    )

    Utils.format_str(
      scala.io.StdIn.readLine("Would you like another menu ? [Yes/no] ")
    ) match {
      case "no" => ()
      case _    => compose
    }
  }
}

// As you can see we use the concept of inheritance for those case classes
case class Woman() extends Human {
  override def sex(): String = "Woman"
  override def maximum_calories(): Int = 2000
}

case class Man() extends Human {
  override def sex(): String = "Man"
  override def maximum_calories(): Int = 2500
}

// As you can see we use the concept of inheritance for those case classes
case class Breakfast() extends Meal {
  override def kind(): String = "Breakfast"
}

case class Lunch() extends Meal {
  override def kind(): String = "Lunch"
}

case class Dinner() extends Meal {
  override def kind(): String = "Dinner"
}

class Product(id: Int, name: String, energy: Int) {
  override def toString: String = s"$id | $energy | $name"

  def calories: Int = energy
}

// Singletons will be defined here
// ------------------------------------------------------------------------
object Utils {
  // Removes new lines & extra white space at the end and start of lines
  def format_str(str: String): String = {
    str.split('\n').mkString.trim
  }
}

// We will define a singleton with a few methods so we can get the sex and the type of meal recursively
object GastroUser {
  def get_sex: Human = {
    val input: String =
      scala.io.StdIn.readLine("Is the meal for a woman or a man ? ")
    Utils.format_str(input.toLowerCase) match {
      case "woman" => Woman()
      case "man"   => Man()
      case _       => get_sex
    }
  }

  def get_meal: Meal = {
    val input: String = scala.io.StdIn.readLine(
      "What kind of meal is desired ? [breakfast/lunch/dinner] "
    )
    Utils.format_str(input.toLowerCase) match {
      case "breakfast" => Breakfast()
      case "lunch"     => Lunch()
      case "dinner"    => Dinner()
      case _           => get_meal
    }
  }

  def calculate_max_calories_per_meal(meal: Meal, person: Human): Double = {
    // The case classes are used for pattern matching here
    meal match {
      // This is based on the "1/6, 1/4, 1/4" rule
      case Breakfast() => (1.0 / 6) * person.maximum_calories
      case _           => (1.0 / 4) * person.maximum_calories
    }
  }
}

object GastroExtractor {
  def extract_products(path: String): List[Product] = {
    (for {
      line <- Source.fromFile(path).getLines.drop(1)
      cols = line.split(";")
    } yield new Product(cols(0).toInt, cols(1), cols(4).toInt)).toList
  }
}

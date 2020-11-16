package gastro.menu

import scala.collection.mutable.ListBuffer

// Abstract class
abstract class Meal(customer: Human) {
  // Mutable scala collection ListBuffer
  var ingredients = new ListBuffer[Product]()
  val customer = customer

  def kind: String = "Meal"

  // Class Java pattern with setter & getter
  def set_ingredients(ingredient: Product): Unit = {
    ingredients += ingredient
  }

  def get_ingredients: List[Product] = {
    ingredients.toList
  }

  def get_maximum_calories: Double = {
    // The case classes are used for pattern matching here
    kind match {
      // This is based on the "1/6, 1/4, 1/4" rule
      case "Breakfast" => (1.0 / 6) * customer.calories_per_day
      case _           => (1.0 / 4) * customer.calories_per_day
    }
  }
}

class Product(id: Int, name: String, energy: Int) {
  override def toString: String = s"$id | $energy | $name"

  def calories: Int = energy
}

// Case classes with inheritance
case class Breakfast(customer: Human) extends Meal {
  override def kind(): String = "Breakfast"
}

case class Lunch(customer: Human) extends Meal {
  override def kind(): String = "Lunch"
}

case class Dinner(customer: Human) extends Meal {
  override def kind(): String = "Dinner"
}

// Trait
trait Human {
  def sex: String
  def calories_per_day: Int // for a day
}

case class Woman() extends Human {
  override def sex(): String = "Woman"
  override def calories_per_day(): Int = 2000
}

case class Man() extends Human {
  override def sex(): String = "Man"
  override def calories_per_day(): Int = 2500
}

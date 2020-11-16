package gastro.Menu

import scala.collection.mutable.ListBuffer

case class Order(id: Int, meal: Meal)

// Abstract class
case class Meal(customer: Human, command_number: Int) {
  // Mutable scala collection ListBuffer
  var ingredients: ListBuffer[Product] = new ListBuffer

  def kind: String = "Meal"

  // Class Java pattern with setter & getter
  def set_ingredients(products: List[Product]): Unit = {
    // For comprehension
    val new_ingredients: List[Product] =
      (for (ingredient <- products if !ingredients.contains(ingredient))
        yield ingredient)
    ingredients = ingredients :++ new_ingredients
  }

  // TODO
  def set_quantity(productID: Int, quantity: Float): Unit = {
    // Defining a method on ingredient would have been more approriate
    // But list comprehension is lit
    ingredients = for {
      ingredient <- ingredients
      val new_ingredient = new Product(
        productID,
        ingredient.name,
        ingredient.energy,
        ingredient.fat,
        quantity
      )
    } yield (new_ingredient)
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

  override def toString: String =
    s"MEAL => Customer : ${customer.sex} | Number of ingredients (${get_ingredients.length})"
}

case class Product(
    id: Int,
    name: String,
    energy: Int,
    fat: Float,
    quantity: Float = 0
) {

  override def toString: String = s"$id | $energy | $name"

  def calories: Int = energy
}

// Case classes with inheritance
class Breakfast(customer: Human, command_number: Int)
    extends Meal(customer, command_number) {
  override def kind(): String = "Breakfast"
}

class Lunch(customer: Human, command_number: Int)
    extends Meal(customer, command_number) {
  override def kind(): String = "Lunch"
}

class Dinner(customer: Human, command_number: Int)
    extends Meal(customer, command_number) {
  override def kind(): String = "Dinner"
}

// Trait
trait Human {
  def sex: String
  def calories_per_day: Int // for a day
  override def toString: String = s"Human is a $sex"
}

case class Woman() extends Human {
  override def sex(): String = "Woman"
  override def calories_per_day(): Int = 2000
}

case class Man() extends Human {
  override def sex(): String = "Man"
  override def calories_per_day(): Int = 2500
}

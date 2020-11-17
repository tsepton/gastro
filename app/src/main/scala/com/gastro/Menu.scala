package gastro.Menu

import scala.collection.mutable.ListBuffer

case class Order(id: Int, meal: Meal)

// Abstract class
case class Meal(customer: Human, commandNumber: Int) {
  // Mutable scala collection ListBuffer
  var ingredients: ListBuffer[Product] = new ListBuffer

  def kind: String = "Meal"

  // Class Java pattern with setter & getter
  def setIngredients(products: List[Product]): Unit = {
    // For comprehension
    val new_ingredients: List[Product] =
      (for (ingredient <- products if !ingredients.contains(ingredient))
        yield ingredient)
    ingredients = ingredients :++ new_ingredients
  }

  def setQuantity(productId: Int, quantity: String): Unit = {
    // Defining a method on ingredient would have been more approriate
    // But list comprehension is lit
    ingredients = for {
      ingredient <- ingredients
      new_ingredient = new Product(
        productId,
        ingredient.name,
        ingredient.energy,
        ingredient.fat,
        ingredient.sugar,
        ingredient.protein,
        quantity
      )
    } yield (new_ingredient)
  }

  def getIngredients: List[Product] = {
    ingredients.toList
  }

  def getMaximumCalories: Double = {
    // The case classes are used for pattern matching here
    kind match {
      // This is based on the "1/6, 1/4, 1/4" rule
      case "Breakfast" => (1.0 / 6) * customer.caloriesPerDay
      case _           => (1.0 / 4) * customer.caloriesPerDay
    }
  }

  override def toString: String =
    s"Meal => Customer : ${customer.sex} | Number of ingredients (${getIngredients.length})"
}

case class Product(
    id: Int,
    name: String,
    energy: Float,
    fat: Float,
    sugar: Float,
    protein: Float,
    quantity: String = ""
) {

  override def toString: String = s"$id | $energy | $name"
}

// Case classes with inheritance
class Breakfast(customer: Human, commandNumber: Int)
    extends Meal(customer, commandNumber) {
  override def kind(): String = "Breakfast"
}

class Lunch(customer: Human, commandNumber: Int)
    extends Meal(customer, commandNumber) {
  override def kind(): String = "Lunch"
}

class Dinner(customer: Human, commandNumber: Int)
    extends Meal(customer, commandNumber) {
  override def kind(): String = "Dinner"
}

// Trait
trait Human {
  def sex: String
  def caloriesPerDay: Int // for a day
  override def toString: String = s"Human is a $sex"
}

case class Woman() extends Human {
  override def sex(): String = "Woman"
  override def caloriesPerDay(): Int = 2000
}

case class Man() extends Human {
  override def sex(): String = "Man"
  override def caloriesPerDay(): Int = 2500
}

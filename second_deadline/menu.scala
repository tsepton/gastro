package gastro.menu

case class MenuRequest()

trait Meal {
  def kind: String
}

case class Breakfast() extends Meal {
  override def kind(): String = "Breakfast"
}

case class Lunch() extends Meal {
  override def kind(): String = "Lunch"
}

case class Dinner() extends Meal {
  override def kind(): String = "Dinner"
}

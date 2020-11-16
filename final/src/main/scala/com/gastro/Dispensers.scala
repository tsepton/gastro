package gastro.Dispensers

import gastro.Menu._
import gastro.Utils._
import akka.actor.{Actor}

abstract class Dispenser(products: List[Product]) extends Actor {
//   def receive: Receive = {}
}

class FatDispenser(products: List[Product]) extends Dispenser(products) {

  override def receive: Actor.Receive = {
    case Meal(_, _) => sender() ! (new ProductList(fat_products))
  }

  private def fat_products: List[Product] = {
    for (ingredient <- products if ingredient.fat > 3.6) yield ingredient
  }
}

class SugarDispenser(products: List[Product]) extends Dispenser(products) {

  override def receive: Actor.Receive = {
    case Meal(_, _) => sender() ! (new ProductList(fat_products))
  }

  private def fat_products: List[Product] = {
    for (ingredient <- products if ingredient.fat > 3.6) yield ingredient
  }
}

class ProteinDispenser(products: List[Product]) extends Dispenser(products) {

  override def receive: Actor.Receive = {
    case Meal(_, _) => sender() ! (new ProductList(fat_products))
  }

  private def fat_products: List[Product] = {
    for (ingredient <- products if ingredient.fat > 3.6) yield ingredient
  }
}

package gastro.Dispensers

import gastro.Menu._
import gastro.Utils._
import akka.actor.{Actor}

abstract class Dispenser(products: List[Product]) extends Actor {}

//TODO
class FatDispenser(products: List[Product]) extends Dispenser(products) {

  override def receive: Actor.Receive = {
    case Meal(_, _) => sender() ! (new ProductList(fatProducts))
  }

  private def fatProducts: List[Product] = {
    for (ingredient <- products if ingredient.fat > 3.6) yield ingredient
  }
}

//TODO
class SugarDispenser(products: List[Product]) extends Dispenser(products) {

  override def receive: Actor.Receive = {
    case Meal(_, _) => sender ! (new ProductList(fatProducts))
  }

  private def fatProducts: List[Product] = {
    for (ingredient <- products if ingredient.fat > 3.6) yield ingredient
  }
}

//TODO
class ProteinDispenser(products: List[Product]) extends Dispenser(products) {

  override def receive: Actor.Receive = {
    case Meal(_, _) => sender() ! (new ProductList(fatProducts))
  }

  private def fatProducts: List[Product] = {
    for (ingredient <- products if ingredient.fat > 3.6) yield ingredient
  }
}

package gastro.Dispensers

import gastro.Menu._
import gastro.Utils._
import akka.actor.{Actor}

abstract class Dispenser(products: List[Product]) extends Actor {}

class FatDispenser(products: List[Product]) extends Dispenser(products) {

  override def receive: Actor.Receive = {
    case Meal(_, _) => sender() ! (new ProductList(fatProducts))
  }

  // 9.15 is the mean for all products
  private def fatProducts: List[Product] = {
    // This is where I translated a for comprehension into its equivalent form
    products filter (_.fat >= 9.15) map (ingredient => ingredient)
    // for (ingredient <- products if ingredient.fat >= 9.15) yield ingredient
  }
}

class SugarDispenser(products: List[Product]) extends Dispenser(products) {

  override def receive: Actor.Receive = {
    case Meal(_, _) => sender ! (new ProductList(sugaryProducts))
  }

  // 227g of sugar is the mean for all products
  private def sugaryProducts: List[Product] = {
    for (ingredient <- products if ingredient.sugar >= 227) yield ingredient
  }
}

class ProteinDispenser(products: List[Product]) extends Dispenser(products) {

  override def receive: Actor.Receive = {
    case Meal(_, _) => sender() ! (new ProductList(proteinProducts))
  }

  //1377 is the mean for all products
  private def proteinProducts: List[Product] = {
    for (ingredient <- products if ingredient.protein >= 1377) yield ingredient
  }
}

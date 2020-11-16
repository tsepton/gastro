package gastro.scheduler

import gastro.menu._
import gastro.utils._
import gastro.dispensers._
import akka.actor.{Actor, ActorRef}

/*
 * This is the main program which will coordinate all the actors
 * so they can work together.
 */

object Main {
  def main(args: Array[String]) {
    val products_file: String = "Products.csv"
    val products: List[Product] = GastroExtractor
      .extract_products(products_file)
      // A higher order method is invoked (sortWith)
      // An anonymous function is called inside sortWith
      .sortWith((item_1: Product, item_2: Product) =>
        item_1.calories < item_2.calories
      )

    // Init actors
    val system = ActorSystem("system")
    val waiter = system.actorOf(Props(classOf[Waiter], coq), "waiter")
    // val intendant =
    //   system.actorOf(Props(classOf[Intendant], products), "intendant")
    // val fatDispenser =
    //   system.actorOf(Props(classOf[CaloryDispenser], products), "fatDispenser")
    // val sugarDispenser =
    //   system.actorOf(Props(classOf[LipidDispenser], products), "sugarDispenser")
    // val proteinDispenser = system.actorOf(
    //   Props(classOf[FiberDispenser], products),
    //   "proteinDispenser"
    // )
    // val coq = system.actorOf(
    //   Props(
    //     classOf[Coq],
    //     intendant,
    //     List(fatDispenser, sugarDispenser, proteinDispenser)
    //   ),
    //   "coq"
    // )

    // Trigger restaurant opening
    waiter ! "Meal request"

  }
}

class Waiter(coq: ActorRef) extends Actor {
  override def receive: Receive = {
    case "init" => get_order
  }

  private def get_sex: Human = {
    val input: String =
      scala.io.StdIn.readLine("Is the meal for a woman or a man ? ")
    Utils.format_str(input.toLowerCase) match {
      case "woman" => Woman()
      case "man"   => Man()
      case _       => get_sex
    }
  }

  private def get_order: Meal = {
    val input: String = scala.io.StdIn.readLine(
      "What kind of meal is desired ? [breakfast/lunch/dinner] "
    )
    Utils.format_str(input.toLowerCase) match {
      case "breakfast" => Breakfast(get_sex)
      case "lunch"     => Lunch(get_sex)
      case "dinner"    => Dinner(get_sex)
      case _           => get_order
    }
  }
}

// class Coq(waiter: ActorRef, intendant: ActorRef, dispensers: List[ActorRef])
//     extends Actor {
//   // This will act as the list of current orders the chef has been asked for
//   var orders: Map[Int, Meal] = Map()

//   override def receive: Receive = {}

//   private def checkIngredients: Boolean = {}
// }

// class Intendant(products: List[Products]) extends Actor {
//   def receive: Receive = {}
//   private def base_product: Product = {}
// }

package gastro.Main

import akka.actor.{ActorSystem, Props}

import gastro.Menu._
import gastro.Utils._
import gastro.Dispensers._
import gastro.Scheduler._

object Main {
  def main(args: Array[String]) {
    val products_file: String = "src/Products.csv"
    val products: List[Product] = Utils
      .extract_products(products_file)
      // A higher order method is invoked (sortWith)
      // An anonymous function is called inside sortWith
      .sortWith((item_1: Product, item_2: Product) =>
        item_1.calories < item_2.calories
      )

    // Init actors
    val system = ActorSystem("system")
    val intendant =
      system.actorOf(Props(classOf[Intendant], products), "intendant")
    val fatDispenser =
      system.actorOf(Props(classOf[FatDispenser], products), "fatDispenser")
    val sugarDispenser =
      system.actorOf(Props(classOf[SugarDispenser], products), "sugarDispenser")
    val proteinDispenser = system.actorOf(
      Props(classOf[ProteinDispenser], products),
      "proteinDispenser"
    )
    val coq = system.actorOf(
      Props(
        classOf[Coq],
        intendant,
        List(fatDispenser, sugarDispenser, proteinDispenser)
      ),
      "coq"
    )
    val waiter = system.actorOf(Props(classOf[Waiter], coq), "waiter")

    handle_input match {
      case "1" => waiter ! "Meal request"
      case "2" =>
        waiter ! "Under pressure"
      case _ => handle_input
    }
  }

  def handle_input: String = {
    scala.io.StdIn.readLine(
      "What do you want to do ?\n"
        + "\t[1] Test personnal input\n"
        + "\t[2] Simulate multiple commands at a time\n"
        + "Answer : "
    )
  }
}

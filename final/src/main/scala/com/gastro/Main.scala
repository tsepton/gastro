package gastro.Main

import akka.actor.{ActorSystem, Props}

import gastro.Menu._
import gastro.Utils._
import gastro.Dispensers._
import gastro.Scheduler._
import akka.actor.ActorRef

object Main {
  def main(args: Array[String]) {
    val productsFile: String = "src/Products.csv"
    val products: List[Product] = Utils
      .extract_products(productsFile)
      // A higher order method is invoked (sortWith)
      // An anonymous function is called inside sortWith
      .sortWith((item_1: Product, item_2: Product) =>
        item_1.calories < item_2.calories
      )

    // Init actors
    val system: ActorSystem = ActorSystem("system")
    val intendant: ActorRef =
      system.actorOf(Props(classOf[Intendant], products), "intendant")
    val fatDispenser: ActorRef =
      system.actorOf(Props(classOf[FatDispenser], products), "fatDispenser")
    val sugarDispenser: ActorRef =
      system.actorOf(Props(classOf[SugarDispenser], products), "sugarDispenser")
    val proteinDispenser: ActorRef = system.actorOf(
      Props(classOf[ProteinDispenser], products),
      "proteinDispenser"
    )
    val coq: ActorRef = system.actorOf(
      Props(
        classOf[Coq],
        intendant,
        List(fatDispenser, sugarDispenser, proteinDispenser)
      ),
      "coq"
    )
    val waiter: ActorRef = system.actorOf(Props(classOf[Waiter], coq), "waiter")

    def handleInput: Unit = {
      scala.io.StdIn.readLine(
        "What do you want to do ?\n"
          + "\t[1] Test personnal input\n"
          + "\t[2] Simulate multiple commands at a time\n"
          + "Answer : "
      ) match {
        case "1" => waiter ! "Meal request"
        case "2" =>
          waiter ! "Under pressure"
        case _ => handleInput
      }
    }
    handleInput
  }
}

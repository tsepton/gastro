package gastro.Main

import akka.actor.{ActorSystem, Props}
import akka.actor.ActorRef

import scala.util.{Success, Failure}

import gastro.Menu._
import gastro.Utils._
import gastro.Dispensers._
import gastro.Coq.Coq
import gastro.Waiter.Waiter
import gastro.Intendant.Intendant

object Main {
  def main(args: Array[String]): Unit = {
    val products: List[Product] = Utils
      .extractProducts("src/Products.csv")
      // A higher order method is invoked (sortWith)
      // An anonymous function is called inside sortWith
      .sortWith((item1: Product, item2: Product) => item1.energy < item2.energy)

    val portions: Map[Int, String] =
      Utils extractPortions ("src/Portions.csv") match {
        case Success(map) => map
        case Failure(e)   => Map()
      }

    // Init actors
    val system: ActorSystem = ActorSystem("system")
    val intendant: ActorRef =
      system.actorOf(Props(classOf[Intendant], products, portions), "intendant")
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

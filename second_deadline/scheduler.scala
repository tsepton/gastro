package gastro.scheduler

import gastro._
import akka.actor.{Actor, ActorRef}

/*
 * This is the main program which will coordinate all the actors
 * so they can work together.
 */

object Main {
  def main(args: Array[String]) {}
}

class Waiter(coq: ActorRef) extends Actor {
  override def receive: Receive = {}
}

class Coq(waiter: ActorRef, intendant: ActorRef, dispensers: List[ActorRef])
    extends Actor {
  // This will act as the list of current orders the chef has been asked for
  var orders: Map[Int, Meal] = Map()

  override def receive: Receive = {}

  private def checkIngredients: Boolean = {}
}

class Intendant(products: List[Products]) extends Actor {
  def receive: Receive = {}
  private def base_product: Product = {}
}

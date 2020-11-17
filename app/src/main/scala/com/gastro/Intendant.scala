package gastro.Intendant

import gastro.Menu._
import gastro.Utils._
import gastro.Dispensers._

import akka.pattern.ask
import akka.util.Timeout
import akka.actor.{Actor, ActorSystem, ActorRef, Props}
import akka.event.Logging

import scala.language.postfixOps
import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer
import scala.util.{Success, Failure, Random, Try}
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import akka.event.LoggingAdapter

import akka.actor._


class Intendant(products: List[Product], portions: Map[Int, String])
    extends Actor {
  val log = Logging(context.system, this)

  def receive: Receive = {
    // Here is an example of the ProductList use
    case Meal(_, _)            => sender ! baseProduct
    case ProductList(products) => sender ! QuantityMap(quantities(products))
    case _                     => println("Intendant: Sorry, our chef may be drunk")
  }

  private def baseProduct: Product = {
    Random.shuffle(products).head
  }

  private def quantities(ingredients: List[Product]): Map[Int, String] = {
    // For comprehension
    (for {
      product <- ingredients
      quantity = getProductQuantity(product).getOrElse("")
    } yield (product.id -> quantity)) toMap
  }

  private def getProductQuantity(ingredient: Product): Option[String] = {
    // Using Option
    portions get ingredient.id
  }
}

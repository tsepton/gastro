package gastro.Waiter

import gastro.Menu._
import gastro.Utils._
import gastro.Dispensers._

import akka.pattern.ask
import akka.util.Timeout
import akka.actor.{Actor, ActorRef, Props}
import akka.event.{Logging, LoggingAdapter}

import scala.language.postfixOps
import scala.concurrent.duration._
import scala.util.{Success, Failure, Random, Try}
import scala.concurrent.{Await, ExecutionContext, Future, Promise}


class Waiter(coq: ActorRef) extends Actor {
  implicit val timeout = Timeout(5 seconds)
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global

  val log: LoggingAdapter = Logging(context.system, this)
  var commandsId: Int = 0

  override def receive: Receive = {
    case "Meal request"   => coq ! getOrder //tellTheKitchen(getOrder)
    case "Under pressure" => emergencyTraining
    case meal: Meal       => bringTheDish(meal)
    case s: String        => println(s)
    case _                => println("I'm sorry but your meal has been lost on the way")
  }

  // Unused right now: waiter is being killed after response... WHY ?!
  private def tellTheKitchen(order: Meal): Unit = {
    // Fixme
    coq ? order onComplete {
      case Success(answer: String) => println(answer)
      case _ =>
        println("Waiter: I'm sorry something wrong happened with your order")
    }
  }

  private def getSex: Human = {
    val input: String =
      scala.io.StdIn.readLine(
        "Waiter : Sorry but I'm blind, is the meal for a woman or a man ? "
      )
    Utils formatString (input.toLowerCase) match {
      case "woman" => Woman()
      case "man"   => Man()
      case _       => getSex
    }
  }

  private def getOrder: Meal = {
    val input: String = scala.io.StdIn.readLine(
      "Waiter : What kind of meal do you desire ? [breakfast/lunch/dinner] "
    )
    Utils formatString (input.toLowerCase) match {
      case "breakfast" => new Breakfast(getSex, getId)
      case "lunch"     => new Lunch(getSex, getId)
      case "dinner"    => new Dinner(getSex, getId)
      case _           => getOrder
    }
  }

  private def bringTheDish(meal: Meal): Unit = {
    println(s"Here is the ${meal.kind} from command n°${meal.commandNumber} !")
    println("Here is the recipe...")
    for (ingredient <- meal.getIngredients)
      println(
        s"\t${ingredient.name} " +
          (if (ingredient.quantity != "")
             s"with a portion weight specified as... ${ingredient.quantity}"
           else "but no portion was specified...")
      )
    println("I hope you'll like it and bon appétit !")
  }

  private def emergencyTraining: Unit = {
    Try(
      scala.io.StdIn.readLine("How many commands should we operate ? ") toInt
    ) match {
      case Success(number: Int) =>
        for (meal <- loadsOfMeal(number)) coq ! meal //tellTheKitchen(meal)
      case Failure(_) => emergencyTraining
    }
  }

  private def loadsOfMeal(number: Int): List[Meal] = {
    val sex: List[String] = List("man", "woman")
    val mealKind: List[String] = List("breakfast", "lunch", "dinner")
    (for (
      i <- 1 to number;
      person: Human = Random.shuffle(sex).head match {
        case "man"   => new Man
        case "woman" => new Woman
      };
      meal: Meal = Random.shuffle(mealKind).head match {
        case "breakfast" => new Breakfast(person, getId)
        case "lunch"     => new Lunch(person, getId)
        case "dinner"    => new Dinner(person, getId)
      }
    ) yield meal) toList
  }

  private def getId: Int = {
    commandsId += 1
    commandsId
  }
}
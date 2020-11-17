package gastro.Coq

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

class Coq(intendant: ActorRef, dispensers: List[ActorRef]) extends Actor {
  implicit val timeout = Timeout(5 seconds)
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global

  val log = Logging(context.system, this)

  // This is needed so we can keep track of our waiter
  var waiter: ActorRef = null

  override def receive: Receive = {
    case meal: Meal => {
      waiter = sender
      waiter ! s"The command n°${meal.commandNumber} is being prepared"
      cook(meal)
    }
    case _ => println("Chef : Waiter, this isn't on the menu !")
  }

  // This is the function that does all the magic,
  // It will retrieve ingredients, get quantities
  // and tell the waiter when it's done
  private def cook(meal: Meal): Unit = {
    Future {
      // Get other ingredients
      setMainIngredient(meal).future onComplete {
        case Success(_) => {
          Future {
            setOtherIngredients(meal).future onComplete {
              case Success(_) => finishAndSend(meal)
              case Failure(_) =>
                println("Chef: What do you want me to do with this !?")
            }
          }
        }
        case Failure(_) =>
          println("Chef: We're sorry but we did not buy any products...")
      }
    }
  }

  private def setMainIngredient(meal: Meal): Promise[Any] = {
    val promise: Promise[Any] = Promise()
    // Using Future & Promise
    // Using onComplete so we do not block our actor
    intendant ? meal onComplete {
      // Using Succes & Failure
      case Success(product: Product) => {
        meal setIngredients (List(product))
        promise success "Main ingredient set"
      }
      case _ => {
        println("Chef: What do you want me to do with this !?")
        promise failure (new IllegalStateException)
      }
    }
    promise
  }

  private def setOtherIngredients(meal: Meal): Promise[Any] = {
    val promise: Promise[Any] = Promise()
    var allIngredients: ListBuffer[Product] = new ListBuffer[Product]
    var answersNumber: Int = 0

    for (dispenser <- dispensers) {
      dispenser ? meal onComplete {
        // Using Succes & Failure
        case Success(ProductList(ingredients)) => {
          allIngredients = allIngredients :++ ingredients
          if (answersNumber == 2) {
            answersNumber += 1
            pickIngredients(meal, allIngredients.toList) match {
              case Success(value) => {
                meal setIngredients value
                promise success "Other ingredients set"
              }
              case _ => promise failure (new IllegalStateException)
            }
          } else {
            answersNumber += 1
          }
        }
        case _ => {
          println("Chef: What do you want me to do with this !?")
          // promise failure (new IllegalStateException)
        }
      }
    }
    promise
  }

  private def pickIngredients(
      meal: Meal,
      ingredients: List[Product]
  ): Try[List[Product]] = {
    // slice higher order function used here & anonymous function too
    val max: Double = meal.getMaximumCalories
    val products: List[Product] = Random
      .shuffle(matchingProducts(ingredients, 0, max.toInt))
      .slice(0, 3)

    if (totalCalories(products) > max)
      pickIngredients(meal, ingredients)
    Try(products)
  }

  private def matchingProducts(
      ingredients: List[Product],
      minEnergy: Int,
      maxEnergy: Int
  ): List[Product] = {
    // Here we use the higher order method filter
    ingredients.filter(minEnergy < _.energy).filter(_.energy < maxEnergy)
  }

  private def totalCalories(ls: List[Product]): Double = {
    // forEach higher order function used here & anonymous function too
    ls.map(_.energy).foldRight(0.0)(_ + _)
  }

  private def finishAndSend(meal: Meal): Unit = {
    intendant ? (new ProductList(meal.getIngredients)) onComplete {
      case Success(QuantityMap(quantities)) => {
        for (ingredient <- meal.getIngredients)
          meal.setQuantity(ingredient.id, quantities(ingredient.id))
        waiter ! meal
      }
      case _ => println("Chef: Please excuse us, we've lost our stock...")
    }
  }
}

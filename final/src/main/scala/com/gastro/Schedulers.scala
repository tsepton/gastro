package gastro.Scheduler

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

/*
 * This is the main program which will coordinate all the actors
 * so they can work together.
 */

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
    // TODO : Choose few products based on something...
    // Using Try here, in case list is empty
    Try {
      List(
        Random.shuffle(ingredients).head,
        Random.shuffle(ingredients).head,
        Random.shuffle(ingredients).head
      )
    }
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
      if (quantity != None)
    } yield (product.id -> quantity)) toMap
  }

  private def getProductQuantity(ingredient: Product): Option[String] = {
    // Using Option
    portions get ingredient.id
  }
}

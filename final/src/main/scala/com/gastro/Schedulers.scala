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
  implicit val timeout = Timeout(5 seconds) // needed for ?
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global

  val log: LoggingAdapter = Logging(context.system, this)
  var commands_id: Int = 0

  override def receive: Receive = {
    case "Meal request"   => coq ! get_order //tell_the_kitchen(get_order)
    case "Under pressure" => emergency_training
    case meal: Meal       => bring_the_dish(meal)
    case s: String        => println(s)
    case _                => println("I'm sorry but your meal has been lost on the way")
  }

  // Unused right now: waiter is being killed after response... WHY ?!
  private def tell_the_kitchen(order: Meal): Unit = {
    // Fixme
    coq ? order onComplete {
      case Success(answer: String) => println(answer)
      case _ =>
        println("Waiter: I'm sorry something wrong happened with your order")
    }
  }

  private def get_sex: Human = {
    val input: String =
      scala.io.StdIn.readLine(
        "Waiter : Sorry but I'm blind, is the meal for a woman or a man ? "
      )
    Utils.format_str(input.toLowerCase) match {
      case "woman" => Woman()
      case "man"   => Man()
      case _       => get_sex
    }
  }

  private def get_order: Meal = {
    val input: String = scala.io.StdIn.readLine(
      "Waiter : What kind of meal do you desire ? [breakfast/lunch/dinner] "
    )
    Utils.format_str(input.toLowerCase) match {
      case "breakfast" => new Breakfast(get_sex, get_id)
      case "lunch"     => new Lunch(get_sex, get_id)
      case "dinner"    => new Dinner(get_sex, get_id)
      case _           => get_order
    }
  }

  private def bring_the_dish(meal: Meal): Unit = {
    println(s"Here is your ${meal.kind} !")
    println("Here is the recipe...")
    for (ingredient <- meal.get_ingredients)
      println(s"\t${ingredient.name} ${ingredient.quantity}")
    println("I hope you'll like it and bon appétit !")
  }

  private def emergency_training(): Unit = {
    // TODO : Handle wrong input
    for (
      meal <- loads_of_meal(
        scala.io.StdIn.readLine("How many commands should we operate ? ") toInt
      )
    ) {
      //tell_the_kitchen(meal)
      coq ! meal
    }
  }

  private def loads_of_meal(number: Int): List[Meal] = {
    val sex: List[String] = List("man", "woman")
    val meal_kind: List[String] = List("breakfast", "lunch", "dinner")
    (for (
      i <- 1 to number;
      person: Human = Random.shuffle(sex).head match {
        case "man"   => new Man
        case "woman" => new Woman
      };
      meal: Meal = Random.shuffle(meal_kind).head match {
        case "breakfast" => new Breakfast(person, get_id)
        case "lunch"     => new Lunch(person, get_id)
        case "dinner"    => new Dinner(person, get_id)
      }
    ) yield meal) toList
  }

  private def get_id: Int = {
    commands_id += 1
    commands_id
  }
}

class Coq(intendant: ActorRef, dispensers: List[ActorRef]) extends Actor {
  implicit val timeout = Timeout(5 seconds) // needed for ?
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global

  val log = Logging(context.system, this)

  // This is needed so we can keep track of our waiter
  var waiter: ActorRef = null

  override def receive: Receive = {
    case meal: Meal => {
      waiter = sender
      waiter ! s"The command n°${meal.command_number} is being prepared"
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
      set_main_ingredient(meal).future onComplete {
        case Success(_) => {
          Future {
            set_other_ingredients(meal).future onComplete {
              case Success(_) => finish_and_send(meal)
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

  private def set_main_ingredient(meal: Meal): Promise[Any] = {
    val promise: Promise[Any] = Promise()
    // Using Future & Promise
    // Using onComplete so we do not block our actor
    intendant ? meal onComplete {
      // Using Succes & Failure
      case Success(product: Product) => {
        meal set_ingredients (List(product))
        promise success "Main ingredient set"
      }
      case _ => {
        println("Chef: What do you want me to do with this !?")
        promise failure (new IllegalStateException)
      }
    }
    promise
  }

  private def set_other_ingredients(meal: Meal): Promise[Any] = {
    // FIXME : Make it more functionnal - how ?
    val promise: Promise[Any] = Promise()
    var all_ingredients: ListBuffer[Product] = new ListBuffer[Product]
    var answers_number: Int = 0

    for (dispenser <- dispensers) {
      dispenser ? meal onComplete {
        // Using Succes & Failure
        case Success(ProductList(ingredients)) => {
          all_ingredients = all_ingredients :++ ingredients
          if (answers_number == 2) {
            answers_number += 1
            pick_ingredients(meal, all_ingredients.toList) match {
              case Success(value) => {
                meal set_ingredients value
                promise success "Other ingredients set"
              }
              case _ => promise failure (new IllegalStateException)
            }
          } else {
            answers_number += 1
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

  private def pick_ingredients(
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

  private def finish_and_send(meal: Meal): Unit = {
    intendant ? (new ProductList(meal.get_ingredients)) onComplete {
      case Success(QuantityMap(quantities)) => {
        for (ingredient <- meal.get_ingredients)
          meal.set_quantity(ingredient.id, quantities(ingredient.id))
        waiter ! meal
      }
      case _ => println("Chef: Please excuse us, we've lost our stock...")
    }
  }
}

class Intendant(products: List[Product]) extends Actor {
  val log = Logging(context.system, this)

  def receive: Receive = {
    // Here is an example of the ProductList use
    case Meal(_, _)            => sender ! base_product
    case ProductList(products) => sender ! QuantityMap(quantities(products))
    case _                     => println("Intendant: Sorry, our chef may be drunk")
  }

  private def base_product: Product = {
    Random.shuffle(products).head
  }

  private def quantities(ingredients: List[Product]): Map[Int, Float] = {
    (for {
      product <- ingredients
      quantity = get_product_quantity(product)
    } yield (product.id -> quantity)) toMap
  }

  private def get_product_quantity(ingredient: Product): Float = {
    // TODO
    0.6 toFloat
  }
}

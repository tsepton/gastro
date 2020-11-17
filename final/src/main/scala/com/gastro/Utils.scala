package gastro.Utils

import scala.io.Source
import gastro.Menu._
import scala.util.{Try, Failure, Success}

object Utils {
  def extract_products(path: String): List[Product] = {
    (for {
      line <- Source.fromFile(path).getLines.drop(1)
      cols = line.split(";")
    } yield new Product(
      cols(0).toInt,
      cols(1),
      cols(4).toInt,
      cols(10).toFloat
    )).toList
  }

  def formatString(str: String): String = {
    str.split('\n').mkString.trim
  }

  // // Fonctions de haut niveau (Try, for, trim)
  // def extract_portions(path: String): Try[Map[Int, ServingSize]] = {

  //   Try (
  //     for (
  //     line <- Source.fromFile(path).getLines()
  //     cols = line.split(";").map(_.trim)
  //   ) yield Map[productID, portion]
  //   )

  //   var ret: Map[Int, ServingSize] = Map()
  //   Try((for {
  //     line <- Source.fromFile(path).getLines()
  //     cols = line.split(";").map(_.trim)
  //     productId = (cols(0).toInt)
  //     quantity = Try(cols(1).toFloat)
  //     uom = Try(cols(2))
  //   } quantity match {
  //     case Success(value) =>
  //       uom match {
  //         case Success(v2) =>
  //           ret = ret + (productId -> new ServingSize(productId, value, v2))
  //         case Failure(_) => null
  //       }
  //     case Failure(_) => null
  //   }))
  //   Try(ret)

  // }
}

// These are enclosing classes so we can overcome the type erasure
// eg : case List[Product] 
//     -> because of type erasure it would match with List[Any] 
case class ProductList(products: List[Product])
case class QuantityMap(quantities: Map[Int, Float])

package gastro.Utils

import scala.io.Source
import gastro.Menu._
import scala.util.{Try, Failure, Success}

object Utils {
  private def checkForValue(option: Try[Any]) = {
    option match {
      case Success(value) => value
      case Failure(e)     => 0.0
    }
  }

  def extractProducts(path: String): List[Product] = {
    (for {
      line <- Source.fromFile(path).getLines.drop(1)
      cols = line.split(";")
      energy = Try(cols(4).toInt)
      fat = Try(cols(10).toFloat)
      sugar = Try(cols(7).toFloat)
      protein = Try(cols(5).toFloat)
    } yield new Product(
      cols(0).toInt,
      cols(1),
      checkForValue(protein).asInstanceOf[Float],
      checkForValue(fat).asInstanceOf[Float],
      checkForValue(sugar).asInstanceOf[Float],
      checkForValue(protein).asInstanceOf[Float]
    )).toList
  }

  def formatString(str: String): String = {
    str.split('\n').mkString.trim
  }

  // Methods  Try, for, trim
  def extractPortions(path: String): Try[Map[Int, String]] = {
    Try((for {
      line <- Source.fromFile(path).getLines().drop(1)
      cols = line.split(";").map(_.trim)
      if (cols(5) != "" && cols(5) != "Quantity not specified"
        && cols(5) != "Not included in a food category")
      productId = cols(0).toInt
      portionWeight = cols(5)
    } yield (productId -> portionWeight)).toMap)
  }
}

// These are enclosing classes so we can overcome the type erasure
// eg : case List[Product]
//     -> because of type erasure it would match with List[Any]
case class ProductList(products: List[Product])
case class QuantityMap(quantities: Map[Int, String])

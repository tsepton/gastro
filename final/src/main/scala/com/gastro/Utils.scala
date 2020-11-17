package gastro.Utils

import scala.io.Source
import gastro.Menu._
import scala.util.{Try, Failure, Success}

object Utils {
  def extractProducts(path: String): List[Product] = {
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

  // Fonctions  Try, for, trim
  def extractPortions(path: String): Try[Map[Int, String]] = {
    // If there is multiple ids the same,
    // the map will remove them and keep the last entry
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

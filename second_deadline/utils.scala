package gastro.utils

object GastroExtractor {
  def extract_products(path: String): List[Product] = {
    (for {
      line <- Source.fromFile(path).getLines.drop(1)
      cols = line.split(";")
    } yield new Product(cols(0).toInt, cols(1), cols(4).toInt)).toList
  }

  // Fonctions de haut niveau (Try, for, trim)
  def extract_portions(path: String): Try[Map[Int, ServingSize]] = {
    var ret: Map[Int, ServingSize] = Map()
    Try((for {
      line <- Source.fromFile(path).getLines()
      cols = line.split(";").map(_.trim)
      productId = (cols(0).toInt)
      quantity = Try(cols(1).toFloat)
      uom = Try(cols(2))
    } quantity match {
      case Success(value) =>
        uom match {
          case Success(v2) =>
            ret = ret + (productId -> new ServingSize(productId, value, v2))
          case Failure(_) => null
        }
      case Failure(_) => null
    }))
    Try(ret)

  }
}

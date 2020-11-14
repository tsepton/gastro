package gastro.dispensers

import gastro.menu._

abstract class Dispenser(products: List[Products]) extends Actor {
  def receive: Receive = {}
}

class FatDispenser(products: List[Product]) extends Dispenser {}
class SugarDispenser(products: List[Product]) extends Dispenser {}
class ProteinDispenser(products: List[Product]) extends Dispenser {}

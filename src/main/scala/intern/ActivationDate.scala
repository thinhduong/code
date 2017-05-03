package intern

import scala.collection.mutable

object ActivationDate {
  case class Transaction(phone: String, activatedDate: String, deActivatedDate: String)

  def solve(xs: List[Transaction]): List[Transaction] = {
    val ret = xs.foldLeft(mutable.Map[String, Transaction]())((map, trans) => {
      if (map.contains(trans.phone)){
        val item = map(trans.phone)

        if (item.deActivatedDate == trans.activatedDate) {
          map(trans.phone) = item.copy(deActivatedDate = trans.deActivatedDate)
        }
        else {
          map(trans.phone) = trans
        }

        map
      }
      else {
        map += trans.phone -> trans
        map
      }
    })

    ret.values.toList
  }

  def main(args: Array[String]): Unit = {

  }
}

package intern

object PhoneNumber {
  private val prefixes: List[String] = List("0120", "0121", "0122", "0123", "0124", "0125", "0126", "0127", "0128", "0129", "0162", "0163", "0164", "0165", "0166", "0167", "0168", "0169", "0186", "0188", "0199", "086", "088", "089", "090", "091", "092", "093", "094", "095", "096", "097", "098", "099")

  def solve(inp: String): List[String] = {
    val phones = inp.toList.foldLeft(List[StringBuilder](new StringBuilder))((xs, c) => {
      if (c.isDigit) {
        xs.head.append(c)
        xs
      }
      else if (c.isWhitespace)
        xs
      else {
        if (xs.head.length > 0) {
          new StringBuilder :: xs
        } else
          xs
      }
    })

    val phoneWithPrefix = phones.filter(p => p.length >= 10 && p.length <= 11).
      map(p => (p, p.take(p.length - 7).toString))

    phoneWithPrefix.filter { case (_, prefix) => prefixes.contains(prefix) }.
      map {case (p, _) => p.toString }
  }

  def main(args: Array[String]): Unit = {

  }
}

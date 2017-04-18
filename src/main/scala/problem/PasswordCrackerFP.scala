package problem

object PasswordCrackerFP {
  def solve(pwds: Seq[String], attempt: String): String = {
    def go(from: Int, len: Int):Option[List[String]] = {
      if (from + len > attempt.length)
        Some(List())

      else {
        val cur = attempt.substring(from, from + len)

        if (!pwds.exists(x => x.length >= cur.length && x.startsWith(cur)))
          None
        else {
          if (pwds.exists(x => x == cur)) {
            val theRest = go(from + len, 1) match {
              case None => None
              case Some(xs) => Some(cur :: xs)
            }

            val total = go(from, len + 1)

            (theRest, total) match {
              case (None, None) => None
              case (None, xs) => xs
              case (xs, _) => xs
            }
          }
          else {
            go(from, len + 1)
          }
        }
      }
    }

    go(0, 1) match {
      case None => "WRONG PASSWORD"
      case Some(xs) => xs.mkString(" ")
    }
  }

  def main(args: Array[String]): Unit = {
    val t = readInt()

    (0 until t).foreach(_ => {
      val n = readInt()

      val pwds = readLine().split(' ')
      val attempt = readLine()

      println(solve(pwds, attempt))
    })
  }
}

package scalatb

import Console._

object Reporting {

  def report(message: String): Unit = {
    println(s"[${MAGENTA}scalatb${RESET}] $message")
  }


}

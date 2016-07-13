package kz.greetgo

import scala.scalajs.js.JSApp

object TutorialApp  {
  def main(args: Array[String]) {
    val x = for (x <- 1 to 5) x
    println("Hello world!"+x)
  }
}
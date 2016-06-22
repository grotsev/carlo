package kz.greetgo.dtlang

import name.lakhin.eliah.projects.papacarlo.lexis.TokenReference

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

/**
  * Created by den on 20.06.16.
  */
@JSExport
object DtLangApi {
  private val lexer = DtLang.lexer
  private val syntax = DtLang.syntax(lexer)

  @JSExport
  def inputAll(text: String) = {
    lexer.input(text)
  }

  @JSExport
  def input(text: String,
            fromLine: Int,
            fromChar: Int,
            toLine: Int,
            toChar: Int) {
    lexer.input(text, fromLine -> fromChar, toLine -> toChar)
  }

  @JSExport
  def getErrors() = {
    toJsArray(syntax.getErrors.map {
      error =>
        val from = tokenCursor(error.from)
        val to = tokenCursor(error.to, after = true)

        js.Dynamic.literal(
          "from" -> from,
          "to" -> to,
          "description" -> error.description
        )
    })
  }

  @JSExport
  def getNodeFragment(id: Int) = {
    syntax.getNode(id) match {
      case Some(node) =>
        js.Dynamic.literal(
          "exists" -> true,
          "id" -> id,
          "from" -> tokenCursor(node.getBegin),
          "to" -> tokenCursor(node.getEnd, after = true)
        )

      case None => js.Dynamic.literal(
        "exists" -> false,
        "id" -> id
      )
    }
  }

  // TODO export changes (added, removed)

  private def toJsArray(iterable: Iterable[js.Any]) = {
    val result = new js.Array[js.Any]
    for (element <- iterable) result.push(element)
    result
  }

  private def tokenCursor(token: TokenReference, after: Boolean = false) = {
    val pair = token.collection.cursor(token.index + (if (after) 1 else 0))
    js.Dynamic.literal("line" -> (pair._1 - 1), "ch" -> (pair._2 - 1))
  }

}

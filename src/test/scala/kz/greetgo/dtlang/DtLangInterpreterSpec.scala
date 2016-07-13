package kz.greetgo.dtlang

import name.lakhin.eliah.projects.papacarlo.syntax.Node
import org.scalatest.FunSpec

/**
  * Created by den on 13.07.16.
  */
class DtLangInterpreterSpec extends FunSpec {
  private val scope: Map[String, DtType] = Map(
    "a" -> Str("AA"),
    "b" -> Str("BB")
  )
  private val assign = (name:String, value:Option[DtType]) => {
    println(name, value)
    value
  }

  private var procedures: Map[String, Node] = Map()

  private val interpreter = new DtLangInterpreter(scope, assign, procedures)

  private val lexer = DtLang.lexer
  private val syntax = DtLang.syntax(lexer)

  lexer.input(
    """
      |assign(a, 5)
    """.stripMargin)


  interpreter.evalExpr(syntax.getRootNode.get)

  //assert(true, "Ok")
}

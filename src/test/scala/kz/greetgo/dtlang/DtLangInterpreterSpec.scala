package kz.greetgo.dtlang

import java.util

import name.lakhin.eliah.projects.papacarlo.syntax.Node
import org.scalatest.FunSpec

/**
  * Created by den on 13.07.16.
  */
class DtLangInterpreterSpec extends FunSpec {
  private val scope: util.SortedMap[String, DtType] = new util.TreeMap()
  scope.put("a", Str("AA"))
  scope.put("b", Str("BB"))
  private val assign = (name:String, value:Option[DtType]) => {
    println(name, value)
    value
  }

  private var procedures: Map[String, Node] = Map()

  private val interpreter = new DtLangInterpreter(scope, procedures)

  private val lexer = DtLang.lexer
  private val syntax = DtLang.syntax(lexer)

  lexer.input(
    """
      |condition (
      |  case (1=1,
      |    group (
      |      assign(a, 5+2),
      |      assign(b, a~b)
      |    )
      |  )
      |)
    """.stripMargin)


  interpreter.evalExpr(syntax.getRootNode.get)

  println(scope)
  //assert(true, "Ok")
}

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
  scope.put("x.1", Str("x1"))
  scope.put("x.2", Str("x2"))
  scope.put("x.2.1", Str("x2.1"))
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
      |      assign(b, a~b),
      |      assign(l, len(x)),
      |      assign(dat1, date("2015-01-02")),
      |      assign(dat2, date(2015,1,2) + (4*day()+3*month()) )
      |    )
      |  )
      |)
    """.stripMargin)


  interpreter.evalExpr(syntax.getRootNode.get)

  println(scope)
  //assert(true, "Ok")
}

package kz.greetgo.dtlang

import name.lakhin.eliah.projects.papacarlo.lexis.TokenReference
import name.lakhin.eliah.projects.papacarlo.syntax.Node

import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.UndefOr
import scala.scalajs.js.annotation.JSExport

/**
  * Created by den on 20.06.16.
  */
@JSExport("DtLangParser")
object DtLangParser {
  private val lexer = DtLang.lexer
  private val syntax = DtLang.syntax(lexer)

  private var firstAddedExpr = Option.empty[Int]

  syntax.onNodeCreate.bind { node => // Root is last
    if (firstAddedExpr.isEmpty && node.getKind == "expr")
      firstAddedExpr = Some(node.getId)
  }

  //syntax.onNodeRemove.bind { node => lastRemovedNode = Some(node.getId) }
  // Is root last or first?

  val statements = List(
    "empty", "assign", "group",
    "condition", "case",
    "foreach", "break", "continue",
    "procedure", "exit", "stop", "message", "error"
  )

  @JSExport
  def replace(text: String, oldId: UndefOr[Int]): UndefOr[js.Dynamic] = {
    firstAddedExpr = None
    val node:Node = if (oldId.isDefined) {
      val oldNode: Node = syntax.getNode(oldId.get).get // WARN should find, otherwise bug is somewhere
      lexer.input(text, tokenPos(oldNode.getBegin), tokenPos(oldNode.getEnd, after = true))
      val n = firstAddedExpr;
      syntax.getNode(n.get).get // should be Some
    } else {
      lexer.input(text)
      syntax.getRootNode.get // should be Some
    }
    firstAddedExpr = None
    extractStats(node).orUndefined;
  }

  def extractStats(node: Node, parent: Option[Int] = None): Option[js.Dynamic] = {
    for (
      result <- node.getBranches.getOrElse("result", List.empty);
      call <- result.getBranches.getOrElse("call", List.empty);
      path <- result.getBranches.getOrElse("path", List.empty);
      segment <- path.getBranches.getOrElse("segment", List.empty);
      name <- segment.getValues.getOrElse("name", List.empty);
      if (statements contains name)
    ) {
      val id = node.getId
      val e = (i: Int) => call.getBranches("expr")(i).sourceCode
      val element = js.Dynamic.literal(
        "id" -> id,
        "parent" -> parent.map(_.toString).getOrElse[String]("#"),
        "text" -> (name match {
          case "assign" => {
            e(0) + " := " + e(1)
          }
          case "case" => {
            e(0)
          }
          case "foreach" => {
            "foreach " + e(0) + " := " + e(1) + " .. " + e(2)
          }
          case "break" => {
            val exprs = call.getBranches.get("expr")
            if (exprs.isEmpty) "break" else "break " + exprs.get(0).sourceCode
          }
          case "continue" => {
            val exprs = call.getBranches.get("expr")
            if (exprs.isEmpty) "break" else "break " + exprs.get(0).sourceCode
          }
          case "procedure" => {
            "procedure " + e(0)
          }
          case "message" => {
            "message " + e(0)
          }
          case "error" => {
            "error " + e(0)
          }
          case _ => name
        }),
        "type" -> name
      )
      val children = new js.Array[js.Any]
      for (subExpr <- call.getBranches.getOrElse("expr", List.empty)) {
        val child = extractStats(subExpr, Some(id))
        child.foreach(children.push(_))
      }
      if (children.length > 0) element.updateDynamic("children")(children)
      return Some(element)
    }
    None
  }

  private def tokenPos(token: TokenReference, after: Boolean = false) = {
    val c = token.collection.cursor(token.index + (if (after) 1 else 0))
    (c._1 - 1, c._2 - 1)
  }

  def main(args: Array[String]) {
    lexer.input("group(assign(x, 3))")
    val y = replace("empty()", 4)
    /*    val x = allNodeList(
          """
            |group ( /* comment */ // comment
            |  assign (x, 5),
            |  assign (client.account[type:in.account[1].type][1].sum, in.sum+100_0001 + 1+ 2*3%4),
            |  assign (client.surname, "Mr."~in.surname~1+2*3%4),
            |  condition (
            |    case (
            |      date("2016-03-04")-today()>month(5),
            |      group (
            |        stop (),
            |        exit ()
            |      )
            |    ),
            |    case (22+size(x) = 0, continue()),
            |    case (true,
            |      foreach (i, 1, 50,
            |        group (
            |          assign (x, x+1),
            |          condition (
            |            case (x>10,
            |              continue (i)),
            |            case (x<0,
            |              procedure (doIt))
            |          )
            |        )
            |      )
            |    )
            |  ),
            |  assign (
            |    y,
            |    min(max((1+2*3)/4, 0), 10) >= round(5.555_555__, 1) & ! (true () | false ())
            |  )
            |)
          """.stripMargin);
        println(x);*/
  }
}


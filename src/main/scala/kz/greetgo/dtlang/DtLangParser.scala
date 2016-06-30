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
  private var lastAddedNode = Option.empty[Int]
  private var lastRemovedNode = Option.empty[Int]
  //  private var addedNodes = List.empty[Int] // TODO remove
  //  private var removedNodes = List.empty[Int]

  syntax.onNodeMerge.bind { node => lastAddedNode = Some(node.getId) } // Root is last
  //syntax.onNodeRemove.bind { node => lastRemovedNode = Some(node.getId) }
  // Is root last or first?

  val statements = List(
    "empty", "assign", "group",
    "condition", "case",
    "foreach", "break", "continue",
    "procedure", "exit", "stop", "message", "error"
  )
  /*
    @JSExport
    def register(onStmtCreate: js.Function1[js.Dynamic, Unit], onStmtRemove: js.Function1[Int, Unit]) = {
      syntax.onNodeCreate.bind { node =>
        for (
          result <- node.getBranches.getOrElse("result", List.empty);
          call <- result.getBranches.getOrElse("call", List.empty);
          path <- result.getBranches.getOrElse("path", List.empty);
          segment <- path.getBranches.getOrElse("segment", List.empty);
          name <- segment.getValues.getOrElse("name", List.empty);
          if (statements contains name)
        ) {
          val dNode = js.Dynamic.literal(
            "id" -> node.getId,
            "parent" -> node.getParent.map(_.getId).orUndefined,
            "text" -> name,
            "type" -> name
          )
          onStmtCreate.apply(dNode)
        }
      }

      syntax.onNodeRemove.bind { node => onStmtRemove.apply(node.getId) }
    }

    private def branch(node: Node, name: String): Unit = {
      node.getBranches.getOrElse(name, List.empty)
    }
  */

  @JSExport
  def replace(text: String, oldId: UndefOr[Int]): UndefOr[js.Dynamic] = {
    oldId.map(id => {
      val oldNode: Node = syntax.getNode(id).get // WARN should find, otherwise bug is somewhere
      lexer.input(text, tokenPos(oldNode.getBegin), tokenPos(oldNode.getEnd, after = true))
    }
    ) orElse lexer.input(text)
    val n = lastAddedNode;
    lastAddedNode = None
    n.flatMap(syntax.getNode).flatMap(node => extractStats(node)).orUndefined;
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
      val element = js.Dynamic.literal(
        "id" -> id,
        "parent" -> parent.map(_.toString).getOrElse[String]("#"),
        "text" -> name,
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

  @JSExport
  def allNodeList(text: String) = { // TODO replace to extractStats
    lexer.input(text)
    val array = new js.Array[js.Any]
    if (syntax.getRootNode.isDefined)
      extractStatements(array, syntax.getRootNode.get);
    array
  }

  def extractStatements(array: js.Array[js.Any], node: Node, parent: Option[Int] = None): Unit = { // TODO remove
    for (
      result <- node.getBranches.getOrElse("result", List.empty);
      call <- result.getBranches.getOrElse("call", List.empty);
      path <- result.getBranches.getOrElse("path", List.empty);
      segment <- path.getBranches.getOrElse("segment", List.empty);
      name <- segment.getValues.getOrElse("name", List.empty);
      if (statements contains name)
    ) {
      val id = node.getId
      val element = js.Dynamic.literal(
        "id" -> id,
        "parent" -> parent.map(_.toString).getOrElse[String]("#"),
        "text" -> name,
        "type" -> name
      )
      array.push(element)
      for (subExpr <- call.getBranches.getOrElse("expr", List.empty))
        extractStatements(array, subExpr, Some(id))
    }
  }

  @JSExport
  def inputAll(text: String) {
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

  private def toJsArray(iterable: Iterable[js.Any]) = {
    val result = new js.Array[js.Any]

    for (element <- iterable) result.push(element)

    result
  }

  private def mapToObject(map: Map[String, js.Array[js.Any]]) = {
    val result = js.Dictionary.empty[js.Any]

    for ((key, values) <- map) result(key) = values

    result
  }

  private def exportNode(node: Node) = {
    val parentId = node.getParent.map(_.getId).getOrElse(-1)

    js.Dynamic.literal(
      "id" -> node.getId,
      "parent" -> parentId,
      "children" ->
        toJsArray(node.getBranches.map(_._2).flatten.map(_.getId: js.Any)),
      "kind" -> node.getKind,
      "values" -> mapToObject(node.getValues
        .map {
          case (key, values) =>
            key -> toJsArray(values.map(s => s: js.Any))
        }
      )
    )
  }

  private def tokenPos(token: TokenReference, after: Boolean = false) = {
    token.collection.cursor(token.index + (if (after) 1 else 0))
  }

  private def tokenCursor(token: TokenReference, after: Boolean = false) = {
    val pair = tokenPos(token, after)
    js.Dynamic.literal("line" -> (pair._1 - 1), "ch" -> (pair._2 - 1))
  }

  def main(args: Array[String]) {
    lexer.input("group(assign(x, 3))")
    val y = replace("empty()", 4)
    val x = allNodeList(
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
    println(x);
  }
}


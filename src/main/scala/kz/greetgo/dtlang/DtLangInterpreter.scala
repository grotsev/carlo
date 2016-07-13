package kz.greetgo.dtlang

import name.lakhin.eliah.projects.papacarlo.Syntax
import name.lakhin.eliah.projects.papacarlo.syntax.Node

/**
  * Created by den on 11.07.16.
  */
class DtLangInterpreter(scope: Map[String, DtType], assign: Function2[String, Option[DtType], Option[DtType]], procedures: Map[String, Node]) {
  var messages: List[String] = Nil

  private def group(exprs: List[Node]): Option[DtType] = {
    var last: Option[DtType] = None
    for (expr <- exprs)
      last = evalExpr(expr)
    last
  }

  private def condition(exprs: List[Node]): Option[DtType] = {
    for (expr <- exprs)
      evalExpr(expr) match {
        case None => // skip
        case x => return x
      }
    None
  }


  def foreach(ref: String, from: BigDecimal, to: Option[BigDecimal], body: Node): Option[DtType] = {
    var i = from
    try {
      while (to.isEmpty || i <= to.get) {
        assign(ref, Some(Num(i)))
        try {
          evalExpr(body)
        } catch {
          case e: ContinueException if e.label.isEmpty || e.label.get == ref => // skip
        }
        i = i + 1
      }
    } catch {
      case e: BreakException if e.label.isEmpty || e.label.get == ref => // skip
    }
    None // foreach is always None
  }

  private def toStr(v: DtType): Option[String] = {
    v match {
      case Str(v) => Some(v)
      case _ => None
    }
  }

  def evalExpr(expr: Node): Option[DtType] = {
    val result = expr.getBranches("result")(0)
    if (result.getValues.contains("num")) {
      Some(Num(BigDecimal(result.getValues("num")(0))))
    } else if (result.getValues.contains("str")) {
      val s: String = result.getValues("str")(0)
      Some(Str(s.substring(1, s.length - 1)))
    } else {
      val path = result.getBranches("path")(0)
      if (result.getBranches.contains("call")) {
        // statement or function call
        val call = result.getBranches("call")(0)
        val arg = (i: Int) => call.getBranches("expr")(i)
        val strArg0 = () => call.getBranches.get("expr").map(_.head.sourceCode)
        path.sourceCode match {
          case "empty" => None
          case "assign" => assign(evalPath(arg(0)), evalExpr(arg(1)))
          case "group" => group(call.getBranches("expr"))

          case "condition" => condition(call.getBranches("expr"))
          case "case" => evalExpr(arg(0)) match {
            case Some(Bool(b)) if b => evalExpr(arg(1))
            case _ => None
          }

          case "foreach" => {
            val ref = arg(0).sourceCode // just simple path with one segment
            evalExpr(arg(1)) match {
              case Some(Num(from)) =>
                call.getBranches("expr").size match {
                  case 3 => foreach(ref, from, None, arg(2))
                  case 4 => evalExpr(arg(2)) match {
                    case Some(Num(to)) => foreach(ref, from, Some(to), arg(3))
                    case _ => None // to is not num
                  }
                  case _ => None // exprs size is not 3 or 4
                }
              case _ => None // from is not num
            }
          }
          case "break" => throw new BreakException(strArg0())
          case "continue" => throw new ContinueException(strArg0())

          case "procedure" => procedures.get(arg(0).sourceCode).flatMap(node => {
            try {
              evalExpr(node)
            } catch {
              case e: BreakException => None
              case e: ContinueException => None
              case e: ExitException => e.value
            }
          })
          case "exit" => throw new ExitException(call.getBranches.get("expr").flatMap(node => evalExpr(node.head)))
          case "stop" => throw new StopException
          case "message" => {
            messages = (evalExpr(arg(0)) match {
              case Some(Str(str)) => str
            }) :: messages
            None
          }
          case "error" => throw new ErrorException(strArg0())
          /* TODO
        case "len" =>
        case "min" =>
        case "max" =>
        case "round" =>
        case "power" =>

        case "true" =>
        case "false" =>

        case "today" =>
        case "day" =>
        case "month" =>
        case "year" =>*/
        }
      } else {
        // variable access
        scope.get(evalPath(path)) // TODO
      }
    }
  }

  def evalPath(path: Node): String = {
    path.sourceCode // TODO
  }

  /*
    def step() {
      val nodeId = stack.head
      val node = syntax.getNode(nodeId).get
      node.getBranches()
    }

    def stepInto()

    def stepOver()

    def run()

    def setBreakPoint(nodeId: Int)

    def clearBreakPoint(nodeId: Int)
  */
}

private class BreakException(val label: Option[String]) extends RuntimeException

private class ContinueException(val label: Option[String]) extends RuntimeException

private class ExitException(val value: Option[DtType]) extends RuntimeException

private class StopException() extends RuntimeException

private class ErrorException(val msg: Option[String]) extends RuntimeException
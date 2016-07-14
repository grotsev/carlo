package kz.greetgo.dtlang

import java.util
import java.util.OptionalInt
import java.util.function.IntPredicate

import name.lakhin.eliah.projects.papacarlo.Syntax
import name.lakhin.eliah.projects.papacarlo.syntax.Node

/**
  * Created by den on 11.07.16.
  */
class DtLangInterpreter(scope: util.SortedMap[String, DtType], procedures: Map[String, Node]) {
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

  private def assign(ref: String, value: Option[DtType]) = {
    if (value.isEmpty) {
      scope.remove(ref)
    } else {
      scope.put(ref, value.get)
    }
    value
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

  private def unaryOp(expr: Node, f: Function[Option[DtType], Option[DtType]]) = {
    val operand = evalExpr(expr.getBranches("operand").head)
    f(operand)
  }

  private def unaryNumOp(expr: Node, f: Function[BigDecimal, BigDecimal]) = {
    unaryOp(expr, {
      case Some(Num(o)) => Some(Num(f(o)))
      case _ => None
    })
  }

  private def binaryOp(expr: Node, f: Function2[Option[DtType], Option[DtType], Option[DtType]]) = {
    val left = evalExpr(expr.getBranches("left").head)
    val right = evalExpr(expr.getBranches("right").head)
    f(left, right)
  }

  private def numOp(expr: Node, f: Function2[BigDecimal, BigDecimal, BigDecimal]) = {
    binaryOp(expr, (left, right) => (left, right) match {
      case (Some(Num(l)), Some(Num(r))) => Some(Num(f(l, r)))
      case _ => None
    })
  }

  private def toStr(value: Option[DtType]): String = value match {
    case None => ""
    case Some(Bool(v)) => v.toString
    case Some(Num(v)) => v.toString
    case Some(Str(v)) => v.toString
    case Some(Dat(v)) => v.toString
  }

  private def compareOp(expr: Node, p: Int => Boolean): Option[DtType] = {
    binaryOp(expr, (left, right) => (left, right) match {
      case (Some(Bool(l)), Some(Bool(r))) => Some(Bool(p(l.compareTo(r))))
      case (Some(Num(l)), Some(Num(r))) => Some(Bool(p(l.compare(r))))
      case (Some(Str(l)), Some(Str(r))) => Some(Bool(p(l.compareTo(r))))
      case (Some(Dat(l)), Some(Dat(r))) => Some(Bool(p(l.compareTo(r))))
    })
  }

  def evalExpr(expr: Node): Option[DtType] = {
    expr.getKind match {
      case "expr" => evalExpr(expr.getBranches("result").head)

      case "+" =>
        if (expr.getBranches.contains("operand")) unaryNumOp(expr, identity)
        else numOp(expr, _ + _)
      case "-" =>
        if (expr.getBranches.contains("operand")) unaryNumOp(expr, _.unary_-)
        else numOp(expr, _ + _)
      case "!" =>
        unaryOp(expr, {
          case Some(Bool(o)) => Some(Bool(!o))
          case _ => None
        })

      case "*" =>
        numOp(expr, _ * _)
      case "/" =>
        numOp(expr, _ / _)
      case "%" =>
        numOp(expr, _ % _)

      case "~" =>
        binaryOp(expr, (left, right) => Some(Str(toStr(left) + toStr(right))))

      case "<" =>
        compareOp(expr, _ < 0)
      case ">" =>
        compareOp(expr, _ > 0)
      case "<=" =>
        compareOp(expr, _ <= 0)
      case ">=" =>
        compareOp(expr, _ >= 0)

      case "=" =>
        compareOp(expr, _ == 0)
      case "!=" =>
        compareOp(expr, _ != 0)

      case "&" =>
        evalExpr(expr.getBranches("left").head) match {
          case x@Some(Bool(false)) => x
          case Some(Bool(_)) => evalExpr(expr.getBranches("right").head)
          case _ => {
            evalExpr(expr.getBranches("right").head) match {
              case x@Some(Bool(false)) => x
              case _ => None
            }
          }
        }
      case "|" =>
        evalExpr(expr.getBranches("left").head) match {
          case x@Some(Bool(true)) => x
          case Some(Bool(_)) => evalExpr(expr.getBranches("right").head)
          case _ => {
            evalExpr(expr.getBranches("right").head) match {
              case x@Some(Bool(true)) => x
              case _ => None
            }
          }
        }

      case "atom" =>
        if (expr.getValues.contains("num")) {
          Some(Num(BigDecimal(expr.getValues("num").head)))
        } else if (expr.getValues.contains("str")) {
          val s: String = expr.getValues("str").head
          Some(Str(s.substring(1, s.length - 1)))
        } else {
          val path = expr.getBranches("path").head
          if (expr.getBranches.contains("call"))
            evalFun(path.sourceCode, expr.getBranches("call").head.getBranches.getOrElse("expr", List()))
          else
            Option(scope get evalPath(path)) // variable access
        }
    }
  }

  private def evalFun(name: String, arg: List[Node]): Option[DtType] = {
    name match {
      case "empty" => None
      case "assign" => assign(evalPath(arg(0)), evalExpr(arg(1)))
      case "group" => group(arg)

      case "condition" => condition(arg)
      case "case" => evalExpr(arg(0)) match {
        case Some(Bool(b)) if b => evalExpr(arg(1))
        case _ => None
      }

      case "foreach" => {
        val ref = arg(0).sourceCode // just simple path with one segment
        evalExpr(arg(1)) match {
          case Some(Num(from)) =>
            arg.size match {
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
      case "break" => throw new BreakException(if (arg.size == 0) None else Some(arg(0).sourceCode))
      case "continue" => throw new ContinueException(if (arg.size == 0) None else Some(arg(0).sourceCode))

      case "procedure" => procedures.get(arg(0).sourceCode).flatMap(node => {
        try {
          evalExpr(node)
        } catch {
          case e: BreakException => None
          case e: ContinueException => None
          case e: ExitException => e.value
        }
      })
      case "exit" => throw new ExitException(if (arg.size == 0) None else evalExpr(arg(0)))
      case "stop" => throw new StopException
      case "message" => {
        evalExpr(arg(0)) match {
          case Some(Str(str)) => messages = str :: messages
          case _ =>
        }
        None
      }
      case "error" => throw new ErrorException(if (arg.size == 0) None else Some(arg(0).sourceCode))
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
  }

  private def evalPath(path: Node): String = {
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
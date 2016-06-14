package kz.greetgo.dtlang

import name.lakhin.eliah.projects.papacarlo.lexis.{Contextualizer, Matcher, Token, Tokenizer}
import name.lakhin.eliah.projects.papacarlo.syntax.Rule._
import name.lakhin.eliah.projects.papacarlo.{Lexer, Syntax}
import name.lakhin.eliah.projects.papacarlo.syntax.rules.NamedRule
import name.lakhin.eliah.projects.papacarlo.syntax.{Expressions, Rule}

/**
  * Created by den on 09.06.16.
  */
object DtLang {
  private def tokenizer = {
    val tokenizer = new Tokenizer()

    import tokenizer._
    import Matcher._

    tokenCategory("whitespace",
      oneOrMore(anyOf(" \t\f\n"))
    ).skip

    tokenCategory("string",
      sequence(
        chunk("\""),
        zeroOrMore(choice(
          anyExceptOf("\n\r\\\""),
          sequence(chunk("\\"), anyOf("\"\\/bfnrt")),
          sequence(
            chunk("\\u"),
            repeat(
              choice(rangeOf('a', 'f'), rangeOf('A', 'F'), rangeOf('0', '9')),
              times = 4
            )
          )
        )),
        chunk("\"")
      )
    )

    tokenCategory("date",
      sequence(
        repeat(rangeOf('0', '9'), times = 4),
        chunk("-"),
        repeat(rangeOf('0', '9'), times = 2),
        chunk("-"),
        repeat(rangeOf('0', '9'), times = 2)
      )
    )

    tokenCategory("number",
      sequence(
        optional(chunk("-")),
        choice(
          chunk("0"),
          sequence(rangeOf('1', '9'), zeroOrMore(rangeOf('0', '9')))
        ),
        optional(sequence(chunk("."), oneOrMore(rangeOf('0', '9')))),
        optional(sequence(
          anyOf("eE"),
          optional(anyOf("+-")),
          oneOrMore(rangeOf('0', '9'))
        ))
      )
    )

    tokenCategory("name",
      sequence(
        choice(rangeOf('a', 'z'), rangeOf('A', 'Z'), chunk("_")),
        zeroOrMore(choice(rangeOf('a', 'z'), rangeOf('A', 'Z'), rangeOf('0', '9'), chunk("_")))
      )
    )

    terminals(".", ",", ":=", ":", "{", "}", "[", "]", "(", ")",
      "+", "-", "*", "/", "%", "<=", "<", ">=", ">", "!=", "=", "||",
      "//", "/*", "*/")

    keywords("true", "false", "not", "and", "or", "null")

    tokenizer
  }

  private def contextualizer = {
    val contextualizer = new Contextualizer

    import contextualizer._

    trackContext("[", "]").allowCaching
    trackContext("{", "}").allowCaching
    trackContext("//", Token.LineBreakKind).forceSkip.topContext
    trackContext("/*", "*/").forceSkip.topContext

    contextualizer
  }

  def lexer = new Lexer(tokenizer, contextualizer)

  def syntax(lexer: Lexer) = new {
    val syntax = new Syntax(lexer)

    import syntax._
    import Rule._
    import Expressions._

    val expr: Rule = rule("expression").cachable.main {
      val rule =
        expression(branch("operand", recover(atom, "operand required")))

      var p = 1
      group(rule, "(", ")")
      prefix(rule, "+", p)
      prefix(rule, "-", p)
      prefix(rule, "not", p)
      p += 1;
      infix(rule, "*", p)
      infix(rule, "/", p)
      infix(rule, "%", p)
      p += 1;
      infix(rule, "+", p)
      infix(rule, "-", p)
      p += 1;
      infix(rule, "||", p)
      p += 1;
      infix(rule, "<", p)
      infix(rule, ">", p)
      infix(rule, "<=", p)
      infix(rule, ">=", p)
      p += 1;
      infix(rule, "=", p)
      infix(rule, "!=", p)
      p += 1;
      infix(rule, "and", p)
      p += 1;
      infix(rule, "or", p)
      p += 1;
      infix(rule, ":=", p) // TODO move

      rule
    }

    val atom = rule("atom") {
      choice(
        token("string"),
        token("date"),
        token("number"),
        choice(token("true"), token("false")),
        branch("path", path)
      )
    }

    val path = rule("path") {
      oneOrMore(
        branch("segment", segment),
        separator =
          recover(token("."), "path entries must be separated with . sign")
      )
    }

    val segment = rule("segment") {
      sequence(
        capture("attribute", token("name")),
        branch("index", zeroOrMore(index))
      )
    }

    val index = rule("index") {
      sequence(
        token("["),
        optional(sequence(
          branch("field", token("name")),
          token(":")
        )),
        branch("filter", expr),
        recover(token("]"), "filter must end with ] sign")
      )
    }

  }.syntax
}

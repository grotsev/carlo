package kz.greetgo.dtlang

import name.lakhin.eliah.projects.papacarlo.lexis.{Contextualizer, Matcher, Token, Tokenizer}
import name.lakhin.eliah.projects.papacarlo.{Lexer, Syntax}
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

    tokenCategory("number",
      sequence(
        optional(chunk("-")),
        choice(
          chunk("0"),
          sequence(rangeOf('1', '9'), zeroOrMore(choice(rangeOf('0', '9'), chunk("_"))))
        ),
        optional(sequence(chunk("."), oneOrMore(choice(rangeOf('0', '9'), chunk("_")))))
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

    keywords(
      "true", "false",
      "not", "and", "or",
      "min", "max", "round", "sizeof",
      "today", "date", "year", "month", "day",
      "case", "null")

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

    // expr

    val expr: Rule = rule("expression").cachable.main {
      val rule =
        expression(branch("operand", recover(atom, "operand required")))

      var p = 1
      group(rule, "(", ")")
      prefix(rule, "+", p)
      prefix(rule, "-", p)
      prefix(rule, "not", p)
      prefix(rule, "min", p)
      prefix(rule, "max", p)
      prefix(rule, "round", p)
      prefix(rule, "sizeof", p)

      prefix(rule, "date", p)
      prefix(rule, "year", p)
      prefix(rule, "month", p)
      prefix(rule, "day", p)
      postfix(rule, "year", p)
      postfix(rule, "month", p)
      postfix(rule, "day", p)

      p += 1;
      infix(rule, "round", p)

      p += 1;
      infix(rule, "*", p)
      infix(rule, "/", p)
      infix(rule, "%", p)

      p += 1;
      infix(rule, "+", p)
      infix(rule, "-", p)

      p += 1;
      infix(rule, "min", p)
      infix(rule, "max", p)

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
        token("number"),
        choice(token("true"), token("false")),
        token("today"),
        branch("path", path)
      )
    }

    // path

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

    // stmt

    val stmt = rule("stmt") {
      choice(seq, alt, loop, asn)
    }

    val seq: Rule = rule("seq") {
      sequence(
        token("{"),
        zeroOrMore(
          branch("stmt", stmt)
        ),
        recover(token("}"), "sequence must end with } sign")
      )
    }

    val alt = rule("alt") {
      sequence(
        token("case"),
        token("{"),
        zeroOrMore(sequence(
          branch("expr", expr),
          token(":"),
          branch("stmt", stmt)
        )),
        recover(token("}"), "sequence must end with } sign")
      )
    }

    val loop = rule("loop") {
      sequence(
        path,
        token(":"),
        range,
        branch("stmt", stmt)
      )
    }

    val range = rule("range") {
      choice(
        // TODO 1 to 10, 1 until 10, path, numbered path
      )
    }

    val asn = rule("asn") {
      // TODO
      sequence(
        branch("path", path),
        token(":="),
        branch("expr", expr),
        recover(token(";"), "assign must end with ; sign")
      )
    }

    // TODO break, continue, call?

  }.syntax
}

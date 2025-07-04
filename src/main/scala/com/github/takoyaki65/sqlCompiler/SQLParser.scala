package com.github.takoyaki65.sqlCompiler

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.PackratParsers

/** SQL Parser that takes a SQL-like string and converts it to tree of
  * operators.
  */
trait SQLParser extends QueryAST {

  def parseSql(input: String) = Grammar.parseAll(input)

  object Grammar extends JavaTokenParsers with PackratParsers {

    def stm: Parser[Operator] =
      selectClause ~ fromClause ~ whereClause ~ groupClause ^^ {
        case p ~ s ~ f ~ g => g(p(f(s)))
      }
    def selectClause: Parser[Operator => Operator] =
      "select" ~> ("*" ^^ { _ => (op: Operator) => op } | fieldList ^^ {
        case (fs, fs1) => Project(fs, fs1, _: Operator)
      })
    def fromClause: Parser[Operator] = "from" ~> joinClause
    def whereClause: Parser[Operator => Operator] =
      opt("where" ~> predicate ^^ { p => (op: Operator) =>
        Filter(p, op): Operator
      }) ^^ {
        _.getOrElse(op => op)
      }
    def groupClause: Parser[Operator => Operator] =
      opt("group" ~> "by" ~> fieldIdList ~ ("sum" ~> fieldIdList) ^^ {
        case p1 ~ p2 => (op: Operator) => Group(p1, p2, op): Operator
      }) ^^ { _.getOrElse(op => op) }

    def joinClause: Parser[Operator] =
      ("nestedloops" ~> repsep(tableClause, "join") ^^ {
        _.reduceLeft((a, b) => Join(a, b))
      }) |
        (repsep(tableClause, "join") ^^ {
          _.reduceLeft((a, b) => HashJoin(a, b))
        })
    def tableClause: Parser[Operator] =
      tableIdent ~ opt("schema" ~> fieldIdList) ~ opt(
        "delim" ~> ("""\t""" ^^ (_ => '\t') | """.""".r ^^ (_.head))
      ) ^^ { case table ~ schema ~ delim =>
        Scan(table, schema, delim)
      } |
        ("(" ~> stm <~ ")")

    def fieldIdent: Parser[String] = """[\w\#]+""".r
    def tableIdent: Parser[String] = """[\w_\-/\.]+""".r | "?"
    def fieldList: Parser[(Schema, Schema)] =
      repsep(fieldIdent ~ opt("as" ~> fieldIdent), ",") ^^ { fs2s =>
        val (fs, fs1) = fs2s.map { case a ~ b => (b.getOrElse(a), a) }.unzip
        (Schema(fs*), Schema(fs1*))
      }
    def fieldIdList: Parser[Schema] =
      repsep(fieldIdent, ",") ^^ { fs => Schema(fs*) }

    def predicate: Parser[Predicate] =
      ref ~ "=" ~ ref ^^ { case a ~ _ ~ b => Eq(a, b) }
    def ref: Parser[Ref] =
      fieldIdent ^^ { s => Field(s) } |
        """'[^']*'""".r ^^ { s => Value(s.drop(1).dropRight(1)) } |
        """[0-9]+""".r ^^ { s => Value(s.toInt) }

    def parseAll(input: String): Operator = parseAll(stm, input) match {
      case Success(res, _) => res
      case res             => throw new Exception(res.toString)
    }
  }
}

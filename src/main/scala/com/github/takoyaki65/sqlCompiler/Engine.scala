package com.github.takoyaki65.sqlCompiler

import java.io.PrintStream
import java.io.ByteArrayOutputStream
import scala.compiletime.uninitialized

trait Engine extends QueryProcessor with SQLParser {
  def query: String
  def filename: String
  def liftTable(n: String): Table
  def eval: Unit
  def prepare: Unit = {}
  def run: Unit = execQuery(PrintCSV(parseSql(query)))
  override def dynamicFilePath(table: String): Table =
    liftTable(if (table == "?") filename else filePath(table))
  def evalString = {
    val source = new ByteArrayOutputStream
    Utils.withOutputFull(new PrintStream(source)) {
      eval
    }
    source.toString
  }
}

object Run {
  var qu: String = uninitialized
  var fn: String = uninitialized

  trait MainEngine extends Engine {
    override def query = qu
    override def filename = fn
  }

  def unstaged_engine: Engine =
    new Engine with MainEngine with step1.QueryUnstaged.QueryInterpreter {
      override def liftTable(n: Table) = n
      override def eval = run
    }

  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("syntax:")
      println("    test run (unstaged) sql [file]")
      println()
      println("example usage:")
      println(
        "    test run unstaged \"select * from ? schema Phrase, Year, MatchCount, VolumeCount, delim \\t where Phrase='Auswanderung'\" src/data/t1gram.csv"
      )
      return
    }
    val version = args(0)
    val engine = version match {
      case "unstaged" => unstaged_engine
      case _ =>
        println(s"warning: unexpected engine, using 'unstaged' by default")
        unstaged_engine
    }
    qu = args(1)
    if (args.length > 2) {
      fn = args(2)
    }

    try {
      engine.prepare
      Utils.time(engine.eval)
    } catch {
      case ex: Exception =>
        println("ERROR: " + ex.getMessage)
    }
  }
}

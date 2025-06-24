package com.github.takoyaki65.sqlCompiler

import org.scalatest.funsuite.FunSuite
import java.io.File
import java.io.FileInputStream
import java.io.IOException
import java.io.PrintWriter
import java.io.ByteArrayOutputStream
import java.io.StringWriter
import java.io.PrintStream

trait LibSuite extends FunSuite {
  def dataFilePath(csv: String) = "data/" + csv
}

trait TutorialFunSuite extends LibSuite {
  // should be false; temporary set to true only to simplify development
  val overwriteCheckFiles = false

  val prefix = "out/"
  val under = "query_"
  def readFile(name: String): String = {
    try {
      val buf = new Array[Byte](new File(name).length().toInt)
      val fis = new FileInputStream(name)
      fis.read(buf)
      fis.close()
      new String(buf)
    } catch {
      case e: IOException => ""
    }
  }
  def writeFile(name: String, content: String): Unit = {
    val out = new PrintWriter(new File(name))
    out.write(content)
    out.close()
  }
  def writeFileIndented(name: String, content: String) {
    val out = new PrintWriter(new File(name))
    printIndented(content)(out)
    out.close()
  }
  def checkOut(label: String, suffix: String, thunk: => Unit) = {
    val output = new ByteArrayOutputStream()
    scala.Console.withOut(new PrintStream(output))(thunk)
    check(label, output.toString(), suffix = suffix)
  }
  def check(label: String, raw_code: String, suffix: String = "scala") = {
    val fileprefix = prefix + under + label
    val name = fileprefix + ".check." + suffix
    val aname = fileprefix + ".actual." + suffix
    val expected = readFile(name)
    val code = indent(raw_code)

    // println(s"expected:\n$expected")
    // println(s"actual:\n$code")
    if (expected != code) {
      val wname = if (overwriteCheckFiles) name else aname
      println("writing " + wname)
      writeFile(wname, code)
    } else {
      val f = new File(aname)
      if (f.exists) f.delete
    }
    assert(expected == code, name)
  }
  def indent(str: String): String = {
    val s = new StringWriter
    printIndented(str)(new PrintWriter(s))
    s.toString()
  }
  def printIndented(src: String)(out: PrintWriter): Unit = {
    val lines = src.split("[\n\r]")
    var indent = 0
    for (l0 <- lines) {
      val l = l0.trim
      if (l.length > 0) {
        var open = 0
        var close = 0
        var initClose = 0
        var nonWsChar = false
        l foreach {
          case '{' => {
            open += 1
            if (!nonWsChar) {
              nonWsChar = true
              initClose = close
            }
          }
          case '}' => close += 1
          case x =>
            if (!nonWsChar && !x.isWhitespace) {
              nonWsChar = true
              initClose = close
            }
        }
        if (!nonWsChar) initClose = close
        out.println("  " * (indent - initClose) + l)
        indent += (open - close)
      }
    }
    assert(indent == 0, "indent mismatch")
  }

  def exec(label: String, code: String, suffix: String = "scala"): Unit = {
    val filePrefix = prefix + under + label
    val aname = filePrefix + ".actual." + suffix
    writeFileIndented(aname, code)
  }
}

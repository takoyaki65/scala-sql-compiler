package com.github.takoyaki65.sqlCompiler

import java.io.File
import java.io.FileInputStream
import java.io.IOException
import java.io.PrintWriter
import java.io.ByteArrayOutputStream
import java.io.StringWriter
import java.io.PrintStream
import java.io.OutputStream

object Utils {
  def time[A](a: => A): A = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println(s"${micros} microseconds")
    result
  }
  def captureLocalOut(func: => Any): String = {
    val source = new ByteArrayOutputStream
    withOutput(new PrintStream(source))(func)
    source.toString
  }
  def captureOut(func: => Any): String = {
    val source = new ByteArrayOutputStream
    withOutputFull(new PrintStream(source))(func)
    source.toString
  }
  def withOutput[T](out: PrintStream)(f: => Unit): Unit = {
    scala.Console.withOut(out)(scala.Console.withErr(out)(f))
  }
  def devnull(f: => Unit): Unit = {
    withOutput(nullout)(f)
  }
  def nullout = new PrintStream(new OutputStream() {
    override def write(b: Int): Unit = {}
    override def write(b: Array[Byte]) = {}
    override def write(b: Array[Byte], off: Int, len: Int) = {}
  })
  def withOutputFull(out: PrintStream)(func: => Unit): Unit = {
    val oldStdOut = System.out
    val oldStdErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      scala.Console.withOut(out)(scala.Console.withErr(out)(func))
    } finally {
      out.flush()
      out.close()
      System.setOut(oldStdOut)
      System.setErr(oldStdErr)
    }
  }
}

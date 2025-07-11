package com.github.takoyaki65.sqlCompiler

import java.io.BufferedReader
import java.io.FileReader

class Scanner(filename: String) {
  val br = new BufferedReader(new FileReader(filename))
  var pending: String = br.readLine()
  def next(delim: Char): String = {
    if (delim == '\n') {
      val field = pending
      pending = br.readLine()
      field
    } else {
      val i = pending.indexOf(delim)
      val field = pending.substring(0, i)
      pending = pending.substring(i + 1)
      field
    }
  }
  def hasNext = pending ne null
  def close = br.close
}

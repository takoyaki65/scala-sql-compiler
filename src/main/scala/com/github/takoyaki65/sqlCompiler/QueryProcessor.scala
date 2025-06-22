package com.github.takoyaki65.sqlCompiler

/** Some plumbing for the multiple query processors we're implementing.
  */

trait QueryProcessor extends QueryAST {
  def version: String
  def defaultFieldDelimiter = ','

  def filePath(table: String): String = table
  def dynamicFilePath(table: String): Table

  def Scan(
      tableName: String,
      schema: Option[Vector[String]],
      delim: Option[Char]
  ): Scan = {
    val dfile = dynamicFilePath(tableName)
    val (schema1, externalSchema) = schema
      .map(s => (s, true))
      .getOrElse(loadSchema(filePath(tableName)), false)
    Scan(dfile, schema1, delim.getOrElse(defaultFieldDelimiter), externalSchema)
  }

  def loadSchema(filename: String): Schema = {
    val s = new Scanner(filename)
    val schema = Schema(s.next('\n').split(defaultFieldDelimiter): _*)
    s.close
    schema
  }

  def execQuery(q: Operator): Unit
}

trait PlainQueryProcessor extends QueryProcessor {
  type Table = String
}

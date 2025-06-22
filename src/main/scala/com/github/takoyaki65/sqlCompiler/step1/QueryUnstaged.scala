package com.github.takoyaki65.sqlCompiler.step1

import com.github.takoyaki65.sqlCompiler.PlainQueryProcessor
import com.github.takoyaki65.sqlCompiler.Scanner

/** Simple Query Interpreter
  */

object QueryUnstaged {

  trait QueryInterpreter extends PlainQueryProcessor {
    override def version = "query_unstaged"

    /** Low-level Processing Logic
      */
    type Fields = Vector[String]

    case class Record(fields: Fields, schema: Schema) {
      def apply(key: String): String = fields(schema indexOf key)
      def apply(keys: Schema): Fields = keys.map(this apply _)
    }

    def processCSV(
        filename: String,
        schema: Schema,
        fieldDelimiter: Char,
        externalSchema: Boolean
    )(yld: Record => Unit): Unit = {
      val s = new Scanner(filename)
      val last = schema.last
      val nextRecord = Record(
        schema.map { x => s.next(if (x == last) '\n' else fieldDelimiter) },
        schema
      )
      if (!externalSchema) {
        // the right thing would be to dynamically re-check the schema,
        // but it clutters the generated code
        // schema.foreach(f => if (s.next != f) println("ERROR: schema mismatch"))
        nextRecord // ignore csv header
      }
      while (s.hasNext) {
        yld(nextRecord)
      }
      s.close
    }

    def printSchema(schema: Schema) = println(
      schema.mkString(defaultFieldDelimiter.toString)
    )

    def printFields(fields: Fields) = printf(
      fields
        .map { _ => "%s" }
        .mkString("", defaultFieldDelimiter.toString, "\n"),
      fields: _*
    )

    /** Query Interpretation
      */

    def evalPred(p: Predicate)(rec: Record): Boolean = p match {
      case Eq(a1, a2) => evalRef(a1)(rec) == evalRef(a2)(rec)
    }

    def evalRef(r: Ref)(rec: Record): String = r match {
      case Field(name) => rec(name)
      case Value(x)    => x.toString
    }

    import scala.collection.mutable.{ArrayBuffer, HashMap}

    def resultSchema(o: Operator): Schema = o match {
      case Scan(name, schema, delim, externalSchema) => schema
      case Filter(pred, parent)                      => resultSchema(parent)
      case Project(outSchema, inSchema, parent)      => outSchema
      case Join(left, right)        => resultSchema(left) ++ resultSchema(right)
      case Group(keys, agg, parent) => keys ++ agg
      case HashJoin(left, right)    => resultSchema(left) ++ resultSchema(right)
      case PrintCSV(parent)         => resultSchema(parent)
    }

    def execOp(o: Operator)(yld: Record => Unit): Unit = o match {
      case Scan(filename, schema, fieldDelimiter, externalSchema) =>
        processCSV(filename, schema, fieldDelimiter, externalSchema)(yld)
      case Filter(pred, parent) =>
        execOp(parent) { rec => if (evalPred(pred)(rec)) yld(rec) }
      case Project(newSchema, parentSchema, parent) =>
        execOp(parent) { rec => yld(Record(rec(parentSchema), newSchema)) }
      case Join(left, right) =>
        execOp(left) { rec1 =>
          execOp(right) { rec2 =>
            val keys = rec1.schema intersect rec2.schema
            if (rec1(keys) == rec2(keys))
              yld(
                Record(rec1.fields ++ rec2.fields, rec1.schema ++ rec2.schema)
              )
          }
        }
      case Group(keys, agg, parent) =>
        val hm = new HashMap[Fields, Seq[Int]]
        execOp(parent) { rec =>
          val kvs = rec(keys)
          val sums = hm.getOrElse(kvs, agg.map(_ => 0))
          hm(kvs) = sums.zip(rec(agg).map(_.toInt)).map(x => x._1 + x._2)
        }
        hm foreach { case (k, a) =>
          yld(Record(k ++ a.map(_.toString), keys ++ agg))
        }
      case HashJoin(left, right) =>
        val keys = resultSchema(left) intersect resultSchema(right)
        val hm = new HashMap[Fields, ArrayBuffer[Record]]
        execOp(left) { rec1 =>
          val buf = hm.getOrElse(rec1(keys), new ArrayBuffer[Record])
          buf += rec1
        }
        execOp(right) { rec2 =>
          hm.get(rec2(keys)) foreach {
            _.foreach { rec1 =>
              yld(
                Record(rec1.fields ++ rec2.fields, rec1.schema ++ rec2.schema)
              )
            }
          }
        }
      case PrintCSV(parent) =>
        val schema = resultSchema(parent)
        printSchema(schema)
        execOp(parent) { rec => printFields(rec(schema)) }
    }

    def execQuery(q: Operator): Unit = execOp(q) { _ => }
  }
}

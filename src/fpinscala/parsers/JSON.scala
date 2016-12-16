package fpinscala.parsers

import language.higherKinds
import language.implicitConversions

trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+ _]](P: Parsers[Err, Parser]): Parser[JSON] = {

    import P._
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    def array = surround("[", "]")(value sep "," map (vs => JArray(vs.toIndexedSeq))) scope "array"

    def obj = surround("{", "}")(keyval sep "," map (kvs => JObject(kvs.toMap))) scope "object"

    def keyval = escapedQuoted ** (":" *> value)

    def lit = scope("literal") {
      "null".as(JNull) |
        double.map(JNumber) |
        escapedQuoted.map(JString) |
        "true".as(JBool(true)) |
        "false".as(JBool(false))
    }

    def value: Parser[JSON] = lit | obj | array

    root(whitespace *> (obj | array))
  }
}


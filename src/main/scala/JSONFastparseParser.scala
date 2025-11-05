import fastparse.all._

object JSONFastparseParser {
  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
    def apply(t: T) = f(t)
    override def toString() = name

  }

  // Here is the parser
  val Whitespace = NamedFunction(" \n".contains(_: Char), "Whitespace")
  val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  val space         = P( CharsWhile(Whitespace).? )
  val digits        = P( CharsWhile(Digits))
  val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
  val fractional    = P( "." ~ digits )
  val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )

  val number = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
    x => JSON.Number(x.toDouble)
  )

  val `null`        = P( "null" ).map(_ => JSON.Null)
  val `false`       = P( "false" ).map(_ => JSON.Boolean(false))
  val `true`        = P( "true" ).map(_ => JSON.Boolean(true))

  val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
  val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )

  val strChars = P( CharsWhile(StringChars) )
  val string =
    P( space ~ "\"" ~! (strChars | escape).rep.! ~ "\"").map(JSON.String)

  val array =
    P( "[" ~! jsonExpr.rep(sep="," ~!) ~ space ~ "]").map(x => JSON.Array(List(x :_*)))

  val pair = P( string.map(_.value) ~! ":" ~! jsonExpr )

  val obj =
    P( "{" ~! pair.rep(sep="," ~!) ~ space ~ "}").map(x => JSON.Object(Map(x :_*)))

  val jsonExpr: P[JSON] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )
}

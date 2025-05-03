import org.parboiled2._
import scala.annotation.switch

/**
 * @see https://github.com/sirthias/parboiled2
 */
class JSONParboiledParser(val input: ParserInput) extends Parser {
  import CharPredicate.{Digit, Digit19, HexDigit}

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
  val QuoteBackslash = CharPredicate("\"\\")
  val QuoteSlashBackSlash = QuoteBackslash ++ "/"

  // the root rule
  val Json = rule { WhiteSpace ~ Value ~ EOI }

  val JsonObject: Rule1[JSON] = rule {
    ws('{') ~ zeroOrMore(Pair).separatedBy(ws(',')) ~ ws('}') ~> { fields: Seq[(String, JSON)] => JSON.Object(Map(fields :_*)) }
  }

  val Pair: Rule1[(String, JSON)] = rule { JsonStringUnwrapped ~ ws(':') ~ Value ~> { (x: String, y: JSON) => (x, y) } }

  val Value: Rule1[JSON] = rule {
    JsonString | JsonNumber | JsonObject | JsonArray | JsonTrue | JsonFalse | JsonNull
  }

  val JsonString = rule { JsonStringUnwrapped ~> (JSON.String(_)) }

  val JsonStringUnwrapped: Rule1[String] = rule { '"' ~ Characters ~ ws('"') ~> { x: Seq[Char] => x.mkString("") } }

  val JsonNumber = rule { capture(Integer ~ optional(Frac) ~ optional(Exp)) ~> (s => JSON.Number(s.toDouble)) ~ WhiteSpace }

  val JsonArray = rule { ws('[') ~ zeroOrMore(Value).separatedBy(ws(',')) ~ ws(']') ~> (x => JSON.Array(List(x:_*))) }

  val Characters = rule { zeroOrMore(NormalChar | '\\' ~ EscapedChar) }

  val NormalChar = rule { !QuoteBackslash ~ capture(ANY) ~> { x: String => x(0) } }

  val EscapedChar = rule (
      '"' ~ push('"')
      | '\\' ~ push('\\')
      | '/' ~ push('/')
      | 'b' ~ push('\b')
      | 'f' ~ push('\f')
      | 'n' ~ push('\n')
      | 'r' ~ push('\r')
      | 't' ~ push('\t')
      | Unicode ~> { code => code.asInstanceOf[Char] }
  )

  val Unicode = rule { 'u' ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> (java.lang.Integer.parseInt(_, 16)) }

  val Integer = rule { optional('-') ~ (Digit19 ~ Digits | Digit) }

  val Digits = rule { oneOrMore(Digit) }

  val Frac = rule { "." ~ Digits }

  val Exp = rule { ignoreCase('e') ~ optional(anyOf("+-")) ~ Digits }

  val JsonTrue = rule { "true" ~ WhiteSpace ~ push(JSON.Boolean(true)) }

  val JsonFalse = rule { "false" ~ WhiteSpace ~ push(JSON.Boolean(false)) }

  val JsonNull = rule { "null" ~ WhiteSpace ~ push(JSON.Null) }

  val WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }

  def ws(c: Char) = rule { c ~ WhiteSpace }
  def ws(s: String) = rule { s ~ WhiteSpace }
  def wsi(s: String) = rule { ignoreCase(s) ~ WhiteSpace }
}

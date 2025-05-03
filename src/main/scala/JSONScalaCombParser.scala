import scala.util.parsing.combinator._

/**
 * @see http://www.artima.com/pins1ed/combinator-parsing.html
 * @see https://github.com/scala/scala-parser-combinators
 * @see http://www.codecommit.com/blog/scala/the-magic-behind-parser-combinators
 * @see http://henkelmann.eu/2011/01/13/an_introduction_to_scala_parser_combinators
 * @see http://www.codecommit.com/blog/scala/the-magic-behind-parser-combinators
 */
object JSONScalaCombParser extends JavaTokenParsers {
  def value : Parser[JSON] = obj | array |
    (stringLiteral ^^ (x => JSON.String(x))) |
    (floatingPointNumber ^^ (x => JSON.Number(x.toDouble))) |
    ("null" ^^^ JSON.Null) |
    ("true" ^^^ JSON.Boolean(true)) |
    ("false" ^^^ JSON.Boolean(false))
  def array: Parser[JSON] = "[" ~> repsep(value, ",") <~ "]" ^^ { l => JSON.Array(List(l :_*))}
  def member: Parser[(String, JSON)] = (stringLiteral <~ ":") ~ value ^^ { case a ~ b => (a, b) }
  def obj: Parser[JSON] = "{" ~> repsep(member, ",") <~ "}" ^^ { l => JSON.Object(Map(l :_*)) }
}

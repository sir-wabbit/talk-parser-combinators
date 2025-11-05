object JSONWabbitParser {
  import wabbitparse.Parser
  import wabbitparse.Parser._

  object fragments {
    val space = oneOf(" \t\n\r")
    val digit = range('0', '9')
    val digit1 = range('1', '9')
    val hex = range('a', 'f') | range('A', 'F') | range('0', '9')
    val esc = one('\\') ~> (oneOf("\\/\"") | (oneOf("bfnrt") ^^ {
      case 'b' => '\b'
      case 'f' => '\f'
      case 'n' => '\n'
      case 'r' => '\r'
      case 't' => '\t'
    }) | one('u') ~> (hex * 4) ^^ { l =>
      Integer.parseInt(l mkString "", 16).asInstanceOf[Char]
    })
    val int = surface(string("0") | (rep1(digit1, digit) ^^ { s => s mkString "" }))
    val exp = surface(oneOf("Ee") ~ oneOf("+-").? ~ int)
  }
  object tokens {
    import fragments._

    def token[A](p: Parser[Char, A]) = p <~ space.*

    val str = token(one('"') ~> (esc | noneOf("\"\\")).* <~ one('"')) ^^ (_.mkString(""))
    val num = token(
      surface(one('-').? ~ int) |
      surface(one('-').? ~ int ~ exp) |
      surface(one('-').? ~ int ~ one('.') ~ rep1(digit) ~ exp.?)
    ) ^^ (_.mkString("").toDouble)

    val `{` = token(one('{'))
    val `}` = token(one('}'))
    val `[` = token(one('['))
    val `]` = token(one(']'))
    val `,` = token(one(','))
    val `:` = token(one(':'))
    val `true` = token(string("true"))
    val `false` = token(string("false"))
    val `null` = token(string("null"))
  }

  import tokens._

  def str: Parser[Char, JSON] = tokens.str ^^ JSON.String
  def num: Parser[Char, JSON] = tokens.num ^^ JSON.Number
  def obj: Parser[Char, JSON] = `{` ~> sep(zip(tokens.str <~ `:`, value), `,`) <~ `}` ^^ {list => JSON.Object(Map(list:_*))}
  def array: Parser[Char, JSON] = `[` ~> sep(value, `,`) <~ `]` ^^ JSON.Array
  def bool: Parser[Char, JSON] = (`true` ^^^ JSON.Boolean(true)) | (`false` ^^^ JSON.Boolean(false))
  def `null`: Parser[Char, JSON] = tokens.`null` ^^^ JSON.Null
  def value: Parser[Char, JSON] = str | num | array | obj | bool | `null`
  def json: Parser[Char, JSON] = fragments.space.* ~> value <~ eof
}

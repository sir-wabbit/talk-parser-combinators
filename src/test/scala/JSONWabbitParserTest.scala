import wabbitparse.Parser
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._

class JSONWabbitParserTest extends FunSuite with Matchers {
  import wabbitparse.Parser
  import wabbitparse.Parser._

  def parser[A](p: Parser[Char, A]): String => Option[A] = { s: String =>
    (JSONWabbitParser.fragments.space.* ~> p <~ eof).run(s.toStream).map(_._2).headOption
  }

  test("tokenTest") {
    val str = parser(JSONWabbitParser.tokens.str)
    val num = parser(JSONWabbitParser.tokens.num)

    str(""" aa """) should be (None)
    str(""" 0 """) should be (None)
    str(""" "abc" """) should be (Some("abc"))
    str(""" "a\nbc" """) should be (Some("a\nbc"))
    str(""" "abc  \"\u1222" """) should be (Some("abc  \"\u1222"))

    num(""" aa """) should be (None)
    num(""" "" """) should be (None)
    num(""" 123 """) should be (Some(123.0))
    num(""" 124124 """) should be (Some(124124.0))
    num(""" 0 """) should be (Some(0.0))
    num(""" 0.0 """) should be (Some(0.0))
    num(""" 0.1e1 """) should be (Some(0.1e1))
  }

  test("jsonTest") {
    import JSON._

    val array = parser(JSONWabbitParser.array)
    val obj = parser(JSONWabbitParser.obj)
    val json = parser(JSONWabbitParser.value)

    array(""" [0, 1, 2] """) should be (Some(JSON.Array(List(
      JSON.Number(0.0), JSON.Number(1.0), JSON.Number(2.0)
    ))))

    array(""" [{}] """) should be (Some(JSON.Array(List(
      JSON.Object(Map.empty)
    ))))

    json(""" [{}] """) should be (Some(JSON.Array(List(
      JSON.Object(Map.empty)
    ))))

    json(""" { "null" : null, "true" : true, "empty": [], "object": { "nested" : 0 } } """) should be (Some(JSON.Object(Map(
      "null" -> JSON.Null,
      "true" -> JSON.Boolean(true),
      "empty" -> JSON.Array(List.empty),
      "object" -> JSON.Object(Map("nested" -> JSON.Number(0.0)))
    ))))
  }
}

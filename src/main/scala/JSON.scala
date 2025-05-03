sealed trait JSON extends Serializable with Product

object JSON {
  type SBool = scala.Boolean
  type SString = java.lang.String

  final case class Object(map: Map[SString, JSON]) extends JSON
  final case class Array(list: List[JSON]) extends JSON
  final case class Number(value: Double) extends JSON
  final case class String(value: SString) extends JSON
  final case class Boolean(value: SBool) extends JSON
  case object Null extends JSON
}
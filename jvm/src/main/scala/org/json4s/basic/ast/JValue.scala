package org.json4s.basic.ast

sealed abstract class JValue extends Serializable with Product

case object JNull extends JValue

case class JString(value:String) extends JValue

object JNumber {
  def apply(value: Int): JNumber = JNumber(value.toString)
  def apply(value: Byte): JNumber = JNumber(value.toString)
  def apply(value: Short): JNumber = JNumber(value.toString)
  def apply(value: Long): JNumber = JNumber(value.toString)
  def apply(value: BigInt): JNumber = JNumber(value.toString)
  def apply(value: BigDecimal): JNumber = JNumber(value.toString)
  def apply(value: Float): JNumber = JNumber(value.toString)
  def apply(value: Double): JNumber = JNumber(value.toString)
}

/**
 * JNumber is internally represented as a string, to improve performance
 * @param value
 */
case class JNumber(value: String) extends JValue {
  def to[B](implicit jNumberConverter: JNumberConverter[B]) = jNumberConverter(value)
}

final class JBoolean private(val isTrue: Boolean) extends JValue {
  private val someGet = Some(isTrue)

  override def productElement(n: Int): Any = if (n == 0) isTrue else throw new IndexOutOfBoundsException

  override def productArity: Int = 1

  override def canEqual(that: Any): Boolean = that.isInstanceOf[JBoolean]
}

object JBoolean {
  val True = new JBoolean(true)
  val False = new JBoolean(false)

  def apply(x: Boolean): JBoolean = if (x) True else False

  def unapply(x: JValue): Option[Boolean] = x match {
    case x: JBoolean => x.someGet
    case _           => None
  }
}


case class JField(field:String, value:JValue)

/**
 * JObject is internally represented as a mutable Array, to improve sequential performance
 * @param value
 */
case class JObject(value: Array[JField] = Array.empty) extends JValue

/**
 * JArray is internally represented as a mutable Array, to improve sequential performance
 * @param value
 */
case class JArray(value: Array[JValue] = Array.empty) extends JValue
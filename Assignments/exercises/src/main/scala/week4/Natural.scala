package week4

abstract class Natural {
  def isZero: Boolean
  def predecessor: Natural
  def successor: Natural = new Succ(this)
  def +(that: Natural): Natural
  def -(that: Natural): Natural
}

object Zero extends Natural {
  override def isZero: Boolean = true

  override def predecessor: Natural = throw new Error("There is no predecessor of Zero")

  override def +(that: Natural): Natural = that

  override def -(that: Natural): Natural =
    if (that.isZero) this
    else throw new Error("Negative result (can't be a Natural Number)")
}

class Succ(num: Natural) extends Natural {
  override def isZero: Boolean = false

  override def predecessor: Natural = num

  override def +(that: Natural): Natural = new Succ(num + that)

  override def -(that: Natural): Natural =
    if (that.isZero) this
    else num - predecessor
}
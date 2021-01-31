abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(that: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def toString: String =
    s"{$left $elem $right}"

  override def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  override def union(that: IntSet): IntSet =
    that union left union right incl elem
}

object Empty extends IntSet {
  override def toString: String =
    "."

  override def incl(x: Int): IntSet =
    new NonEmpty(x, Empty, Empty)

  override def contains(x: Int): Boolean =
    false

  override def union(that: IntSet): IntSet =
    that
}

val node = new NonEmpty(1, Empty, Empty)
node contains 1
node contains 2
node union new NonEmpty(2, Empty, Empty)
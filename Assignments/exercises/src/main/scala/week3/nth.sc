import week3._
import scala.annotation.tailrec

@tailrec
def nth[T](n: Int, xs: List[T]): T =
  if (xs.isEmpty || n < 0) throw new IndexOutOfBoundsException("N is larger than the list")
  else if (n == 0) xs.head
  else nth(n - 1, xs.tail)

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
nth(1, list)
nth(100, list)



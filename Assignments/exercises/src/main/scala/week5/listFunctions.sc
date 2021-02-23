def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y*y :: squareList(ys)
}

def squareList(xs: List[Int]): List[Int] =
  xs map (x => x*x)


// pack list
def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}

val res = pack(List("a", "a", "a", "b", "c", "c", "a"))
assert(res == List(List("a", "a", "a"), List("b"), List("c", "c"), List("a")))


def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs).map(ys => (ys.head, ys.length))

val res2 = encode(List("a", "a", "a", "b", "c", "c", "a"))
assert(res2 == List(("a", 3), ("b", 1), ("c", 2), ("a", 1)))


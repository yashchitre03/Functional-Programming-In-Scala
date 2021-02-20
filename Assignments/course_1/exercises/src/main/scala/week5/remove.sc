def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)
removeAt(1, List('a', 'b', 'c', 'd'))

def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => Nil
  case y :: ys => y match {
    case z: List[Any] => flatten(z) ::: flatten(ys)
    case z => z :: flatten(ys)
  }
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))

def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
  case (Nil, ys) => ys
  case (xs, Nil) => xs
  case (Nil, Nil)  => Nil
  case (x :: xss, y :: yss) =>
    if (x < y) x :: merge(xss, ys)
    else y :: merge(xs, yss)
}
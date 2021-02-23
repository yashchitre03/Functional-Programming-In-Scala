def isort(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case Nil => x :: Nil
  case y :: ys => {
    if (x <= y) x :: xs
    else y :: insert(x, ys)
  }
}
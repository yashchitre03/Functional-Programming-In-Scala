package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def recurse(chars: List[Char], paren:Int): Boolean = {
      if (paren < 0) false
      else if (chars.isEmpty) paren == 0
      else if (chars.head == '(') recurse(chars.tail, paren + 1)
      else if (chars.head == ')') recurse(chars.tail, paren - 1)
      else recurse(chars.tail, paren)
    }
    recurse(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def recurse(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else recurse(money, coins.tail) + recurse(money - coins.head, coins)
    }
    if (money == 0) 0 else recurse(money, coins)
  }
}

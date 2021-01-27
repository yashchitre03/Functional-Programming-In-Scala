/*
Lecture 2.2
 */

// product
def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a+1, b)

val cubes: Int => Int = x => x * x
product(cubes)(3, 4)


// factorial
def fact(n: Int): Int = product(x => x)(1, n)
fact(4)


// general function for both sum and product
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, base_case: Int)(a: Int, b: Int): Int =
  if (a > b) base_case
  else combine(f(a), mapReduce(f, combine, base_case)(a+1, b))

def gen_product(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x * y, 1)(a, b)

def gen_sum(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x + y, 0)(a, b)

gen_product(x => x)(1, 4)
gen_sum(x => x)(1, 4)


// partially applied function
val temp = mapReduce(x => x, (x, y) => x + y, 0) _
temp(1, 4)


// different way to curry
def mapReduce(f: Int => Int)(combine: (Int, Int) => Int, base_case: Int)(a: Int, b: Int): Int =
  if (a > b) base_case
  else combine(f(a), mapReduce(f)(combine, base_case)(a+1, b))

def gen_sum(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f)((x, y) => x + y, 0)(a, b)

gen_sum(x => x)(1, 4)

/*
Lecture 2.5
 */

import scala.annotation.tailrec

class Rational(x: Int, y: Int) {
  require(y > 0, "Denominator must be positive")

  @tailrec
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a.abs
    else gcd(b, a % b)

  val numer: Int = x / gcd(x, y)
  val denom: Int = y / gcd(x, y)

  def + (that: Rational): Rational =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)

  override def toString: String = s"$numer/$denom"

  def unary_- : Rational =
    new Rational(-numer, denom)

  def - (that: Rational): Rational =
    this + -that

  def < (that: Rational): Boolean =
    numer * that.denom < that.numer * denom

  def max(that: Rational): Rational =
    if (this < that) that
    else this
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.numer
x.denom

//x.add(y)

x.toString

//x.neg

//x.sub(y)
//
//x.sub(y).sub(z)
//y.add(y)

/*
Lecture 2.6
 */

//x.less(y)
x.max(y)

//new Rational(1, 0)

x + y + z
x - y - z
package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

  override def toString: String = this match {
    case Nil => "List()"
    case Cons(h, t) => s"List(${toStringElements})"
  }

  private def toStringElements: String = this match {
    case Nil => ""
    case Cons(h, Nil) => s"$h"
    case Cons(h, t) => s"$h, ${t.toStringElements}"
  }

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("An empty list does not have a tail")
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => sys.error("An empty list cannot get its head set")
    case Cons(_, tail) => Cons(h, tail)
  
  def take[A](l: List[A], elementsToTake: Int): List[A] = l match {
    case Cons(head, tail) if (elementsToTake > 0) => Cons(head, take(tail, elementsToTake - 1))
    case list if (elementsToTake < 1) => Nil
    case Nil => Nil
    case Cons(_, _) => throw new Exception("This case should never be hit")
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case empty @ Nil => empty
    case list @ Cons(head, tail) if (n < 1) => list
    case Cons(head, tail) => drop(tail, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) if (f(head)) => dropWhile(tail, f)
    case list => list
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil =>  sys.error("An empty list cannot have its last element removed")
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0, (_, size) => size + 1)

  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = l match {
    case Nil => acc
    case Cons(head, tail) => foldLeft(tail, f(acc, head), f)
  }

  def foldRightViaFoldLeft[A,B](as: List[A], acc: B, f: (A, B) => B): B = foldLeft(as, identity[B], (acc, e) => b => acc(f(e, b)))(acc)

  def foldLeftViaFoldRight[A,B](l: List[A], acc: B, f: (B, A) => B): B = foldRight(l, identity[B], (e, acc) => b => acc(f(b, e)))(acc)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (size, _) => size + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A], (acc, e) => Cons(e,acc))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r, Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] = foldRightViaFoldLeft(l, Nil: List[A], append)

  def incrementEach(l: List[Int]): List[Int] = foldRightViaFoldLeft(l, Nil: List[Int], (e, acc) => Cons(e + 1, acc))

  def doubleToString(l: List[Double]): List[String] = foldRightViaFoldLeft(l, Nil: List[String], (e, acc) => Cons(e.toString(), acc))

  def map[A,B](l: List[A], f: A => B): List[B] = foldRightViaFoldLeft(l, Nil: List[B], (e, acc) => Cons(f(e), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] = foldRightViaFoldLeft(as, Nil: List[A], (e, acc) => if (f(e)) Cons(e, acc) else acc)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = foldRightViaFoldLeft(as, Nil: List[B], (e, acc) => append(f(e), acc))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = flatMap(as, e => if (f(e)) List(e) else List())

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = {
    (a,b) match
      case (Nil, Nil) => Nil
      case (Nil, bList) => Nil
      case (aList, Nil) => Nil
      case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(head1 + head2, addPairwise(tail1, tail2))
  }

  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] = {
    (a,b) match
      case (Nil, Nil) => Nil
      case (Nil, bList) => Nil
      case (aList, Nil) => Nil
      case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(f(head1,head2), zipWith(tail1, tail2, f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val subSize = length(sub)
    take(sup, subSize)
    ???
  }

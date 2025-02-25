package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

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


  // List(1, 2 ,3)
  // Right to left
  // fold right -> ((3 * 2) * 1)

  // f(1 * f(2 * 3))
  // fold left -> 1 * 2 * 3

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double = {
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => sys.error("empty list has no tail")
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] =  l match
    case Nil => sys.error("empty list cannot replace an element")
    // case Cons(head, tail) => Cons(h, Cons(head, tail))
    case Cons(_, tail) => Cons(h, tail)
  
  @tailrec
  def droppings[A](l: List[A], n: Int): List[A] = n match
    case n if n <= 0 => l
    case _ => l match
      case Nil => Nil
      case Cons(_, tail) => droppings(tail, n - 1)
  
  @tailrec
  def drop[A](xs: List[A], n: Int): List[A] = xs match {
      case Nil => Nil
      case _ if n <= 0 => xs
      case Cons(_, tail) => drop(tail, n - 1)
  }

  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
      case Cons(h, tail) if f(h) => dropWhile(tail, f)
      case _ => xs
  }

  def init[A](xs: List[A]): List[A] = {

    if (xs == Nil) sys.error("An empty list cannot have its last element removed")
    else {
      def initRec(xs2: List[A]): List[A] = {
        xs2 match {
          case Cons(h, t) if (t != Nil) => Cons(h, initRec(t))
          case Cons(_, Nil) => Nil
        }
      }

      initRec(xs) 
    }
  }

  def init2[A](xs: List[A]): List[A] = {
    xs match
      case Nil => sys.error("An empty list cannot have its last element removed")
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init2(tail))
  }


  def length[A](xs: List[A]): Int = ???

  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = ???

  def sumViaFoldLeft(ns: List[Int]): Int = ???

  def productViaFoldLeft(ns: List[Double]): Double = ???

  def lengthViaFoldLeft[A](l: List[A]): Int = ???

  def reverse[A](l: List[A]): List[A] = ???

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = ???

  def concat[A](l: List[List[A]]): List[A] = ???

  def incrementEach(l: List[Int]): List[Int] = ???

  def doubleToString(l: List[Double]): List[String] = ???

  def map[A,B](l: List[A], f: A => B): List[B] = ???

  def filter[A](as: List[A], f: A => Boolean): List[A] = ???

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = ???

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = ???

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = ???

  // def zipWith - TODO determine signature

  // sup = List(1, 2 ,3, 4) valids: List(1, 2, 3, 4), List(), List(1,2), List9
  // sub List(1, 2, 3)  1/2/3/4/1,2/2,3/3,4/1,2,3/2,3,4/1,2,3,4
  // 2,3

  // drop(1)
  // list = List(1,2,3,4)
  // list == sub if not, then drop (1)
  // sub = List(1, 2, 3)

  // sup: 1,2,3,1,7 sub: 1,7
  @annotation.tailrec
  def hasSubsequence[A](list: List[A], sub: List[A]): Boolean = {
    (list, sub) match 
       case (Cons(lh, _), Cons(sh, Nil)) if lh == sh =>  true
       case (Cons(lh, lt), Cons(sh, st)) if lh != sh =>  hasSubsequence(lt, sub)
       case (Cons(lh, lt), Cons(sh, st)) if lh == sh =>  hasSubsequence(lt, st)
       case (_, Nil) => true
       case _ => false
  }

  def hasSubsequenceCWR[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub match {
      case Nil => true
      case _ => false
    }
    case Cons(supH, Nil) => sub match {
      case Nil => false
      case Cons(subH, Nil) => supH == subH
      case Cons(subH, subT) => false
    }
    case Cons(supH, supT) => sub match {
      case Nil => false
      case Cons(subH, Nil) => supH == subH
      case Cons(subH, subT) => {
        if (supH != subH) then {
          false
        } else {
          hasSubsequence(supT, subT)
        }
      }
    }
  }
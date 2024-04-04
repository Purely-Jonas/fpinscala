package fpinscala.exercises.laziness

import scala.annotation.tailrec
import fpinscala.exercises.laziness.LazyList.empty

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  // Inefficient, can make a better implementation
  //@tailrec
  def toList: List[A] = this match
    case Empty => List.empty
    case Cons(h, t) => h() :: t().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if (n > 0) => LazyList.cons(h(), t().take(n - 1))
    case _ => Empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if (n > 0) => t().drop(n - 1)
    case left => left

  def takeWhileBasic(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if(p(h())) => LazyList.cons(h(), t().takeWhile(p))
    case _ => Empty

  def takeWhile(p: A => Boolean): LazyList[A] = this.foldRight(LazyList.empty)((element, acc) => if (p(element)) LazyList.cons(element, acc) else acc)

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((element, acc) => p(element) && acc)

  def headOption: Option[A] = this.foldRight(None: Option[A])((element, _) => Some(element))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def append[B >: A](other: => LazyList[B]): LazyList[B] = {
    this.foldRight(other)((element, acc) => {
      LazyList.cons(element, acc)
    })
  }

  def map[B](f: A => B): LazyList[B] = this.foldRight(LazyList.empty)((element, acc) => {
    LazyList.cons(f(element), acc)
  })

  def flatMap[B](f: A => LazyList[B]): LazyList[B] = this.foldRight(LazyList.empty)((element, acc) => {
    f(element).append(acc)
  })

  def filter(f: A => Boolean): LazyList[A] = {
    this.foldRight(LazyList.empty){(element, acc) => 
      if (f(element)) LazyList.cons(element, acc) else acc
    }
  }

  // 5.13 map, take, takeWhile, zipWith and zipAll using unfold

  def mapViaUnfold[B](f: A => B): LazyList[B] = LazyList.unfold(this){
    case Empty => None
    case Cons(h, t) => Some(f(h()) -> t())
  }

  def takeViaUnfold(initalN: Int): LazyList[A] = LazyList.unfold(initalN -> this){
    case (n, Cons(h, t)) if (n > 0) => 
      lazy val nextState = n-1 -> t()
      Some(h() -> nextState)
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] = LazyList.unfold(this){
    case Cons(h, t) if (p(h())) => Some(h() -> t())
    case _ => None
  }

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] = LazyList.unfold(this -> that){
    case (Cons(h1, t1), Cons(h2, t2)) =>
      lazy val nextState = t1() -> t2()
      Some(f(h1(), h2()) -> nextState)
    case _ => None
  }

  private def toNextStep[A, B](h1: Option[A], h2: Option[B], t1: LazyList[A], t2: LazyList[B]) = {
    lazy val nextState = t1 -> t2
    lazy val e = h1 -> h2
    Some(e -> nextState)
  }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = LazyList.unfold(this -> that){
    case (Cons(h1, t1), Cons(h2, t2)) => toNextStep(Some(h1()), Some(h2()), t1(), t2())
    case (Cons(h1, t1), Empty) => toNextStep(Some(h1()), None, t1(), Empty)
    case (Empty, Cons(h2, t2)) => toNextStep(None, Some(h2()), Empty, t2())
    case _ => None
  }
  
  def startsWith[B](s: LazyList[B]): Boolean = zipAll(s).takeWhile(_._2.isDefined).forAll(_ == _)

  def tails = LazyList.unfold(this){
    case Empty => None
    case list @ Cons(_, t) => Some(list -> t())
  }.append(LazyList(empty))

  def hasSubsequence[B](s: LazyList[B]): Boolean = tails.exists(_.startsWith(s))

  def scanRight[B](z: => B)(f: (A, => B) => B): LazyList[B] = {
    this.foldRight(z -> LazyList(z)){
    case (e, (accE, accL)) => 
      val computation = f(e,accE)
      computation -> LazyList.cons(computation, accL)
  }._2
}

object LazyList:    
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] = cons(0, doFibs(0, 1))

  private def doFibs(e1: Int, e2: Int): LazyList[Int] = cons(e2, doFibs(e2, e1 + e2))

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = {
    f(state)
      .map((a, s) => cons(a, unfold(s)(f)))
      .getOrElse(empty)
  }

  lazy val fibsViaUnfold: LazyList[Int] = unfold(0 -> 1)((e1, e2) => {
    val nextState = (e2, e1 + e2)
    Some(e1 -> nextState)
  })

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(no => Some(no, no + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(a)(aVal => Some(aVal -> aVal))

  lazy val onesViaUnfold: LazyList[Int] = unfold(1)(one => Some(one -> one))

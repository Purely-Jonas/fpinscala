package fpinscala.exercises.laziness

import scala.annotation.tailrec

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

  def map[B](f: A => B): LazyList[B] = this.foldRight(LazyList.empty)((element, acc) => {
    LazyList.cons(f(element), acc)
  })

  def filter(f: A => Boolean): LazyList[A] = {
    this.foldRight(LazyList.empty){ (element, acc) => 
      if (f(element)) LazyList.cons(element, acc) else acc
    }
  }

  // def filterWithout(f: A => Boolean): LazyList[A] = this match
  //   case Empty => LazyList.Empty
  //   case Cons(h, t) => if (f(h())) filterWithout(f)
  

  /*
    LazyList(1,2,3).map(_ + 10).filter(_ % 2 == 0) => 
      // This will happen only when it gets executed conceptually
      LazyList(m(1), m(2), m(3)) => LazyList(f(m(1)), f(m(2)), f(m(3))) 
    

    Cons(() => 1 + 10, () => LazyList(2,3).map(_ + 10))
    Cons(() => () => 1 + 10 % 2 == 0, () => LazyList(2,3).map(_ + 10)).filter(_ % 2 == 0))
  
    fist value + the rest of the list

    LazyList.cons(f(element), acc))
  */
  
  def startsWith[B](s: LazyList[B]): Boolean = ???


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

  def continually[A](a: A): LazyList[A] = ???

  def from(n: Int): LazyList[Int] = ???

  lazy val fibs: LazyList[Int] = ???

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  lazy val fibsViaUnfold: LazyList[Int] = ???

  def fromViaUnfold(n: Int): LazyList[Int] = ???

  def continuallyViaUnfold[A](a: A): LazyList[A] = ???

  lazy val onesViaUnfold: LazyList[Int] = ???

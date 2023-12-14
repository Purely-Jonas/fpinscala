import fpinscala.exercises.datastructures.List
import fpinscala.exercises.datastructures.List.*
import scala.{List as SList, Nil as SNil}

val list = List(1,2,3)

val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

List.drop(list, 2)

List.drop(list, 4)

List.dropWhile(list, _ < 3)

List.init(list)

def oldFoldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
as match
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc, f))

def newFoldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = {
    foldLeft(as, identity[B], (acc, e) => b => acc(f(e, b)))(acc)
}

def newFoldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = {
    foldRight(l, identity[B], (e, acc) => b => acc(f(b, e)))(acc)
}

newFoldRight(list, Nil: List[Int], Cons(_, _))

//foldRight, builds up the function
foldRight(list, Nil: List[Int], Cons(_, _))
Cons(1, Cons(2, Cons(3, Nil)))
// foldLeft, evaluates immediatly
foldLeft(list, Nil: List[Int], (acc, e) => Cons(e, acc))
Cons(3, Cons(2, Cons(1, Nil)))


newFoldLeft(list, Nil: List[Int], (acc, e) => Cons(e, acc))
foldLeft(list, Nil: List[Int], (acc, e) => Cons(e, acc))
// (acc: (A, B) => B, e: A)
// b: B => acc(a, b)
// B => B

/**
* The main problem is that our functions are applied in reverse order. For example:
    foldRight: f(a, f(b, f(c, acc)))
        Cons(a, Cons(b, Cons(c, Nil)))
    foldLeft: f(f(f(Nil, a), b), c)
        Cons(3, Cons(2, Cons(1, Nil)))
*/

val list2 = List(4, 5, 6, 7)

appendViaFoldRight(list, list2)

def listToScalaList[A](list: List[A]): SList[A] = list match
    case Nil         => SList.empty[A]
    case Cons(x, xs) => x +: listToScalaList(xs)
    
val (testList1, testList2) = (List(-466925377, -1318598204, -377250524, -871624231, 524269034, 1434445526, -254551190, 645113710, -1529359428, 168174980, 819135473, 1023719373, -53591719, 847179518),
List(-1284786128, -1215422025, 1830302292, 506148103, -1901071630, 1745584936, 49051267, 261309221, 1894388528))

def testy() = {
    val sTestList1 = listToScalaList(testList1)
    val sTestList2 = listToScalaList(testList2)
    sTestList1.zip(sTestList2).map{ case (a, b) => a + b }
}


val res = testy()

val ownRes = addPairwise(testList1, testList2)

res.size

listToScalaList(ownRes).size

listToScalaList(testList1).size

val listy = take(testList1, -1)

listToScalaList(listy).size

SList(1, 2,3).take(-1)

import fpinscala.exercises.datastructures.List
import fpinscala.exercises.datastructures.List.*

val list = List(1,2,3,4)

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

foldRight(list, Nil: List[Int], Cons(_, _))

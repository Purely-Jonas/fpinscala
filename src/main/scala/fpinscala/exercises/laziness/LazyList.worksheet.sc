import fpinscala.exercises.laziness.LazyList
import scala.collection.immutable.{LazyList as SLazyList}

def valueWithSideEffect[A](a: A) = {
    println(a)
    a
}


val ls = LazyList(1, 2, 3, 4).map(valueWithSideEffect)

ls.toList

val lsFilter = LazyList(1, 2, 3, 4).map(valueWithSideEffect).filter(_ > 2)
lsFilter.toList

val sLs = SLazyList(1,2,3,4).map(valueWithSideEffect)
sLs.toList

val slsFilter = SLazyList(1, 2, 3, 4).map(valueWithSideEffect).filter(_ > 2)
slsFilter.toList
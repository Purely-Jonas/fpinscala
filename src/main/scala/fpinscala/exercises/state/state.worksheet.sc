import fpinscala.exercises.state.RNG
import fpinscala.exercises.state.State
import fpinscala.exercises.state.State.sequence
import fpinscala.exercises.state.Machine
import fpinscala.exercises.state.Input.*
import fpinscala.exercises.state.Candy.*

RNG.nonNegativeInt(RNG.Simple(1))

Math.abs(Int.MinValue) - 1

Math.abs(Int.MinValue + 1)

Math.abs(Int.MinValue + 2)

Math.abs(Int.MinValue)

Int.MinValue + 1

Int.MinValue * -1

Int.MinValue / 1

~Int.MinValue

~Int.MaxValue + 1

(Int.MaxValue - 1).toDouble / Int.MaxValue.toDouble

(Int.MaxValue - 2).toDouble / Int.MaxValue.toDouble

0.toDouble / Int.MaxValue.toDouble

Int.MinValue.toDouble / Int.MaxValue.toDouble

val stateA: State[List[String], Option[String]] =
    State:
      case Nil          => (None, Nil)
      case head :: tail => (Some(head), tail)

val list = List(1,2,3,4).map(_.toString())
val half = list.length / 2
val listOfStates = (0 until half).map(_ => stateA).toList
val result = sequence(listOfStates)
result.run(list)

sequence(list.map(_ => stateA).toList).run(list)

val machine = Machine(true,1,0)

val newMachine = simulateMachine(List(Coin)).run(machine)

newMachine._1
newMachine._2.candies
newMachine._2.coins

//import fpinscala.state._

object State {

  type State[S, +A] = S => (A, S)

  def flatMap[S, A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] =
    s => {
      val (a, s1) = f(s)
      g(a)(s1)
    }

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] =
    flatMap(s)(a => (ss => (f(a), ss)))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = s => fs match {
    case Nil => (List[A](), s)
    case h :: t => {
      val (a, s1) = h(s)
      val (l, s2) = sequence(t)(s1)
      (a :: l, s2)
    }
  }

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def insertCoin = if (locked) Machine(false, candies, coins + 1) else this

    def takeCandy = if (!locked && (candies > 0)) Machine(true, candies - 1, coins) else this
  }

  type MachineState = State[Machine, (Int, Int)]

  sealed trait Input {
    def action: MachineState
  }

  case object Coin extends Input {
    def action: MachineState = m => {
      val m2 = m.insertCoin;
      ((m2.coins, m2.candies), m2)
    }
  }

  case object Turn extends Input {
    def action: MachineState = m => {
      val m2 = m.takeCandy;
      ((m2.coins, m2.candies), m2)
    }
  }

  def simulateMachine(inputs: List[Input]): MachineState =
    map(sequence(inputs.map(_.action)))(_.reverse.head)
}
import State._

val actions = List.fill(4)(List(Coin, Turn)).flatten
simulateMachine(actions)(Machine(true, 5, 10))
Coin.action(Machine(true, 5, 10))

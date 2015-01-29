package fpinscala.iomonad

object Main {
  case class Player(name: String, score: Int)

  def ___contest(p1: Player, p2: Player): Unit =
    if (p1.score > p2.score)
      println(s"${p1.name} is the winner!")
    else if (p2.score > p1.score)
      println(s"${p2.name} is the winner!")
    else
      println("It's a draw.")

  /**
   * Contains the logic for computing the winner, or the fact that there is a draw
   */
  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None

  /**
   * Has the responsibility of declaring the winner on the console
   */
  def __contest(p1: Player, p2: Player): Unit = winner(p1, p2) match {
    case Some(Player(name, _)) => println(s"$name is the winner!")
    case None => println("It's a draw.")
  }

  /**
   * Has the responsibility of determining which message is appropriate
   */
  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's a draw."

  /**
   * Has the responsibility of printing the message to the console
   */
  def _contest(p1: Player, p2: Player): Unit =
    println(winnerMsg(winner(p1, p2)))

//  import IO3._
//  def contest(p1: Player, p2: Player): Console[Unit] = new PrintLine(winnerMsg(winner(p1, p2)))

  def main(args: Array[String]) {
    if (true)
    {
      import IO2a._
      val p = IO.forever(printLine("Still going..."))
      run(p)
    }
    else if (true)
    {
      import IO1._
      val p = IO.forever(PrintLine("Still going..."))
      p.run

      converter.run
      factorialREPL.run
    }
    else if (true)
    {
      import IO0._

      def PrintLine(msg: String): IO =
        new IO { def run = println(msg) }

      def contest(p1: Player, p2: Player): IO = PrintLine(winnerMsg(winner(p1, p2)))
      contest(Player("Hello", 1), Player("world!", 2)).run

      converter
    }
  }
}

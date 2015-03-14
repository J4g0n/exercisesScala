package chapitre6


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def lock = copy(locked = false)
  def unlock = copy(locked = true)
  def dispenseCandy = copy(candies = this.candies - 1)
  def addCoin = copy(coins = this.coins + 1)
  def isEmpty = candies == 0
  def init = Machine(locked = false, candies = 10, coins = 2)
}

object StateMachine {
  val m = Machine(false, 5, 2)
  val inputList = List(Coin, Turn, Coin, Turn, Turn, Coin, Turn)

  def simulateMachine(inputs: List[Input])  =
    for {
      _ <- State.sequence ( inputs.map (i => State.modify((s: Machine) =>
        (i,s) match {
          case (Coin, s) if s.locked && !s.isEmpty => s.addCoin.unlock
          case (Turn, s) if !s.locked && !s.isEmpty => s.dispenseCandy.lock
          case _ => s
        })))
      s <- State.get
    } yield (s.coins, s.candies)

  def runMachine = simulateMachine(inputList).run(m)
}
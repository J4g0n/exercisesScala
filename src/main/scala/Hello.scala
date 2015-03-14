import chapitre4.Stream2
import chapitre6.{Machine, StateMachine, RNG}

object Hello {
  def main(args: Array[String]): Unit = {
    //println(RNG.positiveInt(RNG.simple(System.currentTimeMillis())))
    println(StateMachine.runMachine)
  }
}

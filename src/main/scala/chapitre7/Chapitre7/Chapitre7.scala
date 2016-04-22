package chapitre7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = ???
}
private case class UnitFuture[A] (get : A) extends Future[A] {
  def get(timeout: Long, unit: TimeUnit): A = get
  def isDone: Boolean = true
  def cancel(evenIfRunning: Boolean): Boolean = false
  def isCancelled: Boolean = false
}

/**
  * Created by ipi on 22/04/2016.
  */
object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A] (s: ExecutorService) (a : Par[A]) : Future[A] = a(s)

  def unit[A](a : A) : Par[A] =  es => UnitFuture(a)

  def fork[A](parA : => Par[A]) : Par[A] = es => {
    es.submit (new Callable[A] {
      override def call(): A = parA(es).get
    })
  }

  def async[A](a : => A) : Par[A] = fork(unit(a))

  def product[A,B](fa: Par[A], fb: Par[B]): Par[(A,B)] = es => unit((fa(es).get, fb(es).get))(es)

  def map[A,B](fa: Par[A])(f: A => B): Par[B] = es => unit(f(fa(es).get))(es)
  // map2(fa, unit(()))((a,_) => f(a))

  def map2[A, B, C](t1 : Par[A], t2 : Par[B])(func : (A, B) => C) : Par[C] = es => {
    val a = t1(es)
    val b = t2(es)
    unit(func(a.get, b.get))(es)
  }

  def map2Bis[A, B, C](t1 : Par[A], t2 : Par[B])(func : (A, B) => C) : Par[C] =
    map (product(t1, t2)) (x => func(x._1, x._2))

  def asyncF[A,B](f: A => B): A => Par[B] = a => unit(f(a))

  def sortPar(l: Par[List[Int]]): Par[List[Int]] = map(l)(_.sorted)
}

object ParImpl {
  import Par._

  /*def sum(as: IndexedSeq[Int]): Int =
    if (as.size <= 1) as.headOption getOrElse 0
    else {
      val (l,r) = as.splitAt(as.length/2)

      // Version 1
      /*val left: Par[Int] = unit(sum(l))
      val right: Par[Int] = unit(sum(r))
      run(left) + run(right)*/

      // Version 2
      //run(Par.unit(sum(l))) + run(Par.unit(sum(r)))
      ???
    }*/

  def sumWithMap(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) unit(as.headOption getOrElse 0)
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2[Int, Int, Int] (fork(sumWithMap(l)), fork(sumWithMap(r))) (_ + _)
    }
}

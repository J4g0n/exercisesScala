import chapitre4.Stream2

def fun(x: Int) = 3*x
def fun2(x: Int)= x+3

val x = 1000/168

def filterM [A](ax: List[A])(p: A => List[Boolean]): List[List[A]] = {
  if (ax.isEmpty) List(Nil)
  else {
    p(ax.head).flatMap { b =>
      val y = filterM(ax.tail)(p)
      y.map { ys =>
        if (b) ax.head :: ys
        else ys
      }
    }
  }
}

filterM (List(1,2,3)) (x => List(true, false))


Stream2(1,2,3).take(2).toListRecursive


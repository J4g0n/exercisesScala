package chapitre4

import java.util.regex._


object ExercicesOptions {
  def pattern(s: String): Option[Pattern] = {
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    /*for {
    aVal <- a
    bVal <- b
    } yield f(aVal,bVal)*/
    a.flatMap ( aVal =>
      b.map( bVal => f(aVal, bVal))
    )
  }

  def mkMatcher(pat: String): Option[String => Boolean] = {
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)
  }

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] = {
    map2 (mkMatcher(pat), mkMatcher(pat2)) ((f1, f2) => f1(s) && f2(s))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))
  }

  def sequence[A](lo: List[Option[A]]): Option[List[A]] = {
    /*lo.foldRight (Option(List[A]())) {
    (opt, ol) => map2 (opt, ol) (_ :: _)
    // <=> ol flatMap (l => opt map (o => o::l))
    // <=> for {l <- ol; o <- opt} yield (o::l)
    }*/
    traverse(lo)(identity)
  }

  val listOption = List(Some(3), Some(4), Some(5))
  sequence (listOption)
}

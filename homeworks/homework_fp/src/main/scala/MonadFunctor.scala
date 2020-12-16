
trait Monad[F[_]] {

  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    flatMap(fa)(a => flatMap(fb)(b => pure(f(a, b))))
  }

  def sequence[A](fas: List[F[A]]): F[List[A]] = {
    ///map2(fas.head, fas(1))((a,b) => List(a, b))
    if (fas.length == 1) flatMap(fas.head)(a => pure(List(a)))
    else {
      var res = map2(fas.head, fas(1))((a,b) => List(a, b))
      if (fas.length > 2) {
        for (i <- 2 until fas.length) res = map2(res, fas(i))((a, b) => a ::: List(b))
      }
      res
    }
  }

  def compose[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C] = ???
}

trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]
}

object Functor {
  def functorFromMonad[F[_]](M: Monad[F]): Functor[F] = new Functor[F] {
    def map[A, B](a: F[A])(f: A => B): F[B] = ???
  }
}

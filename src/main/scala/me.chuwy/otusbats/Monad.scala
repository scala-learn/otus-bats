package me.chuwy.otusbats

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = flatten(self.map(fa)(f))

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = self.flatMap(fa)(x => x)
}

object Monad {
  val listMonad = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = flatten(map(fa)(f))

    override def point[A](a: A): List[A] = List(a)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = flatMap(fa)(x => List(f(x)))

  }
}

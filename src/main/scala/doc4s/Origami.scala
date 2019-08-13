package doc4s

object Origami {

  case class Fix[F[_, _], A](out: F[A, Fix[F, A]])

  trait BiFunctor[F[_, _]] {
    def bimap[A, B, C, D]: (A => B) => (C => D) => F[A, C] => F[B, D]
    def fmap2[A, B, C]: (B => C) => F[A, B] => F[A, C] = bimap(identity[A])
  }

  def map[A, B, F[_, _]](f: A => B)(t: Fix[F, A])(implicit ft: BiFunctor[F]): Fix[F, B] =
    Fix[F, B](ft.bimap(f)(map[A, B, F](f))(t.out))

  def fold[A, B, F[_, _]](f: F[A, B] => B)(t: Fix[F, A])(implicit ft: BiFunctor[F]): B = {
    f(ft.fmap2(fold[A, B, F](f))(t.out))
  }

  def unfold[A, B, F[_, _]](f: B => F[A, B])(x: B)(implicit ft: BiFunctor[F]): Fix[F, A] =
    Fix[F, A](ft.fmap2(unfold[A, B, F](f))(f(x)))

  def hylo[A, B, C, F[_, _]](f: A => F[C, A])(g: F[C, B] => B)(x: A)(implicit ft: BiFunctor[F]): B =
    g(ft.fmap2(hylo[A, B, C, F](f)(g))(f(x)))

  def build[A, F[_, _]](f: { def apply[B]: (F[A, B] => B) => B }) =
    f.apply(Fix[F, A])
}
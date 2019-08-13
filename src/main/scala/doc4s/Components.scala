package doc4s

import doc4s.Origami._

object Components {
  trait DocF[A, B]
  case class Para[A, B](body: A) extends DocF[A, B]
  case class Sec[A, B](title: String, contents: B) extends DocF[A, B]

  implicit object biDoc extends BiFunctor[DocF] {
    def bimap[A, B, C, D] = f => g => {
      case Para(body) => Para(f(body))
      case Sec(title, contents) => Sec(title, g(contents))
    }
  }

  type Doc[A] = Fix[DocF, A]
  def para[A] = (b: A) => Fix[DocF, A](Para(b))
  def sec[A, B] = (title: String, contents: Doc[A]) => Fix[DocF, A](Sec(title, contents))
}

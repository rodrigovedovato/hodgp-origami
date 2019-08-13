package doc4s

import doc4s.Origami._
import doc4s.Components._

object Iterator extends App {
  val dc: Doc[String] = sec("aa", sec("bb", para("xyz")))

  def correct(s: String) = s.toLowerCase
  def corrector(d: Doc[String]) = map(correct)(d)

  val iterateDoc = fold[String, Seq[String], DocF] {
    case Para(a) => Seq(a)
    case Sec(title, c) => title +: c
  } _

  println(iterateDoc(dc))
}

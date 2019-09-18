package exercises.functors

import cats.Functor

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

trait Printable[A] {
  self =>

  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] = new Printable[B] {
    def format(value: B): String = self.format(func(value))
  }
}

final case class Box[A](value: A)

object Printable {
  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = p.contramap[Box[A]](_.value)

  implicit val booleanPrintable =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)
}

trait Codec[A] {
  self =>

  def encode(value: A): String

  def decode(value: String): A

  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    def encode(value: B): String = self.encode(enc(value))

    def decode(value: String): B = dec(self.decode(value))
  }
}

object CodecInstances {
  implicit val doubleCodec: Codec[Double] = new Codec[Double] {
    def encode(value: Double): String = value.toString

    def decode(value: String): Double = value.toDouble
  }

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = c.imap[Box[A]](Box(_), _.value)

  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)
}

object Functors extends App {

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](tree: Tree[A])(func: A => B): Tree[B] = tree match {
      case Branch(left, right) => Branch(map(left)(func), map(right)(func))
      case Leaf(value) => Leaf(func(value))
    }
  }
  val leaf = Leaf[Int](2)
  val leaf2 = Leaf[Int](3)
  val leaf3 = Leaf[Int](4)
  val branch = Branch[Int](leaf, leaf2)
  val tree = Branch[Int](branch, leaf3)
  println(tree)
  println(Functor[Tree].map(tree)(x => x + 1))

  import Printable._

  format(Box(true))

  import CodecInstances._

  println(encode(123.4))
  println(decode[Double]("123.4"))
  println(encode(Box(123.4)))
  println(decode[Box[Double]]("123.4"))
}

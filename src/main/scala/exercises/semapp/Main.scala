package exercises.semapp

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.data.Validated

object SemApp {
  def product[M[_] : Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = x flatMap { valueX => y map { valueY => (valueX, valueY) } }

  def getValue(data: Map[String, String], fieldName: String): Either[List[String], String] = data.get(fieldName).toRight(List(s"Key $fieldName not found in the map"))

  def parseInt(string: String): Either[List[String], Int] = {
    import cats.syntax.either._

    Either.catchOnly[NumberFormatException](string.toInt).leftMap(_ => List(s"$string can't be parsed to int"))
  }

  def nonBlank(string: String): Either[List[String], String] = if (string.isEmpty) Left(List("String is empty")) else Right(string)

  def nonNegative(number: Int): Either[List[String], Int] = if (number >= 0) Right(number) else Left(List("Number is negative"))

  def readName(data: Map[String, String]): Either[List[String], String] = getValue(data, "name") flatMap nonBlank

  def readAge(data: Map[String, String]): Either[List[String], Int] = getValue(data, "age") flatMap parseInt flatMap nonNegative

  def parse(data: Map[String, String]): Validated[List[String], User] = {
    import cats.implicits._

    (
      readName(data).toValidated,
      readAge(data).toValidated
    ).mapN(User.apply)
  }
}

case class User(name: String, age: Int)

object Main extends App {

  import SemApp._

  val testMap = Map("name" -> "Dade Murphy", "age" -> "2")
  println(getValue(testMap, "name"))
  println(getValue(testMap, "foo"))

  println(parseInt("2"))
  println(parseInt("bar"))

  println(nonBlank("foo"))
  println(nonBlank(""))
  println(nonNegative(2))
  println(nonNegative(-1))

  println(readName(testMap))
  val unnamedMap: Map[String, String] = Map()
  println(readName(unnamedMap))
  val emptyMap = Map("name" -> "")
  println(readName(emptyMap))
  println(readAge(testMap))
  val unparsableMap = Map("age" -> "foo")
  println(readAge(unparsableMap))
  val negativeMap = Map("age" -> "-2")

  println(parse(testMap))
  val invalidMap = Map("name" -> "", "age" -> "-2")
  println(parse(invalidMap))

  import cats.data._
  val foo = NonEmptyChain(1, 2, 3, 4)
  println(foo.toNonEmptyList.toList.mkString(", "))
}

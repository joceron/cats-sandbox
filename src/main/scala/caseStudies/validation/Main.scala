package caseStudies.validation

import cats.Semigroup
import cats.data.{Kleisli, Validated}
import cats.syntax.either._
import cats.syntax.semigroup._
import cats.syntax.apply._
import cats.data.Validated._ // for Valid and Invalid

case class Predicate[E, A](func: A => Validated[E, A]) {
  def apply(value: A): Validated[E, A] = func(value)

  def and(that: Predicate[E, A])(implicit s: Semigroup[E]): Predicate[E, A] = Predicate { value =>
    (this (value), that(value)).mapN((_, _) => value)
  }

  def or(that: Predicate[E, A])(implicit s: Semigroup[E]): Predicate[E, A] = Predicate { value =>
    this (value) match {
      case Valid(a) => Valid(a)
      case Invalid(e1) => that(value) match {
        case Valid(a) => Valid(a)
        case Invalid(e2) => Invalid(e1 |+| e2)
      }
    }
  }

  def run: A => Either[E, A] = (value: A) => func(value).toEither
}

case class Check[E, A, B](func: A => Validated[E, B]) {
  def apply(value: A): Validated[E, B] = func(value)

  def map[C](func: B => C): Check[E, A, C] = Check { value =>
    this (value).map(func)
  }

  def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] = Check { value =>
    this (value).withEither(_.flatMap(b => func(b)(value).toEither))
  }

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = Check { value =>
    this (value).withEither(_.flatMap(b => that(b).toEither))
  }
}

object Main extends App {

  import cats.instances.list._ // for Semigroup

  val a: Predicate[List[String], Int] = Predicate { value =>
    if (value > 2) value.asRight.toValidated
    else List("Must be > 2").asLeft.toValidated
  }

  val b: Predicate[List[String], Int] = Predicate { value =>
    if (value < -2) value.asRight.toValidated
    else List("Must be < -2").asLeft.toValidated
  }

  val predicateAnd: Predicate[List[String], Int] = a and b

  println(predicateAnd(5))
  println(predicateAnd(0))

  val predicateOr: Predicate[List[String], Int] = a or b

  println(predicateOr(5))
  println(predicateOr(0))

  import cats.data.NonEmptyList

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] = Predicate { value =>
    if (value.length > n) value.asRight.toValidated
    else error(s"Must be longer than $n characters").asLeft.toValidated
  }

  val alphanumeric: Predicate[Errors, String] = Predicate { value =>
    if (value.forall(_.isLetterOrDigit)) value.asRight.toValidated
    else error(s"Must be all alphanumeric characters").asLeft.toValidated
  }

  def contains(char: Char): Predicate[Errors, String] = Predicate { value =>
    if (value.contains(char)) value.asRight.toValidated
    else error(s"Must contain the character $char").asLeft.toValidated
  }

  def containsOnce(char: Char): Predicate[Errors, String] = Predicate { value =>
    if (value.count(c => c == char) == 1) value.asRight.toValidated
    else error(s"Must contain the character $char only once").asLeft.toValidated
  }

  println(longerThan(3)("1234"))
  println(longerThan(3)("123"))
  println(alphanumeric("12qw"))
  println(alphanumeric("!@#"))
  println(contains('c')("cas"))
  println(contains('c')("das"))
  println(containsOnce('c')("cas"))
  println(containsOnce('c')("ccas"))

  import cats.implicits._

  type Result[A] = Either[Errors, A]
  type KleisliCheck[A, B] = Kleisli[Result, A, B]

  // Create a check from a function:
  def check[A, B](func: A => Result[B]): KleisliCheck[A, B] = Kleisli(func)

  // Create a check from a Predicate:
  def checkPred[A](pred: Predicate[Errors, A]): KleisliCheck[A, A] = Kleisli[Result, A, A](pred.run)

  val checkUsername: KleisliCheck[String, String] = checkPred(longerThan(3) and alphanumeric)

  val splitEmail: KleisliCheck[String, (String, String)] = check(_.split('@') match {
    case Array(name, domain) => Right((name, domain))
    case _                   => Left(error("Must containt a single @ character"))
  })

  val checkLeft: KleisliCheck[String, String] = checkPred(longerThan(0))
  val checkRight: KleisliCheck[String, String] = checkPred(longerThan(3) and contains('.'))
  val joinEmail: KleisliCheck[(String, String), String] = check {
    case(l, r) => (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
  }

  val checkEmail: KleisliCheck[String, String] = splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  def createUser(userName: String, email: String): Either[Errors, User] =
    (checkUsername.run(userName), checkEmail.run(email)).mapN(User)

  println(createUser("Noel", "noel@underscore.io"))
  println(createUser("", "dave@underscore@io"))
}

// Commutative Replicated Data Types
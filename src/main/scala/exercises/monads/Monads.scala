package exercises.monads

import cats.data.{Reader, State, Writer}
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.{Eval, Id}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

trait Monad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(a => pure(func(a)))
}

object MonadInstances {
  implicit val idMonad: Monad[Id] = new Monad[Id] {
    def pure[A](a: A): Id[A] = a

    override def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)

    def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)
  }

  implicit val treeMonad: cats.Monad[Tree] = new cats.Monad[Tree] {
    def flatMap[A, B](tree: Tree[A])(fn: A => Tree[B]): Tree[B] = tree match {
      case l: Leaf[A] => fn(l.value)
      case t: Branch[A] => Monad.branch(flatMap(t.left)(fn), flatMap(t.right)(fn))
    }

    def pure[A](value: A): Tree[A] = Monad.leaf(value)

    def tailRecM[A, B](a: A)(fn: A => Tree[Either[A, B]]): Tree[B] = fn(a) match {
      case Branch(left, right) => Branch(flatMap(left) {
        case Left(value) => tailRecM(value)(fn)
        case Right(value) => pure(value)
      }, flatMap(right) {
        case Left(value) => tailRecM(value)(fn)
        case Right(value) => pure(value)
      })
      case Leaf(Left(value)) => tailRecM(value)(fn)
      case Leaf(Right(value)) => Leaf(value)
    }
  }
}

case class Db(usernames: Map[Int, String], passwords: Map[String, String])

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Monad {
  def pure[F[_], A](a: A)(implicit m: Monad[F]): F[A] = m.pure(a)

  def map[F[_], A, B](value: F[A])(func: A => B)(implicit m: Monad[F]): F[B] = m.map(value)(func)

  def flatMap[F[_], A, B](value: F[A])(func: A => F[B])(implicit m: Monad[F]): F[B] = m.flatMap(value)(func)

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = as match {
    case head :: tail => Eval.defer(foldRight(tail, acc)(fn).map(result => fn(head, result)))
    case Nil => Eval.now(acc)
  }

  def slowly[A](body: => A): A = try body finally Thread.sleep(100)

  type Logged[A] = Writer[Vector[String], A]

  def factorial(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) 1.pure[Logged] else slowly(factorial(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] = Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    username <- findUsername(userId)
    result <- username.map(u => checkPassword(u, password)).getOrElse(false.pure[DbReader])
  } yield result

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = State[List[Int], Int] { oldStack =>
    val result = Try(sym.toInt) match {
      case Success(v) => v
      case Failure(_) => parseOperation(sym)(oldStack.head, oldStack.tail.head)
    }
    val newStack = Try(sym.toInt) match {
      case Success(v) => v :: oldStack
      case Failure(_) => result :: oldStack.drop(2)
    }
    (newStack, result)
  }

  def parseOperation(sym: String): (Int, Int) => Int = sym match {
    case "+" => _ + _
    case "-" => _ - _
    case "*" => _ * _
    case "/" => _ / _
    case _ => sys.error("Error")
  }

  def evalAll(input: List[String]): CalcState[Int] = input.foldLeft(0.pure[CalcState]) { (a, b) =>
    a.flatMap(_ => evalOne(b))
  }

  def evalInput(input: String): Int = {
    val inputList = input.split(" ").toList
    evalAll(inputList).runA(Nil).value
  }

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  def flatMapM[F[_], A, B](tree: F[A])(fn: A => F[B])(implicit m: cats.Monad[F]): F[B] = m.flatMap(tree)(fn)

  def pureM[F[_], A](value: A)(implicit m: cats.Monad[F]): F[A] = m.pure(value)
}

object Monads extends App {

  import MonadInstances._
  val id = Monad.pure(123)
  println(id)
  println(Monad.map(id)(_ * 2))
  println(Monad.flatMap(id)(_ * 2))

  val list: List[Int] = (1 to 5).toList
  println(Monad.foldRight(list, 0)(_ + _).value)

  println(Await.result(Future.sequence(Vector(Future(Monad.factorial(3).run), Future(Monad.factorial(3).run))), 5 seconds))

  val users = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")
  val passwords = Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")
  val db = Db(users, passwords)
  println(Monad.checkLogin(1, "zerocool").run(db))
  println(Monad.checkLogin(4, "davinci").run(db))

  println(Monad.evalOne("42").runA(Nil).value)
  val program = for {
    _ <- Monad.evalOne("1")
    _ <- Monad.evalOne("2")
    ans <- Monad.evalOne("+")
  } yield ans
  println(program.runA(Nil).value)
  val program2 = Monad.evalAll(List("1", "2", "+", "3", "*"))
  println(program2.runA(Nil).value)
  val program3 = for {
    _ <- Monad.evalAll(List("1", "2", "+"))
    _ <- Monad.evalAll(List("3", "4", "+"))
    ans <- Monad.evalOne("*")
  } yield ans
  println(program3.runA(Nil).value)
  println(Monad.evalInput("1 2 + 3 4 + *"))

  val leaf = Monad.leaf(1)
  val leaf2 = Monad.leaf(2)
  val branch = Monad.branch(leaf, leaf2)
  val leaf3 = Monad.leaf(3)
  val branch2 = Monad.branch(branch, leaf3)
  println(Monad.pureM(branch2)(treeMonad))
  println(Monad.flatMapM(branch2)(num => Monad.leaf(num + 1))(treeMonad))
}

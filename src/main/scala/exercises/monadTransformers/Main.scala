package exercises.monadTransformers

import cats.data.EitherT
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object MonadTransformers {
  type Response[A] = EitherT[Future, String, A] // Defined type alias Response

  val powerLevels: Map[String, Int] = Map("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(powerLevel) => EitherT.right(Future.successful(powerLevel))
      case None => EitherT.left(Future.successful(s"$autobot unreachable"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      powerLevel1 <- getPowerLevel(ally1)
      powerLevel2 <- getPowerLevel(ally2)
    } yield (powerLevel1 + powerLevel2) > 15
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    val can = for {
      either <- canSpecialMove(ally1, ally2).value
    } yield either
    Await.result(can, 10 seconds) match {
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge"
      case Left(message) => s"Comms error: $message"
    }
  }
}

object Main extends App {

  import MonadTransformers._

  for {
    either <- getPowerLevel("Jazz").value
    _ = println(either.right.get)
  } yield ()
  for {
    either <- getPowerLevel("Foo").value
    _ = println(either.left.get)
  } yield ()

  val result1 = for {
    either <- canSpecialMove("Jazz", "Hot Rod").value
  } yield either
  println(Await.result(result1, 10 seconds))
  val result2 = for {
    either <- canSpecialMove("Jazz", "Bumblebee").value
  } yield either
  println(Await.result(result2, 10 seconds))
  val result3 = for {
    either <- canSpecialMove("Foo", "Bar").value
  } yield either
  println(Await.result(result3, 10 seconds))

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))
}
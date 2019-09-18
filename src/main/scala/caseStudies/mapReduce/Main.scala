package caseStudies.mapReduce

import cats.Monoid
import cats.syntax.semigroup._

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Main extends App {

  def foldMap[A, B](values: Vector[A])(function: A => B)(implicit m: Monoid[B]): B = {
    val valuesB: Vector[B] = values.map(function)
    m.combineAll(valuesB)
  }


  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val availableProcessors = Runtime.getRuntime.availableProcessors
    val data = values.grouped(availableProcessors)
    val collectionFuture = for {
      singleProcData <- data
    } yield Future.successful(foldMap(singleProcData)(func))

    val futureCollection: Future[Iterator[B]] = Future.sequence(collectionFuture)

    futureCollection map {collection => collection.foldLeft(Monoid[B].empty)(_ |+| _)}
  }

  import cats.instances.int._ // for Monoid

  println(foldMap(Vector(1, 2, 3))(identity))
  // res2: Int = 6

  import cats.instances.string._ // for Monoid

  // Mapping to a String uses the concatenation monoid:
  println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
  // res4: String = "1! 2! 3! "

  // Mapping over a String to produce a String:
  println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))
  // res6: String = HELLO WORLD!

  val result = parallelFoldMap((1 to 1000000).toVector)(identity)
  println(Await.result(result, 1.second))
}

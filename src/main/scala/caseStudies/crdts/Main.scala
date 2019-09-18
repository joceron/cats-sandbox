package caseStudies.crdts

import cats.Monoid
import cats.instances.list._
import cats.instances.map._
import cats.syntax.foldable._
import cats.syntax.semigroup._

import scala.math.max

trait GCounter[F[_,_],K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit m: Monoid[V]): V
}

object GCounter {
  implicit def mapInstance[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
    override def increment(f: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]): Map[K, V] =
      f.get(k).fold(f)(value => f + (k -> (value |+| v)))

    override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] = f1 |+| f2

    // This takes the max of all the numbers, not the sum of them. Is it wrong in the book?
    override def total(f: Map[K, V])(implicit m: Monoid[V]): V = f.values.toList.combineAll
  }

  def apply[F[_,_], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] = counter
}

trait BoundedSemiLattice[A] extends Monoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

object BoundedSemiLattice {
  implicit val intBoundedSemiLattice: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
      override def combine(a1: Int, a2: Int): Int = max(a1, a2)
      override def empty: Int = 0
    }

  implicit def setBoundedSemiLattice[A](): BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
    override def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2
    override def empty: Set[A] = Set.empty
  }
}

trait KeyValueStore[F[_, _]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
  def get[K, V](f: F[K, V])(k: K): Option[V]
  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V = get(f)(k).getOrElse(default)
  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {
  implicit val mapInstance: KeyValueStore[Map] = new KeyValueStore[Map] {
      override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)
      override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)
      override def values[K, V](f: Map[K, V]): List[V] = f.values.toList
    }
}

object Main extends App {
  import BoundedSemiLattice._

  val g1 = Map("a" -> 7 , "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)
  val counter = GCounter[Map, String, Int]
  val merged = counter.merge(g1, g2)
  println(merged)
  println(counter.total(merged))
}

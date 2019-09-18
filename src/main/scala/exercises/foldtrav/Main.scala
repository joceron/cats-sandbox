package exercises.foldtrav

import cats.Monoid
import cats.instances.int._

object Main extends App {

  println(List(1, 2, 3).foldLeft(List.empty[Int])((acc, item) => item :: acc))
  println(List(1, 2, 3).foldRight(List.empty[Int])((item, acc) => item :: acc))

  def mapList[T, S](list: List[T])(function: T => S): List[S] = list.foldRight(List.empty[S])((item, acc) => function(item) :: acc)

  def flatMapList[T, S](list: List[T])(function: T => List[S]): List[S] = list.foldRight(List.empty[S])((item, acc) => function(item) ::: acc)

  def filterList[T](list: List[T])(function: T => Boolean): List[T] = list.foldRight(List.empty[T])((item, acc) => if (function(item)) item :: acc else acc)

  def sumList[T](list: List[T])(implicit m: Monoid[T]): T = list.foldRight(m.empty)(m.combine)

  println(mapList(List(1, 2, 3))(value => value + 1))
  println(flatMapList(List(1, 2, 3))(value => List(value + 1)))
  println(filterList(List(1, 2, 3, 4))(value => value > 2))
  println(sumList(List(1, 2, 3)))

}

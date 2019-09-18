package caseStudies.testAsync

import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Id}

import scala.concurrent.Future

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  override def getUptime(hostname: String): Future[Int]
}

trait TestUptimeClient[F[_]] extends UptimeClient[Id] {
  override def getUptime(hostname: String): Int
}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
     hostnames.traverse(client.getUptime).map(_.sum)
}

class TestUptimeClientImpl(hosts: Map[String, Int]) extends TestUptimeClient[Id] {
  override def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)
}

object Main extends App {

  def testTotalUptime(): Unit = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClientImpl(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

}

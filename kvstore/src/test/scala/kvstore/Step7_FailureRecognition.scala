package kvstore

import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import kvstore.Arbiter.{JoinedSecondary, Replicas, JoinedPrimary, Join}
import kvstore.Replicator.{SnapshotAck, Snapshot}
import scala.concurrent.duration._
import scala.language.postfixOps

class Step7_FailureRecognition extends TestKit(ActorSystem("Step7FailureRecognition"))
with FunSuite
with BeforeAndAfterAll
with ShouldMatchers
with ImplicitSender
with Tools {

  override def afterAll(): Unit = {
    system.shutdown()
  }

  test("case1: Flaky persistence without secondaries") {
    val arbiter = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "case1-primary")
    val user = session(primary)

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    (1 to 20).foreach { i =>
      user.setAcked(s"k$i", s"v$i")
    }
  }

  test("case3: Flaky persistence with one deaf secondary") {
    val arbiter = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "case2-primary")
    val user = session(primary)
    val secondary = TestProbe()

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)
    arbiter.send(primary, Replicas(Set(primary, secondary.ref)))

    (1 to 5).foreach { i =>
      val (k, v) = (s"k$i", s"v$i")
      val ack = user.set(k, v)
      secondary.expectMsgType[Snapshot]
      user.nothingHappens(800 milliseconds)
      user.waitFailed(ack)
    }
  }

  test("case3: Flaky persistence in primary with two good secondaries") {
    val arbiter = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "case3-primary")
    val user = session(primary)
    val secondary1 = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case3-secondary1")
    val secondary2 = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case3-secondary2")
    val secondary1User = session(secondary1)
    val secondary2User = session(secondary1)

    arbiter.expectMsg(Join)
    arbiter.expectMsg(Join)
    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)
    arbiter.send(secondary1, JoinedSecondary)
    arbiter.send(secondary2, JoinedSecondary)
    arbiter.send(primary, Replicas(Set(primary, secondary1, secondary2)))

    (1 to 20).foreach { i =>
      val (k, v) = (s"k$i", s"v$i")
      val ack = user.set(k, v)
      user.waitAck(ack)
      secondary1User.get(k) should equal (Some(v))
      secondary2User.get(k) should equal (Some(v))
    }
  }

  test("case4: Flaky persistence in secondary") {
    val arbiter = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case4-primary")
    val user = session(primary)
    val secondary1 = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "case4-secondary1")
    val secondary1User = session(secondary1)

    arbiter.expectMsg(Join)
    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)
    arbiter.send(secondary1, JoinedSecondary)
    arbiter.send(primary, Replicas(Set(primary, secondary1)))

    (1 to 20).foreach { i =>
      val (k, v) = (s"k$i", s"v$i")
      val ack = user.set(k, v)
      user.waitAck(ack)
      secondary1User.get(k) should equal (Some(v))
    }
  }

}
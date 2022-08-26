package graph

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import matchers.should.Matchers._

class GraphTest extends AnyFlatSpec:
  val graph = Graph.empty.add.add.add.add
    .connect(0, Seq(1, 2))
    .connect(1, Seq(0, 3))
    .connect(2, Seq(0))
    .connect(3, Seq(1))

  "A Graph" should "return its size" in {
    graph.size shouldEqual 4
  }

  it should "return its vertices" in {
    graph.vertices shouldEqual Seq(0, 1, 2, 3)
  }

  it should "return its edges" in {
    graph.edges shouldEqual Seq((0, 1), (0, 2), (1, 3))
  }

  "Neighbours" should "return neighbours" in {
    graph.neighboursOf(0) shouldEqual Seq(1, 2)
    graph.neighboursOf(1) shouldEqual Seq(0, 3)
    graph.neighboursOf(2) shouldEqual Seq(0)
    graph.neighboursOf(3) shouldEqual Seq(1)
  }

  it should "not allow out of bounds queries" in {
    the[IllegalArgumentException] thrownBy {
      graph.neighboursOf(4)
    } should have message "requirement failed: 4 out of bounds (3)"
  }

  "Add" should "add to the end" in {
    val added = graph.add
    added.size shouldEqual graph.size + 1
    added.neighboursOf(added.size - 1) shouldBe empty
  }

  "Remove" should "remove all edges and reindex" in {
    val removed0 = graph.remove(0)
    removed0.size shouldEqual graph.size - 1
    removed0.neighboursOf(0) shouldEqual Seq(2)
    removed0.neighboursOf(1) shouldEqual Seq()
    removed0.neighboursOf(2) shouldEqual Seq(0)

    val removed2 = graph.remove(2)
    removed2.size shouldEqual graph.size - 1
    removed2.neighboursOf(0) shouldEqual Seq(1)
    removed2.neighboursOf(1) shouldEqual Seq(0, 2)
    removed2.neighboursOf(2) shouldEqual Seq(1)
  }

  "Connect" should "connect both ways" in {
    val connected = graph.connect(2, Seq(1, 3))
    connected.neighboursOf(2) shouldEqual Seq(0, 1, 3)
    connected.neighboursOf(1) shouldEqual Seq(0, 3, 2)
    connected.neighboursOf(3) shouldEqual Seq(1, 2)
  }

  it should "not connect vertex to itself" in {
    the[IllegalArgumentException] thrownBy {
      graph.connect(2, Seq(1, 2, 3))
    } should have message "requirement failed: Cannot connect to itself"
  }

  it should "not allow out of bounds connections" in {
    the[IllegalArgumentException] thrownBy {
      graph.connect(2, Seq(1, 3, 4))
    } should have message "requirement failed: 4 out of bounds (3)"
  }

  it should "collapse duplicate connections" in {
    val connected = graph.connect(2, Seq(1, 1, 3))
    connected.neighboursOf(2) shouldEqual Seq(0, 1, 3)
    connected.neighboursOf(1) shouldEqual Seq(0, 3, 2)
    connected.neighboursOf(3) shouldEqual Seq(1, 2)
  }

  it should "ignore existing connections" in {
    val connected = graph.connect(2, Seq(0, 1, 3))
    connected.neighboursOf(2) shouldEqual Seq(0, 1, 3)
    connected.neighboursOf(1) shouldEqual Seq(0, 3, 2)
    connected.neighboursOf(3) shouldEqual Seq(1, 2)
  }

  "Disconnect" should "disconnect both ways" in {
    val disconnected = graph.disconnect(0, Seq(1, 2))
    disconnected.neighboursOf(0) shouldEqual Seq()
    disconnected.neighboursOf(1) shouldEqual Seq(3)
    disconnected.neighboursOf(2) shouldEqual Seq()
    disconnected.neighboursOf(3) shouldEqual Seq(1)
  }

  it should "not allow out of bounds disconnections" in {
    the[IllegalArgumentException] thrownBy {
      graph.disconnect(2, Seq(1, 3, 4))
    } should have message "requirement failed: 4 out of bounds (3)"
  }
end GraphTest

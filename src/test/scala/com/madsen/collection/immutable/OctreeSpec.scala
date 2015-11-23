package com.madsen.collection.immutable

import org.scalatest.FlatSpec

/**
  * Created by erikmadsen2 on 22/11/2015.
  */
class OctreeSpec extends FlatSpec {

  "An octree" should "be empty when no elements are added" in {
    val o = Octree()

    assert(o.isEmpty)
  }

  it should "report correct size when new elements are added" in {
    val o: Octree[Int] = Octree()
    val n: Octree[Int] = o + ((1L, 2L, 3L) → 22) + ((2L, 2L, 3L) → 22)

    assert(n.size == 2)
  }

  it should "bla" in {
    val pointA = (1L, 45L, 14L)
    val pointB = (65L, 15L, 14L)
    val o = Octree[Int]() + (pointA → 78) + (pointB → 23)

    assert(o.contains(pointA))
    assert(o.contains(pointB))

    val o1 = o - pointB

    assert(o1.contains(pointA))
    assert(!o1.contains(pointB))

    val o2 = o1 - pointA

    assert(!o2.contains(pointA))
    assert(!o2.contains(pointB))
    assert(o2.isEmpty)
  }
}

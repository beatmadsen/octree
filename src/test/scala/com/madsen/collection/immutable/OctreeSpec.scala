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
}

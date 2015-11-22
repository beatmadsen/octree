package com.madsen.collection.immutable

import org.scalatest.FlatSpec

/**
  * Created by erikmadsen2 on 22/11/2015.
  */
class OctreeSpec extends FlatSpec {

  "An octree" should "bla" in {
    val o = Octree()

    assert(o.isEmpty)
  }

  it should "scuby" in {
    val o: Octree[Int] = Octree()

    o + ((1L, 2L, 3L) → 22) + ((2L, 2L, 3L) → 22)

    assert(o.size == 2)
  }
}

package jmh
/* vim: set fdm=marker : */

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

import scala.offheap._
import scala.offheap.numeric._

/* In sbt:
   jmh/run DenseMatrix
*/

@State(Scope.Thread)
class DenseMatrix {
  import scala.language.experimental.macros

  implicit val props = Region.Props(Pool(
    pageSize=  200 * (1 << 20),
    chunkSize=2000 * (1 << 20)))

  val d1: Double = math.random
  val d2: Double = math.random

  /* {{{ Matrices definition */
  val offheapMatrix1_10 = DenseMatrix.rand(10, 10)(malloc)
  val offheapMatrix2_10 = DenseMatrix.rand(10, 10)(malloc)
  val offheapMatrix3_10 = DenseMatrix.rand(10, 10)(malloc)
  val offheapMatrix4_10 = DenseMatrix.rand(10, 10)(malloc)
  val breezeMatrix1_10 = breeze.linalg.DenseMatrix.rand(10, 10)
  val breezeMatrix2_10 = breeze.linalg.DenseMatrix.rand(10, 10)
  val breezeMatrix3_10 = breeze.linalg.DenseMatrix.rand(10, 10)
  val breezeMatrix4_10 = breeze.linalg.DenseMatrix.rand(10, 10)

  val offheapMatrix1_15 = DenseMatrix.rand(15, 15)(malloc)
  val offheapMatrix2_15 = DenseMatrix.rand(15, 15)(malloc)
  val offheapMatrix3_15 = DenseMatrix.rand(15, 15)(malloc)
  val offheapMatrix4_15 = DenseMatrix.rand(15, 15)(malloc)
  val breezeMatrix1_15 = breeze.linalg.DenseMatrix.rand(15, 15)
  val breezeMatrix2_15 = breeze.linalg.DenseMatrix.rand(15, 15)
  val breezeMatrix3_15 = breeze.linalg.DenseMatrix.rand(15, 15)
  val breezeMatrix4_15 = breeze.linalg.DenseMatrix.rand(15, 15)

  val offheapMatrix1_20 = DenseMatrix.rand(20, 20)(malloc)
  val offheapMatrix2_20 = DenseMatrix.rand(20, 20)(malloc)
  val offheapMatrix3_20 = DenseMatrix.rand(20, 20)(malloc)
  val offheapMatrix4_20 = DenseMatrix.rand(20, 20)(malloc)
  val breezeMatrix1_20 = breeze.linalg.DenseMatrix.rand(20, 20)
  val breezeMatrix2_20 = breeze.linalg.DenseMatrix.rand(20, 20)
  val breezeMatrix3_20 = breeze.linalg.DenseMatrix.rand(20, 20)
  val breezeMatrix4_20 = breeze.linalg.DenseMatrix.rand(20, 20)

  val offheapMatrix1_25 = DenseMatrix.rand(25, 25)(malloc)
  val offheapMatrix2_25 = DenseMatrix.rand(25, 25)(malloc)
  val offheapMatrix3_25 = DenseMatrix.rand(25, 25)(malloc)
  val offheapMatrix4_25 = DenseMatrix.rand(25, 25)(malloc)
  val breezeMatrix1_25 = breeze.linalg.DenseMatrix.rand(25, 25)
  val breezeMatrix2_25 = breeze.linalg.DenseMatrix.rand(25, 25)
  val breezeMatrix3_25 = breeze.linalg.DenseMatrix.rand(25, 25)
  val breezeMatrix4_25 = breeze.linalg.DenseMatrix.rand(25, 25)

  val offheapMatrix1_30 = DenseMatrix.rand(30, 30)(malloc)
  val offheapMatrix2_30 = DenseMatrix.rand(30, 30)(malloc)
  val offheapMatrix3_30 = DenseMatrix.rand(30, 30)(malloc)
  val offheapMatrix4_30 = DenseMatrix.rand(30, 30)(malloc)
  val breezeMatrix1_30 = breeze.linalg.DenseMatrix.rand(30, 30)
  val breezeMatrix2_30 = breeze.linalg.DenseMatrix.rand(30, 30)
  val breezeMatrix3_30 = breeze.linalg.DenseMatrix.rand(30, 30)
  val breezeMatrix4_30 = breeze.linalg.DenseMatrix.rand(30, 30)

  val offheapMatrix1_40 = DenseMatrix.rand(40, 40)(malloc)
  val offheapMatrix2_40 = DenseMatrix.rand(40, 40)(malloc)
  val offheapMatrix3_40 = DenseMatrix.rand(40, 40)(malloc)
  val offheapMatrix4_40 = DenseMatrix.rand(40, 40)(malloc)
  val breezeMatrix1_40 = breeze.linalg.DenseMatrix.rand(40, 40)
  val breezeMatrix2_40 = breeze.linalg.DenseMatrix.rand(40, 40)
  val breezeMatrix3_40 = breeze.linalg.DenseMatrix.rand(40, 40)
  val breezeMatrix4_40 = breeze.linalg.DenseMatrix.rand(40, 40)

  val offheapMatrix1_50 = DenseMatrix.rand(50, 50)(malloc)
  val offheapMatrix2_50 = DenseMatrix.rand(50, 50)(malloc)
  val offheapMatrix3_50 = DenseMatrix.rand(50, 50)(malloc)
  val offheapMatrix4_50 = DenseMatrix.rand(50, 50)(malloc)
  val breezeMatrix1_50 = breeze.linalg.DenseMatrix.rand(50, 50)
  val breezeMatrix2_50 = breeze.linalg.DenseMatrix.rand(50, 50)
  val breezeMatrix3_50 = breeze.linalg.DenseMatrix.rand(50, 50)
  val breezeMatrix4_50 = breeze.linalg.DenseMatrix.rand(50, 50)

  val offheapMatrix1_66 = DenseMatrix.rand(66, 66)(malloc)
  val offheapMatrix2_66 = DenseMatrix.rand(66, 66)(malloc)
  val offheapMatrix3_66 = DenseMatrix.rand(66, 66)(malloc)
  val offheapMatrix4_66 = DenseMatrix.rand(66, 66)(malloc)
  val breezeMatrix1_66 = breeze.linalg.DenseMatrix.rand(66, 66)
  val breezeMatrix2_66 = breeze.linalg.DenseMatrix.rand(66, 66)
  val breezeMatrix3_66 = breeze.linalg.DenseMatrix.rand(66, 66)
  val breezeMatrix4_66 = breeze.linalg.DenseMatrix.rand(66, 66)

  val offheapMatrix1_83 = DenseMatrix.rand(83, 83)(malloc)
  val offheapMatrix2_83 = DenseMatrix.rand(83, 83)(malloc)
  val offheapMatrix3_83 = DenseMatrix.rand(83, 83)(malloc)
  val offheapMatrix4_83 = DenseMatrix.rand(83, 83)(malloc)
  val breezeMatrix1_83 = breeze.linalg.DenseMatrix.rand(83, 83)
  val breezeMatrix2_83 = breeze.linalg.DenseMatrix.rand(83, 83)
  val breezeMatrix3_83 = breeze.linalg.DenseMatrix.rand(83, 83)
  val breezeMatrix4_83 = breeze.linalg.DenseMatrix.rand(83, 83)

  val offheapMatrix1_100 = DenseMatrix.rand(100, 100)(malloc)
  val offheapMatrix2_100 = DenseMatrix.rand(100, 100)(malloc)
  val offheapMatrix3_100 = DenseMatrix.rand(100, 100)(malloc)
  val offheapMatrix4_100 = DenseMatrix.rand(100, 100)(malloc)
  val breezeMatrix1_100 = breeze.linalg.DenseMatrix.rand(100, 100)
  val breezeMatrix2_100 = breeze.linalg.DenseMatrix.rand(100, 100)
  val breezeMatrix3_100 = breeze.linalg.DenseMatrix.rand(100, 100)
  val breezeMatrix4_100 = breeze.linalg.DenseMatrix.rand(100, 100)

  val offheapMatrix1_500 = DenseMatrix.rand(500, 500)(malloc)
  val offheapMatrix2_500 = DenseMatrix.rand(500, 500)(malloc)
  val offheapMatrix3_500 = DenseMatrix.rand(500, 500)(malloc)
  val offheapMatrix4_500 = DenseMatrix.rand(500, 500)(malloc)
  val breezeMatrix1_500 = breeze.linalg.DenseMatrix.rand(500, 500)
  val breezeMatrix2_500 = breeze.linalg.DenseMatrix.rand(500, 500)
  val breezeMatrix3_500 = breeze.linalg.DenseMatrix.rand(500, 500)
  val breezeMatrix4_500 = breeze.linalg.DenseMatrix.rand(500, 500)

  val offheapMatrix1_1000 = DenseMatrix.rand(1000, 1000)(malloc)
  val offheapMatrix2_1000 = DenseMatrix.rand(1000, 1000)(malloc)
  val offheapMatrix3_1000 = DenseMatrix.rand(1000, 1000)(malloc)
  val offheapMatrix4_1000 = DenseMatrix.rand(1000, 1000)(malloc)
  val breezeMatrix1_1000 = breeze.linalg.DenseMatrix.rand(1000, 1000)
  val breezeMatrix2_1000 = breeze.linalg.DenseMatrix.rand(1000, 1000)
  val breezeMatrix3_1000 = breeze.linalg.DenseMatrix.rand(1000, 1000)
  val breezeMatrix4_1000 = breeze.linalg.DenseMatrix.rand(1000, 1000)

  val offheapMatrix1_5000 = DenseMatrix.rand(5000, 5000)(malloc)
  val offheapMatrix2_5000 = DenseMatrix.rand(5000, 5000)(malloc)
  val offheapMatrix3_5000 = DenseMatrix.rand(5000, 5000)(malloc)
  val offheapMatrix4_5000 = DenseMatrix.rand(5000, 5000)(malloc)
  val breezeMatrix1_5000 = breeze.linalg.DenseMatrix.rand(5000, 5000)
  val breezeMatrix2_5000 = breeze.linalg.DenseMatrix.rand(5000, 5000)
  val breezeMatrix3_5000 = breeze.linalg.DenseMatrix.rand(5000, 5000)
  val breezeMatrix4_5000 = breeze.linalg.DenseMatrix.rand(5000, 5000)
  /* }}} */

  /* {{{ Multiplication 1 (A * B) */
  @Benchmark // {{{ 10
  def offheapMultiplicationU10 = {
    Region { implicit r =>
      offheapMatrix1_10 * offheapMatrix2_10
    }
  }
  @Benchmark
  def breezeMultiplicationU10 = {
    breezeMatrix1_10 * breezeMatrix2_10
  } // }}}

  @Benchmark // {{{ 15
  def offheapMultiplicationU15 = {
    Region { implicit r =>
      offheapMatrix1_15 * offheapMatrix2_15
    }
  }
  @Benchmark
  def breezeMultiplicationU15 = {
    breezeMatrix1_15 * breezeMatrix2_15
  } // }}}

  @Benchmark // {{{ 20
  def offheapMultiplicationU20 = {
    Region { implicit r =>
      offheapMatrix1_20 * offheapMatrix2_20
    }
  }
  @Benchmark
  def breezeMultiplicationU20 = {
    breezeMatrix1_20 * breezeMatrix2_20
  } // }}}

  @Benchmark // {{{ 25
  def offheapMultiplicationU25 = {
    Region { implicit r =>
      offheapMatrix1_25 * offheapMatrix2_25
    }
  }
  @Benchmark
  def breezeMultiplicationU25 = {
    breezeMatrix1_25 * breezeMatrix2_25
  } // }}}

  @Benchmark // {{{ 30
  def offheapMultiplicationU30 = {
    Region { implicit r =>
      offheapMatrix1_30 * offheapMatrix2_30
    }
  }
  @Benchmark
  def breezeMultiplicationU30 = {
    breezeMatrix1_30 * breezeMatrix2_30
  } // }}}

  @Benchmark // {{{ 40
  def offheapMultiplicationU40 = {
    Region { implicit r =>
      offheapMatrix1_40 * offheapMatrix2_40
    }
  }
  @Benchmark
  def breezeMultiplicationU40 = {
    breezeMatrix1_40 * breezeMatrix2_40
  } // }}}

  @Benchmark // {{{ 50
  def offheapMultiplicationU50 = {
    Region { implicit r =>
      offheapMatrix1_50 * offheapMatrix2_50
    }
  }
  @Benchmark
  def breezeMultiplicationU50 = {
    breezeMatrix1_50 * breezeMatrix2_50
  } // }}}

  @Benchmark // {{{ 66
  def offheapMultiplicationU66 = {
    Region { implicit r =>
      offheapMatrix1_66 * offheapMatrix2_66
    }
  }
  @Benchmark
  def breezeMultiplicationU66 = {
    breezeMatrix1_66 * breezeMatrix2_66
  } // }}}

  @Benchmark // {{{ 83
  def offheapMultiplicationU83 = {
    Region { implicit r =>
      offheapMatrix1_83 * offheapMatrix2_83
    }
  }
  @Benchmark
  def breezeMultiplicationU83 = {
    breezeMatrix1_83 * breezeMatrix2_83
  } // }}}

  @Benchmark // {{{ 100
  def offheapMultiplicationU100 = {
    Region { implicit r =>
      offheapMatrix1_100 * offheapMatrix2_100
    }
  }
  @Benchmark
  def breezeMultiplicationU100 = {
    breezeMatrix1_100 * breezeMatrix2_100
  } // }}}

  @Benchmark // {{{ 500
  def offheapMultiplicationU500 = {
    Region { implicit r =>
      offheapMatrix1_500 * offheapMatrix2_500
    }
  }
  @Benchmark
  def breezeMultiplicationU500 = {
    breezeMatrix1_500 * breezeMatrix2_500
  } // }}}

  @Benchmark // {{{ 1000
  def offheapMultiplicationU1000 = {
    Region { implicit r =>
      offheapMatrix1_1000 * offheapMatrix2_1000
    }
  }
  @Benchmark
  def breezeMultiplicationU1000 = {
    breezeMatrix1_1000 * breezeMatrix2_1000
  } // }}}

  @Benchmark // {{{ 5000
  def offheapMultiplicationU5000 = {
    Region { implicit r =>
      offheapMatrix1_5000 * offheapMatrix2_5000
    }
  }
  @Benchmark
  def breezeMultiplicationU5000 = {
    breezeMatrix1_5000 * breezeMatrix2_5000
  } // }}}
  /* }}} */

  /* {{{ Multiplication 2  (d1 * A * B + d2 * C) */
  @Benchmark // {{{ 10
  def offheapMultiplication2U10 = {
    Region { implicit r =>
      opt(malloc, {
        d1 * offheapMatrix1_10 * offheapMatrix2_10 + d2 * offheapMatrix3_10
      })
    }
  }
  @Benchmark
  def breezeMultiplication2U10 = {
    d1 * breezeMatrix1_10 * breezeMatrix2_10 + d2 * breezeMatrix3_10
  } // }}}

  @Benchmark // {{{ 15
  def offheapMultiplication2U15 = {
    Region { implicit r =>
      opt(malloc, {
        d1 * offheapMatrix1_15 * offheapMatrix2_15 + d2 * offheapMatrix3_15
      })
    }
  }
  @Benchmark
  def breezeMultiplication2U15 = {
    d1 * breezeMatrix1_15 * breezeMatrix2_15 + d2 * breezeMatrix3_15
  } // }}}

  @Benchmark // {{{ 20
  def offheapMultiplication2U20 = {
    Region { implicit r =>
      opt(malloc, {
        d1 * offheapMatrix1_20 * offheapMatrix2_20 + d2 * offheapMatrix3_20
      })
    }
  }
  @Benchmark
  def breezeMultiplication2U20 = {
    d1 * breezeMatrix1_20 * breezeMatrix2_20 + d2 * breezeMatrix3_20
  } // }}}

  @Benchmark // {{{ 25
  def offheapMultiplication2U25 = {
    Region { implicit r =>
      opt(malloc, {
        d1 * offheapMatrix1_25 * offheapMatrix2_25 + d2 * offheapMatrix3_25
      })
    }
  }
  @Benchmark
  def breezeMultiplication2U25 = {
    d1 * breezeMatrix1_25 * breezeMatrix2_25 + d2 * breezeMatrix3_25
  } // }}}

  @Benchmark // {{{ 30
  def offheapMultiplication2U30 = {
    Region { implicit r =>
      opt(malloc, {
        d1 * offheapMatrix1_30 * offheapMatrix2_30 + d2 * offheapMatrix3_30
      })
    }
  }
  @Benchmark
  def breezeMultiplication2U30 = {
    d1 * breezeMatrix1_30 * breezeMatrix2_30 + d2 * breezeMatrix3_30
  } // }}}

  @Benchmark // {{{ 40
  def offheapMultiplication2U40 = {
    Region { implicit r =>
      opt(malloc, {
        d1 * offheapMatrix1_40 * offheapMatrix2_40 + d2 * offheapMatrix3_40
      })
    }
  }
  @Benchmark
  def breezeMultiplication2U40 = {
    d1 * breezeMatrix1_40 * breezeMatrix2_40 + d2 * breezeMatrix3_40
  } // }}}

  @Benchmark // {{{ 50
  def offheapMultiplication2U50 = {
    Region { implicit r =>
      opt(malloc, {
        d1 * offheapMatrix1_50 * offheapMatrix2_50 + offheapMatrix3_50
      })
    }
  }
  @Benchmark
  def breezeMultiplication2U50 = {
    d1 * breezeMatrix1_50 * breezeMatrix2_50 + breezeMatrix3_50
  } // }}}

  @Benchmark // {{{ 66
  def offheapMultiplication2U66 = {
    Region { implicit r =>
      opt(malloc, {
        d1 * offheapMatrix1_66 * offheapMatrix2_66 + d2 * offheapMatrix3_66
      })
    }
  }
  @Benchmark
  def breezeMultiplication2U66 = {
    d1 * breezeMatrix1_66 * breezeMatrix2_66 + d2 * breezeMatrix3_66
  } // }}}

  @Benchmark // {{{ 83
  def offheapMultiplication2U83 = {
    Region { implicit r =>
      opt(malloc, {
        d1 * offheapMatrix1_83 * offheapMatrix2_83 + d2 * offheapMatrix3_83
      })
    }
  }
  @Benchmark
  def breezeMultiplication2U83 = {
    d1 * breezeMatrix1_83 * breezeMatrix2_83 + d2 * breezeMatrix3_83
  } // }}}

  @Benchmark // {{{ 100
  def offheapMultiplication2U100 = {
    Region { implicit r =>
      opt(malloc, {
        d1 * offheapMatrix1_100 * offheapMatrix2_100 + offheapMatrix3_100
      })
    }
  }
  @Benchmark
  def breezeMultiplication2U100 = {
    d1 * breezeMatrix1_100 * breezeMatrix2_100 + breezeMatrix3_100
  } // }}}

  @Benchmark // {{{ 500
  def offheapMultiplication2U500 = {
    Region { implicit r =>
      opt(malloc, {
        d1 * offheapMatrix1_500 * offheapMatrix2_500 + offheapMatrix3_500
      })
    }
  }
  @Benchmark
  def breezeMultiplication2U500 = {
    d1 * breezeMatrix1_500 * breezeMatrix2_500 + breezeMatrix3_500
  } // }}}

  @Benchmark // {{{ 1000
  def offheapMultiplication2U1000 = {
    Region { implicit r =>
      opt(malloc, {
        d1 * offheapMatrix1_1000 * offheapMatrix2_1000 + offheapMatrix3_1000
      })
    }
  }
  @Benchmark
  def breezeMultiplication2U1000 = {
    d1 * breezeMatrix1_1000 * breezeMatrix2_1000 + breezeMatrix3_1000
  } // }}}

  @Benchmark // {{{ 5000
  def offheapMultiplication2U5000 = {
    Region { implicit r =>
      opt(malloc, {
        d1 * offheapMatrix1_5000 * offheapMatrix2_5000 + offheapMatrix3_5000
      })
    }
  }
  @Benchmark
  def breezeMultiplication2U5000 = {
    d1 * breezeMatrix1_5000 * breezeMatrix2_5000 + breezeMatrix3_5000
  } // }}}

  /* }}} */

  /* {{{ Multiplication 3 (A' * B) */
  @Benchmark // {{{ 10
  def offheapMultiplication3U10 = {
    Region { implicit r =>
      (offheapMatrix1_10.t * offheapMatrix2_10).copy
    }
  }
  @Benchmark
  def breezeMultiplication3U10 = {
    (breezeMatrix1_10.t * breezeMatrix2_10).copy
  } // }}}

  @Benchmark // {{{ 15
  def offheapMultiplication3U15 = {
    Region { implicit r =>
      (offheapMatrix1_15.t * offheapMatrix2_15).copy
    }
  }
  @Benchmark
  def breezeMultiplication3U15 = {
    (breezeMatrix1_15.t * breezeMatrix2_15).copy
  } // }}}

  @Benchmark // {{{ 20
  def offheapMultiplication3U20 = {
    Region { implicit r =>
      (offheapMatrix1_20.t * offheapMatrix2_20).copy
    }
  }
  @Benchmark
  def breezeMultiplication3U20 = {
    (breezeMatrix1_20.t * breezeMatrix2_20).copy
  } // }}}

  @Benchmark // {{{ 25
  def offheapMultiplication3U25 = {
    Region { implicit r =>
      (offheapMatrix1_25.t * offheapMatrix2_25).copy
    }
  }
  @Benchmark
  def breezeMultiplication3U25 = {
    (breezeMatrix1_25.t * breezeMatrix2_25).copy
  } // }}}

  @Benchmark // {{{ 30
  def offheapMultiplication3U30 = {
    Region { implicit r =>
      (offheapMatrix1_30.t * offheapMatrix2_30).copy
    }
  }
  @Benchmark
  def breezeMultiplication3U30 = {
    (breezeMatrix1_30.t * breezeMatrix2_30).copy
  } // }}}

  @Benchmark // {{{ 40
  def offheapMultiplication3U40 = {
    Region { implicit r =>
      (offheapMatrix1_40.t * offheapMatrix2_40).copy
    }
  }
  @Benchmark
  def breezeMultiplication3U40 = {
    (breezeMatrix1_40.t * breezeMatrix2_40).copy
  } // }}}

  @Benchmark // {{{ 50
  def offheapMultiplication3U50 = {
    Region { implicit r =>
      (offheapMatrix1_50.t * offheapMatrix2_50).copy
    }
  }
  @Benchmark
  def breezeMultiplication3U50 = {
    (breezeMatrix1_50.t * breezeMatrix2_50).copy
  } // }}}

  @Benchmark // {{{ 66
  def offheapMultiplication3U66 = {
    Region { implicit r =>
      (offheapMatrix1_66.t * offheapMatrix2_66).copy
    }
  }
  @Benchmark
  def breezeMultiplication3U66 = {
    (breezeMatrix1_66.t * breezeMatrix2_66).copy
  } // }}}

  @Benchmark // {{{ 83
  def offheapMultiplication3U83 = {
    Region { implicit r =>
      (offheapMatrix1_83.t * offheapMatrix2_83).copy
    }
  }
  @Benchmark
  def breezeMultiplication3U83 = {
    (breezeMatrix1_83.t * breezeMatrix2_83).copy
  } // }}}

  @Benchmark // {{{ 100
  def offheapMultiplication3U100 = {
    Region { implicit r =>
      (offheapMatrix1_100.t * offheapMatrix2_100).copy
    }
  }
  @Benchmark
  def breezeMultiplication3U100 = {
    (breezeMatrix1_100.t * breezeMatrix2_100).copy
  } // }}}

  @Benchmark // {{{ 500
  def offheapMultiplication3U500 = {
    Region { implicit r =>
      (offheapMatrix1_500.t * offheapMatrix2_500).copy
    }
  }
  @Benchmark
  def breezeMultiplication3U500 = {
    (breezeMatrix1_500.t * breezeMatrix2_500).copy
  } // }}}

  @Benchmark // {{{ 1000
  def offheapMultiplication3U1000 = {
    Region { implicit r =>
      (offheapMatrix1_1000.t * offheapMatrix2_1000).copy
    }
  }
  @Benchmark
  def breezeMultiplication3U1000 = {
    (breezeMatrix1_1000.t * breezeMatrix2_1000).copy
  } // }}}

  @Benchmark // {{{ 5000
  def offheapMultiplication3U5000 = {
    Region { implicit r =>
      (offheapMatrix1_5000.t * offheapMatrix2_5000).copy
    }
  }
  @Benchmark
  def breezeMultiplication3U5000 = {
    (breezeMatrix1_5000.t * breezeMatrix2_5000).copy
  } // }}}

  /* }}} */

/* {{{ Complex example */
/*
  @Benchmark // {{{ example 10
  def offheapExampleU10 = {
    Region { implicit r =>
      opt(malloc, {
        val a = offheapMatrix1_10
        val b = offheapMatrix2_10
        val c = offheapMatrix3_10
        val d = offheapMatrix4_10
        val x = 7.0 * (a + (b + c))
        val y = 4.0 * c * (b + c)
        x + y
      })
    }
  }
  @Benchmark
  def breezeExampleU10 = {
    val a = breezeMatrix1_10
    val b = breezeMatrix2_10
    val c = breezeMatrix3_10
    val d = breezeMatrix4_10
    val x = (a + b + c) * 7.0
    val y = c * (b + c) * 4.0
    x + y
  } // }}}

  @Benchmark // {{{ example 15
  def offheapExampleU15 = {
    Region { implicit r =>
      opt(malloc, {
        val a = offheapMatrix1_15
        val b = offheapMatrix2_15
        val c = offheapMatrix3_15
        val d = offheapMatrix4_15
        val x = 7.0 * (a + (b + c))
        val y = 4.0 * c * (b + c)
        x + y
      })
    }
  }
  @Benchmark
  def breezeExampleU15 = {
    val a = breezeMatrix1_15
    val b = breezeMatrix2_15
    val c = breezeMatrix3_15
    val d = breezeMatrix4_15
    val x = (a + b + c) * 7.0
    val y = c * (b + c) * 4.0
    x + y
  } // }}}

  @Benchmark // {{{ example 20
  def offheapExampleU20 = {
    Region { implicit r =>
      opt(malloc, {
        val a = offheapMatrix1_20
        val b = offheapMatrix2_20
        val c = offheapMatrix3_20
        val d = offheapMatrix4_20
        val x = 7.0 * (a + (b + c))
        val y = 4.0 * c * (b + c)
        x + y
      })
    }
  }
  @Benchmark
  def breezeExampleU20 = {
    val a = breezeMatrix1_20
    val b = breezeMatrix2_20
    val c = breezeMatrix3_20
    val d = breezeMatrix4_20
    val x = (a + b + c) * 7.0
    val y = c * (b + c) * 4.0
    x + y
  } // }}}

  @Benchmark // {{{ example 25
  def offheapExampleU25 = {
    Region { implicit r =>
      opt(malloc, {
        val a = offheapMatrix1_25
        val b = offheapMatrix2_25
        val c = offheapMatrix3_25
        val d = offheapMatrix4_25
        val x = 7.0 * (a + (b + c))
        val y = 4.0 * c * (b + c)
        x + y
      })
    }
  }
  @Benchmark
  def breezeExampleU25 = {
    val a = breezeMatrix1_25
    val b = breezeMatrix2_25
    val c = breezeMatrix3_25
    val d = breezeMatrix4_25
    val x = (a + b + c) * 7.0
    val y = c * (b + c) * 4.0
    x + y
  } // }}}

  @Benchmark // {{{ example 30
  def offheapExampleU30 = {
    Region { implicit r =>
      opt(malloc, {
        val a = offheapMatrix1_30
        val b = offheapMatrix2_30
        val c = offheapMatrix3_30
        val d = offheapMatrix4_30
        val x = 7.0 * (a + (b + c))
        val y = 4.0 * c * (b + c)
        x + y
      })
    }
  }
  @Benchmark
  def breezeExampleU30 = {
    val a = breezeMatrix1_30
    val b = breezeMatrix2_30
    val c = breezeMatrix3_30
    val d = breezeMatrix4_30
    val x = (a + b + c) * 7.0
    val y = c * (b + c) * 4.0
    x + y
  } // }}}

  @Benchmark // {{{ example 40
  def offheapExampleU40 = {
    Region { implicit r =>
      opt(malloc, {
        val a = offheapMatrix1_40
        val b = offheapMatrix2_40
        val c = offheapMatrix3_40
        val d = offheapMatrix4_40
        val x = 7.0 * (a + (b + c))
        val y = 4.0 * c * (b + c)
        x + y
      })
    }
  }
  @Benchmark
  def breezeExampleU40 = {
    val a = breezeMatrix1_40
    val b = breezeMatrix2_40
    val c = breezeMatrix3_40
    val d = breezeMatrix4_40
    val x = (a + b + c) * 7.0
    val y = c * (b + c) * 4.0
    x + y
  } // }}}

  @Benchmark // {{{ example 50
  def offheapExampleU50 = {
    Region { implicit r =>
      opt(malloc, {
        val a = offheapMatrix1_50
        val b = offheapMatrix2_50
        val c = offheapMatrix3_50
        val d = offheapMatrix4_50
        val x = 7.0 * (a + (b + c))
        val y = 4.0 * c * (b + c)
        x + y
      })
    }
  }
  @Benchmark
  def breezeExampleU50 = {
    val a = breezeMatrix1_50
    val b = breezeMatrix2_50
    val c = breezeMatrix3_50
    val d = breezeMatrix4_50
    val x = (a + b + c) * 7.0
    val y = c * (b + c) * 4.0
    x + y
  } // }}}

  @Benchmark // {{{ example 66
  def offheapExampleU66 = {
    Region { implicit r =>
      opt(malloc, {
        val a = offheapMatrix1_66
        val b = offheapMatrix2_66
        val c = offheapMatrix3_66
        val d = offheapMatrix4_66
        val x = 7.0 * (a + (b + c))
        val y = 4.0 * c * (b + c)
        x + y
      })
    }
  }
  @Benchmark
  def breezeExampleU66 = {
    val a = breezeMatrix1_66
    val b = breezeMatrix2_66
    val c = breezeMatrix3_66
    val d = breezeMatrix4_66
    val x = (a + b + c) * 7.0
    val y = c * (b + c) * 4.0
    x + y
  } // }}}

  @Benchmark // {{{ example 83
  def offheapExampleU83 = {
    Region { implicit r =>
      opt(malloc, {
        val a = offheapMatrix1_83
        val b = offheapMatrix2_83
        val c = offheapMatrix3_83
        val d = offheapMatrix4_83
        val x = 7.0 * (a + (b + c))
        val y = 4.0 * c * (b + c)
        x + y
      })
    }
  }
  @Benchmark
  def breezeExampleU83 = {
    val a = breezeMatrix1_83
    val b = breezeMatrix2_83
    val c = breezeMatrix3_83
    val d = breezeMatrix4_83
    val x = (a + b + c) * 7.0
    val y = c * (b + c) * 4.0
    x + y
  } // }}}

  @Benchmark // {{{ example 100
  def offheapExampleU100 = {
    Region { implicit r =>
      opt(malloc, {
        val a = offheapMatrix1_100
        val b = offheapMatrix2_100
        val c = offheapMatrix3_100
        val d = offheapMatrix4_100
        val x = 7.0 * (a + (b + c))
        val y = 4.0 * c * (b + c)
        x + y
      })
    }
  }
  @Benchmark
  def breezeExampleU100 = {
    val a = breezeMatrix1_100
    val b = breezeMatrix2_100
    val c = breezeMatrix3_100
    val d = breezeMatrix4_100
    val x = (a + b + c) * 7.0
    val y = c * (b + c) * 4.0
    x + y
  } // }}}

  @Benchmark // {{{ example 500
  def offheapExampleU500 = {
    Region { implicit r =>
      opt(malloc, {
        val a = offheapMatrix1_500
        val b = offheapMatrix2_500
        val c = offheapMatrix3_500
        val d = offheapMatrix4_500
        val x = 7.0 * (a + (b + c))
        val y = 4.0 * c * (b + c)
        x + y
      })
    }
  }
  @Benchmark
  def breezeExampleU500 = {
    val a = breezeMatrix1_500
    val b = breezeMatrix2_500
    val c = breezeMatrix3_500
    val d = breezeMatrix4_500
    val x = (a + b + c) * 7.0
    val y = c * (b + c) * 4.0
    x + y
  } // }}}

  @Benchmark // {{{ example 1000
  def offheapExampleU1000 = {
    Region { implicit r =>
      opt(malloc, {
        val a = offheapMatrix1_1000
        val b = offheapMatrix2_1000
        val c = offheapMatrix3_1000
        val d = offheapMatrix4_1000
        val x = 7.0 * (a + (b + c))
        val y = 4.0 * c * (b + c)
        x + y
      })
    }
  }
  @Benchmark
  def breezeExampleU1000 = {
    val a = breezeMatrix1_1000
    val b = breezeMatrix2_1000
    val c = breezeMatrix3_1000
    val d = breezeMatrix4_1000
    val x = (a + b + c) * 7.0
    val y = c * (b + c) * 4.0
    x + y
  } // }}}

  @Benchmark // {{{ example 5000
  def offheapExampleU5000 = {
    Region { implicit r =>
      opt(malloc, {
        val a = offheapMatrix1_5000
        val b = offheapMatrix2_5000
        val c = offheapMatrix3_5000
        val d = offheapMatrix4_5000
        val x = 7.0 * (a + (b + c))
        val y = 4.0 * c * (b + c)
        x + y
      })
    }
  }
  @Benchmark
  def breezeExampleU5000 = {
    val a = breezeMatrix1_5000
    val b = breezeMatrix2_5000
    val c = breezeMatrix3_5000
    val d = breezeMatrix4_5000
    val x = (a + b + c) * 7.0
    val y = c * (b + c) * 4.0
    x + y
  } // }}}
*/
/* }}} */

}

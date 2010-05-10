package scalala;

import org.junit.runner.RunWith;
import org.scalatest.junit.JUnitRunner;
import scala.collection.mutable.ArrayOps;

/**
 * The global profiling suite.
 *
 * @author dramage
 */

@RunWith(classOf[JUnitRunner])
class ScalalaProfilingSuite extends Scalala with ScalalaProfiling with ScalalaTest {

//  test("SparseBinaryVectorMultAdd") {
//    import tensor.dense._;
//    import tensor.sparse._;
//
//    val iter = 100;
//    val n = 50000;
//
//    val direct = profile(iter) {
//      val dv = new DenseVector(n);
//      val sv = new SparseBinaryVector(n,Array(1,20,300,4000,4500,4999));
//      for(i <- sv.activeDomain) {
//        dv(i) += sv(i) * 1.0
//      }
//    }
//
//    val operator = profile(iter) {
//      val dv = new DenseVector(n);
//      val sv = new SparseBinaryVector(n,Array(1,20,300,4000,4500,4999));
//      dv += sv * 1.0;
//    }
//
//    printf("%g %g\n", direct, operator);
//  }

  test("SparseBinaryVectorActiveDomain") {
    import tensor.sparse._;
    import collection.MergeableSet;

    val iter = 2000;
    val n = 5000;

    val direct = profile(iter) {
      val sv = new SparseBinaryVector(n,Array(1,20,300,4000,4500,4999));
      for(i<- 0 until iter)
        for(idx <- (sv.index:ArrayOps[Int]))
          0;
    }

    def activeDomain(sv:SparseBinaryVector) = new MergeableSet[Int] {
      override def size = sv.used;
      override def contains(i : Int) = sv.findOffset(i) >= 0;
      override def iterator =
        if (sv.used==sv.index.length) (sv.index:ArrayOps[Int]).iterator;
          else (sv.index:ArrayOps[Int]).view.take(sv.used).iterator;
    }

    val indirect = profile(iter) {
      val sv = new SparseBinaryVector(n,Array(1,20,300,4000,4500,4999));
      for(i<- 0 until iter)
        for(idx <- activeDomain(sv))
          0;
    }

    val default = profile(iter) {
      val sv = new SparseBinaryVector(n,Array(1,20,300,4000,4500,4999));
      for(i<- 0 until iter)
        for(idx <- sv.activeDomain)
          0;
    }

    printf("%g %g %g\n", direct, indirect, default);
  }

  test("SparseVector vs SparseHashVector") {
    import tensor.sparse._;

    val iter = 100;
    val n = 50000;

    println();

    for (m <- 100 to 5000 by 500) {
      val profileSparseVector = profile(iter) {
        val rand = new java.util.Random(m);
        val x = new SparseVector(n);
        var i = 0;
        while (i < m) {
          x(rand.nextInt(n)) = rand.nextDouble;
          i += 1;
        }
      }

      val profileInt2DoubleCounter = profile(iter) {
        val rand = new java.util.Random(m);
        val x = new SparseHashVector(n);
        var i = 0;
        while (i < m) {
          x(rand.nextInt(n)) = rand.nextDouble;
          i += 1;
        }
      }

      printf("% 5d %01.6f %01.6f\n", m, profileSparseVector, profileInt2DoubleCounter);
    }
  }
}

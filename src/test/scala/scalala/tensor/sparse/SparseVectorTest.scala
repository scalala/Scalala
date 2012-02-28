package scalala.tensor.sparse

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit._
import org.junit.runner.RunWith
import scalala.tensor.dense.DenseVector

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class SparseVectorTest extends FunSuite with ShouldMatchers  {

  val TOLERANCE = 1e-4;
  def assertClose(a : Double, b : Double) =
    assert(math.abs(a - b) < TOLERANCE, a + " vs. " +  b);

  test("MulInner") {
    val a = SparseVector(0.56390,0.36231,0.14601,0.60294,0.14535);
    val b = SparseVector(0.15951,0.83671,0.56002,0.57797,0.54450);
    val bd = DenseVector(0.15951,0.83671,0.56002,0.57797,0.54450);
    val bdSplit = DenseVector(0., 0.15951, 0., 0.83671,0., 0.56002, 0., 0.57797, 0., 0.54450);
    val bdd = bdSplit(1 to 9 by 2)
    assertClose(a dot b, .90249);
    assertClose(a dot bd, .90249);
    assertClose(bd dot a, .90249);
    assertClose(bdd dot a, .90249);
    assertClose(a.t * b, .90249);
  }

  test("Subtraction") {
    val a = SparseVector(0.56390,0.36231,0.14601,0.60294,0.14535);
    val ad = DenseVector(0.56390,0.36231,0.14601,0.60294,0.14535);
    val b = SparseVector(0.15951,0.83671,0.56002,0.57797,0.54450);
    val bd = DenseVector(0.15951,0.83671,0.56002,0.57797,0.54450);
    val bdd = bd - ad
    b -= a
    bd -= a
    assertClose(b.norm(2), bd.norm(2))
    assertClose(bdd.norm(2), bd.norm(2))
  }

}
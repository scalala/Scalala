package scalala.tensor.operators;

import scalala.ScalalaTest._;

import scalala.tensor._;
import scalala.tensor.dense._;
import OperatorSupport._;

object OperatorTest extends TestConsoleMain {
  def vector_test() = {
    val x = Vector(1,2,3);
    
    x += 2;
    assertEquals(x, Vector(3,4,5));
    
    val y = Vector(2,2,2);
    val z = Vector(.5,2,1);
    
    x -= y :* z;
    assertEquals(x, Vector(2,0,3));
    
    assertEquals(2*.5+2*2+2*1, y.t * z);
    assertEquals(2*.5+2*2+2*1, y * z.t);
    assertEquals(2*.5+2*2+2*1, z.t * y);
    assertEquals(2*.5+2*2+2*1, z * y.t);
  }
  
  def matrix_test() = {
    val A = new DenseMatrix(Array(1,2,3,4,5,6),2,3);
    val b = Vector(1,.5,1);
    
    assertEquals(Vector(1*1+.5*3+1*5, 1*2+.5*4+1*6), (A*b).value);
    assertEquals(Vector(1*1+.5*3+1*5, 1*2+.5*4+1*6), (b.t * A.t).value);
  }
  
  def implicits_test() = {
    import TensorImplicits._;
    
    val x = Array(1.0,2.0,3.0);
    x *= 2.0;
    assertEquals(x.toList, List(2.0,4.0,6.0));
    
    val y = Array(3,2,1);
    // y *= 2;      // correct compile error: doesn't promote to mutable
    x :*= y;
    assertEquals(x.toList, List(6.0, 8.0, 6.0));
  }
}

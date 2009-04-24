package scalala

object ScalalaProfiling {
  import scalala.Scalala._;
  import scalala.ScalalaTest._;

  /**
   * Runs the given code block 2*n times.  The last n
   * times are averaged to compute the average time
   * to run the code block, returned in millesconds.
   */
  def profile[E](n : Int)(func : => Unit) : Double = {
    for (i <- 0 until n) { func; }
    mean(
      for (i <- 0 until n) yield {
        val start = System.currentTimeMillis;
        func;
        System.currentTimeMillis - start;
      }
    );
  }
  
  private var tests = List[(String, ()=>Unit)]();
  protected def test(name : String)(func : => Unit) {
    tests ::= (name, func _);
  }
  
  test("SparseBinaryVectorMultAdd") {
    import scalala.tensor.dense._;
    import scalala.tensor.sparse._;
    
    val iter = 100;
    val n = 50000;
    
    val direct = profile(iter) {
      val dv = new DenseVector(n);
      val sv = new SparseBinaryVector(n,Array(1,20,300,4000,4500,4999));
      for(i <- sv.activeDomain) {
        dv(i) += sv(i) * 1.0
      }
    }
    
    val operator = profile(iter) {
      val dv = new DenseVector(n);
      val sv = new SparseBinaryVector(n,Array(1,20,300,4000,4500,4999));
      dv += sv * 1.0;
    }
    
    printf("%g %g\n", direct, operator);
  }
  
  def main(args : Array[String]) {
    for ((name,func) <- tests) {
      println(name);
      func();
    }
  }
}

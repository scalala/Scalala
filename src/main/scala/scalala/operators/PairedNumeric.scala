
package scalala;
package operators;

/** Marker trait for scalar values. @author dramage */
trait Scalar[V];

object Scalar {
  implicit object ScalarI extends Scalar[Int];
  implicit object ScalarS extends Scalar[Short];
  implicit object ScalarL extends Scalar[Long];
  implicit object scalarF extends Scalar[Float];
  implicit object scalarD extends Scalar[Double];
  implicit object scalarB extends Scalar[Boolean];
}

/** Returns the zero value for the given type. @author dramage */
trait NumericZero[@specialized(Int,Short,Long,Float,Double,Boolean) V] extends Scalar[V] {
  def value : V;
}

object NumericZero {
  implicit object NumericZeroInt extends NumericZero[Int]
  { def value = 0; }

  implicit object NumericZeroShort extends NumericZero[Short]
  { def value = 0.toShort; }

  implicit object NumericZeroLong extends NumericZero[Long]
  { def value = 0l; }

  implicit object NumericZeroFloat extends NumericZero[Float]
  { def value = 0f; }

  implicit object NumericZeroDouble extends NumericZero[Double]
  { def value = 0.0; }

  implicit object NumericZeroBoolean extends NumericZero[Boolean]
  { def value = false; }
}

trait NumericNeg[@specialized(Int,Short,Long,Float,Double,Boolean) V]
extends (V => V);

object NumericNeg {
  implicit object NumericNegInt extends NumericNeg[Int]
  { def apply(value : Int) = -value; }

  implicit object NumericNegShort extends NumericNeg[Short]
  { def apply(value : Short) = (-value).toShort; }

  implicit object NumericNegLong extends NumericNeg[Long]
  { def apply(value : Long) = -value; }

  implicit object NumericNegFloat extends NumericNeg[Float]
  { def apply(value : Float) = -value; }

  implicit object NumericNegDouble extends NumericNeg[Double]
  { def apply(value : Double) = -value; }
}

/**
 * Numeric operations on disparate value types A and B returning result
 * type X.  This formulation allows for automatically returning the
 * correct collection-level return type of operations that require
 * numeric promotion, i.e. Int + Double -> Double.
 *
 * @author dramage
 */
trait PairedNumeric
[@specialized(Int,Long,Float,Double) A,
 @specialized(Int,Long,Float,Double) B,
 @specialized(Int,Long,Float,Double) X]
extends Function2[A,B,X];

/** Computes A+B. @author dramage */
trait NumericAdd[A,B,X] extends PairedNumeric[A,B,X];

object NumericAdd {
  implicit object NumericAddII extends NumericAdd[Int,Int,Int]
  { def apply(a : Int, b : Int) = a + b; }

  implicit object NumericAddIL extends NumericAdd[Int,Long,Long]
  { def apply(a : Int, b : Long) = a + b; }

  implicit object NumericAddIF extends NumericAdd[Int,Float,Float]
  { def apply(a : Int, b : Float) = a + b; }

  implicit object NumericAddID extends NumericAdd[Int,Double,Double]
  { def apply(a : Int, b : Double) = a + b; }

  implicit object NumericAddLI extends NumericAdd[Long,Int,Long]
  { def apply(a : Long, b : Int) = a + b; }

  implicit object NumericAddLL extends NumericAdd[Long,Long,Long]
  { def apply(a : Long, b : Long) = a + b; }

  implicit object NumericAddLF extends NumericAdd[Long,Float,Double]
  { def apply(a : Long, b : Float) = a + b; }

  implicit object NumericAddLD extends NumericAdd[Long,Double,Double]
  { def apply(a : Long, b : Double) = a + b; }

  implicit object NumericAddFI extends NumericAdd[Float,Int,Float]
  { def apply(a : Float, b : Int) = a + b; }

  implicit object NumericAddFL extends NumericAdd[Float,Long,Double]
  { def apply(a : Float, b : Long) = a + b; }

  implicit object NumericAddFF extends NumericAdd[Float,Float,Float]
  { def apply(a : Float, b : Float) = a + b; }

  implicit object NumericAddFD extends NumericAdd[Float,Double,Double]
  { def apply(a : Float, b : Double) = a + b; }

  implicit object NumericAddDI extends NumericAdd[Double,Int,Double]
  { def apply(a : Double, b : Int) = a + b; }

  implicit object NumericAddDL extends NumericAdd[Double,Long,Double]
  { def apply(a : Double, b : Long) = a + b; }

  implicit object NumericAddDF extends NumericAdd[Double,Float,Double]
  { def apply(a : Double, b : Float) = a + b; }

  implicit object NumericAddDD extends NumericAdd[Double,Double,Double]
  { def apply(a : Double, b : Double) = a + b; }
}

/** Computes A-B. @author dramage */
trait NumericSub[A,B,X] extends PairedNumeric[A,B,X];

object NumericSub {
  implicit object NumericSubII extends NumericSub[Int,Int,Int]
  { def apply(a : Int, b : Int) = a - b; }

  implicit object NumericSubID extends NumericSub[Int,Double,Double]
  { def apply(a : Int, b : Double) = a - b; }

  implicit object NumericSubDI extends NumericSub[Double,Int,Double]
  { def apply(a : Double, b : Int) = a - b; }

  implicit object NumericSubDD extends NumericSub[Double,Double,Double]
  { def apply(a : Double, b : Double) = a - b; }
}

/** Computes A*B. @author dramage */
trait NumericMul[A,B,X] extends PairedNumeric[A,B,X];

object NumericMul {
  implicit object NumericMulII extends NumericMul[Int,Int,Int]
  { def apply(a : Int, b : Int) = a * b; }

  implicit object NumericMulID extends NumericMul[Int,Double,Double]
  { def apply(a : Int, b : Double) = a * b; }

  implicit object NumericMulDI extends NumericMul[Double,Int,Double]
  { def apply(a : Double, b : Int) = a * b; }

  implicit object NumericMulDD extends NumericMul[Double,Double,Double]
  { def apply(a : Double, b : Double) = a * b; }
}

/** Computes A/B. @author dramage */
trait NumericDiv[A,B,X] extends PairedNumeric[A,B,X];

object NumericDiv {
  implicit object NumericDivII extends NumericDiv[Int,Int,Int]
  { def apply(a : Int, b : Int) = a / b; }

  implicit object NumericDivID extends NumericDiv[Int,Double,Double]
  { def apply(a : Int, b : Double) = a / b; }

  implicit object NumericDivDI extends NumericDiv[Double,Int,Double]
  { def apply(a : Double, b : Int) = a / b; }

  implicit object NumericDivDD extends NumericDiv[Double,Double,Double]
  { def apply(a : Double, b : Double) = a / b; }
}

/** Computes A%B. @author dramage */
trait NumericMod[A,B,X] extends PairedNumeric[A,B,X];

object NumericMod {
  implicit object NumericModII extends NumericMod[Int,Int,Int]
  { def apply(a : Int, b : Int) = a % b; }

  implicit object NumericModID extends NumericMod[Int,Double,Double]
  { def apply(a : Int, b : Double) = a % b; }

  implicit object NumericModDI extends NumericMod[Double,Int,Double]
  { def apply(a : Double, b : Int) = a % b; }

  implicit object NumericModDD extends NumericMod[Double,Double,Double]
  { def apply(a : Double, b : Double) = a % b; }
}

/** Computes A^B (A raised to the power of B). @author dramage */
trait NumericPow[A,B,X] extends PairedNumeric[A,B,X];

object NumericPow {
  implicit object NumericPowII extends NumericMod[Int,Int,Double]
  { def apply(a : Int, b : Int) = math.pow(a,b); }

  implicit object NumericPowID extends NumericMod[Int,Double,Double]
  { def apply(a : Int, b : Double) = math.pow(a,b); }

  implicit object NumericPowDI extends NumericMod[Double,Int,Double]
  { def apply(a : Double, b : Int) = math.pow(a,b); }

  implicit object NumericPowDD extends NumericMod[Double,Double,Double]
  { def apply(a : Double, b : Double) = math.pow(a,b); }
}

/** Computes A &lt; B. @author dramage */
trait NumericLT[A,B] extends PairedNumeric[A,B,Boolean];

object NumericLT {
  implicit object NumericLTII extends NumericLT[Int,Int]
  { def apply(a : Int, b : Int) = a < b; }

  implicit object NumericLTID extends NumericLT[Int,Double]
  { def apply(a : Int, b : Double) = a < b; }

  implicit object NumericLTDI extends NumericLT[Double,Int]
  { def apply(a : Double, b : Int) = a < b; }

  implicit object NumericLTDD extends NumericLT[Double,Double]
  { def apply(a : Double, b : Double) = a < b; }
}

/** Computes A &lt;= B. @author dramage */
trait NumericLTE[A,B] extends PairedNumeric[A,B,Boolean];

object NumericLTE {
  implicit object NumericLTEII extends NumericLTE[Int,Int]
  { def apply(a : Int, b : Int) = a <= b; }

  implicit object NumericLTEID extends NumericLTE[Int,Double]
  { def apply(a : Int, b : Double) = a <= b; }

  implicit object NumericLTEDI extends NumericLTE[Double,Int]
  { def apply(a : Double, b : Int) = a <= b; }

  implicit object NumericLTEDD extends NumericLTE[Double,Double]
  { def apply(a : Double, b : Double) = a <= b; }
}

/** Computes A &gt; B. @author dramage */
trait NumericGT[A,B] extends PairedNumeric[A,B,Boolean];

object NumericGT {
  implicit object NumericGTII extends NumericGT[Int,Int]
  { def apply(a : Int, b : Int) = a > b; }

  implicit object NumericGTID extends NumericGT[Int,Double]
  { def apply(a : Int, b : Double) = a > b; }

  implicit object NumericGTDI extends NumericGT[Double,Int]
  { def apply(a : Double, b : Int) = a > b; }

  implicit object NumericGTDD extends NumericGT[Double,Double]
  { def apply(a : Double, b : Double) = a > b; }
}

/** Computes A &lt;= B. @author dramage */
trait NumericGTE[A,B] extends PairedNumeric[A,B,Boolean];

object NumericGTE {
  implicit object NumericGTEII extends NumericGTE[Int,Int]
  { def apply(a : Int, b : Int) = a >= b; }

  implicit object NumericGTEID extends NumericGTE[Int,Double]
  { def apply(a : Int, b : Double) = a >= b; }

  implicit object NumericGTEDI extends NumericGTE[Double,Int]
  { def apply(a : Double, b : Int) = a >= b; }

  implicit object NumericGTEDD extends NumericGTE[Double,Double]
  { def apply(a : Double, b : Double) = a >= b; }
}

/** Computes A == B. @author dramage */
trait NumericEq[A,B] extends PairedNumeric[A,B,Boolean];

object NumericEq {
  implicit object NumericEqII extends NumericEq[Int,Int]
  { def apply(a : Int, b : Int) = a == b; }

  implicit object NumericEqID extends NumericEq[Int,Double]
  { def apply(a : Int, b : Double) = a == b; }

  implicit object NumericEqDI extends NumericEq[Double,Int]
  { def apply(a : Double, b : Int) = a == b; }

  implicit object NumericEqDD extends NumericEq[Double,Double]
  { def apply(a : Double, b : Double) = a == b; }
}

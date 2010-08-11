
package scalala;
package operators;

/** Marker trait for scalar values. @author dramage */
trait Scalar[@specialized(Int,Short,Long,Float,Double,Boolean) V];

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

trait NumericNegation[@specialized(Int,Short,Long,Float,Double,Boolean) V]
extends (V => V);

object NumericNegation {
  implicit object NumericNegationInt extends NumericNegation[Int]
  { def apply(value : Int) = -value; }

  implicit object NumericNegationShort extends NumericNegation[Short]
  { def apply(value : Short) = (-value).toShort; }

  implicit object NumericNegationLong extends NumericNegation[Long]
  { def apply(value : Long) = -value; }

  implicit object NumericNegationFloat extends NumericNegation[Float]
  { def apply(value : Float) = -value; }

  implicit object NumericNegationDouble extends NumericNegation[Double]
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
trait NumericPlus[A,B,X] extends PairedNumeric[A,B,X];

object NumericPlus {
  implicit object NumericPlusII extends NumericPlus[Int,Int,Int]
  { def apply(a : Int, b : Int) = a + b; }

  implicit object NumericPlusIL extends NumericPlus[Int,Long,Long]
  { def apply(a : Int, b : Long) = a + b; }

  implicit object NumericPlusIF extends NumericPlus[Int,Float,Float]
  { def apply(a : Int, b : Float) = a + b; }

  implicit object NumericPlusID extends NumericPlus[Int,Double,Double]
  { def apply(a : Int, b : Double) = a + b; }

  implicit object NumericPlusLI extends NumericPlus[Long,Int,Long]
  { def apply(a : Long, b : Int) = a + b; }

  implicit object NumericPlusLL extends NumericPlus[Long,Long,Long]
  { def apply(a : Long, b : Long) = a + b; }

  implicit object NumericPlusLF extends NumericPlus[Long,Float,Double]
  { def apply(a : Long, b : Float) = a + b; }

  implicit object NumericPlusLD extends NumericPlus[Long,Double,Double]
  { def apply(a : Long, b : Double) = a + b; }

  implicit object NumericPlusFI extends NumericPlus[Float,Int,Float]
  { def apply(a : Float, b : Int) = a + b; }

  implicit object NumericPlusFL extends NumericPlus[Float,Long,Double]
  { def apply(a : Float, b : Long) = a + b; }

  implicit object NumericPlusFF extends NumericPlus[Float,Float,Float]
  { def apply(a : Float, b : Float) = a + b; }

  implicit object NumericPlusFD extends NumericPlus[Float,Double,Double]
  { def apply(a : Float, b : Double) = a + b; }

  implicit object NumericPlusDI extends NumericPlus[Double,Int,Double]
  { def apply(a : Double, b : Int) = a + b; }

  implicit object NumericPlusDL extends NumericPlus[Double,Long,Double]
  { def apply(a : Double, b : Long) = a + b; }

  implicit object NumericPlusDF extends NumericPlus[Double,Float,Double]
  { def apply(a : Double, b : Float) = a + b; }

  implicit object NumericPlusDD extends NumericPlus[Double,Double,Double]
  { def apply(a : Double, b : Double) = a + b; }
}

/** Computes A-B. @author dramage */
trait NumericMinus[A,B,X] extends PairedNumeric[A,B,X];

object NumericMinus {
  implicit object NumericMinusII extends NumericMinus[Int,Int,Int]
  { def apply(a : Int, b : Int) = a - b; }

  implicit object NumericMinusID extends NumericMinus[Int,Double,Double]
  { def apply(a : Int, b : Double) = a - b; }

  implicit object NumericMinusDI extends NumericMinus[Double,Int,Double]
  { def apply(a : Double, b : Int) = a - b; }

  implicit object NumericMinusDD extends NumericMinus[Double,Double,Double]
  { def apply(a : Double, b : Double) = a - b; }
}

/** Computes A*B. @author dramage */
trait NumericTimes[A,B,X] extends PairedNumeric[A,B,X];

object NumericTimes {
  implicit object NumericTimesII extends NumericTimes[Int,Int,Int]
  { def apply(a : Int, b : Int) = a * b; }

  implicit object NumericTimesID extends NumericTimes[Int,Double,Double]
  { def apply(a : Int, b : Double) = a * b; }

  implicit object NumericTimesDI extends NumericTimes[Double,Int,Double]
  { def apply(a : Double, b : Int) = a * b; }

  implicit object NumericTimesDD extends NumericTimes[Double,Double,Double]
  { def apply(a : Double, b : Double) = a * b; }
}

/** Computes A/B. @author dramage */
trait NumericDivide[A,B,X] extends PairedNumeric[A,B,X];

object NumericDivide {
  implicit object NumericDivideII extends NumericDivide[Int,Int,Int]
  { def apply(a : Int, b : Int) = a / b; }

  implicit object NumericDivideID extends NumericDivide[Int,Double,Double]
  { def apply(a : Int, b : Double) = a / b; }

  implicit object NumericDivideDI extends NumericDivide[Double,Int,Double]
  { def apply(a : Double, b : Int) = a / b; }

  implicit object NumericDivideDD extends NumericDivide[Double,Double,Double]
  { def apply(a : Double, b : Double) = a / b; }
}

/** Computes A%B. @author dramage */
trait NumericModulo[A,B,X] extends PairedNumeric[A,B,X];

object NumericModulo {
  implicit object NumericModuloII extends NumericModulo[Int,Int,Int]
  { def apply(a : Int, b : Int) = a % b; }

  implicit object NumericModuloID extends NumericModulo[Int,Double,Double]
  { def apply(a : Int, b : Double) = a % b; }

  implicit object NumericModuloDI extends NumericModulo[Double,Int,Double]
  { def apply(a : Double, b : Int) = a % b; }

  implicit object NumericModuloDD extends NumericModulo[Double,Double,Double]
  { def apply(a : Double, b : Double) = a % b; }
}

/** Computes A^B (A raised to the power of B). @author dramage */
trait NumericPow[A,B,X] extends PairedNumeric[A,B,X];

object NumericPow {
  implicit object NumericPowII extends NumericModulo[Int,Int,Double]
  { def apply(a : Int, b : Int) = math.pow(a,b); }

  implicit object NumericPowID extends NumericModulo[Int,Double,Double]
  { def apply(a : Int, b : Double) = math.pow(a,b); }

  implicit object NumericPowDI extends NumericModulo[Double,Int,Double]
  { def apply(a : Double, b : Int) = math.pow(a,b); }

  implicit object NumericPowDD extends NumericModulo[Double,Double,Double]
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

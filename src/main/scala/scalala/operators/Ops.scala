
package scalala;
package operators;

import scala.reflect.ClassManifest;

import collection.{DomainMap,DomainMapLike};
import collection.domain.{IterableDomain, DomainLike};
import collection.domain.{DomainException};
import collection.generic.DomainMapCanMapValuesFrom;

//
// Unary operations
//

trait UnaryOp[-A, +That] {
  def apply(value : A) : That;
}

/** Construction delegate for -A */
trait CanNegate[-A,+That] extends UnaryOp[A,That];

object CanNegate {
  implicit def canNegateArray[V](implicit m : ClassManifest[V], n : NumericNegation[V])
  : CanNegate[Array[V],Array[V]] = new CanNegateArray[V];

  class CanNegateArray[V](implicit m : ClassManifest[V], n : NumericNegation[V])
  extends CanNegate[Array[V],Array[V]] {
    def apply(value : Array[V]) = value.map(n);
  }
}

//
// Binary operations
//

/** Operation that creates That from A and B. @author dramage */
trait BinaryOp[-A, -B, +That] {
  def apply(a : A, b : B) : That;
}

/** Base class for BinaryOp on a pair of arrays. @author dramage */
class ArrayArrayOp[V1,V2,RV](implicit m : ClassManifest[RV], op : PairedNumeric[V1,V2,RV])
extends BinaryOp[Array[V1],Array[V2],Array[RV]] {
  override def apply(a : Array[V1], b : Array[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    val rv = new Array[RV](a.length);
    var i = 0;
    while (i < rv.length) {
      rv(i) = op(a(i),b(i));
      i += 1;
    }
    rv;
  }
}

/** Base class for Array<->Scalar op. */
class ArrayScalarOp[V1,V2,RV](implicit m : ClassManifest[RV], op : PairedNumeric[V1,V2,RV])
extends BinaryOp[Array[V1],V2,Array[RV]] {
  override def apply(a : Array[V1], b : V2) = {
    val rv = new Array[RV](a.length);
    var i = 0;
    while (i < rv.length) {
      rv(i) = op(a(i),b);
      i += 1;
    }
    rv;
  }
}

/** Base class for BinaryOp on a pair of scala maps. @author dramage */
class MapMapOp[K,V1,V2,RV](implicit op : PairedNumeric[V1,V2,RV])
extends BinaryOp[Map[K,V1],Map[K,V2],Map[K,RV]] {
  def apply(a : Map[K,V1], b : Map[K,V2]) =
    (a.keySet ++ b.keySet).map(k => (k,op(a(k),b(k)))).toMap;
}

/** Construction delegate for A + B. @author dramage */
trait CanAdd[-A,-B,+That] extends BinaryOp[A,B,That];

object CanAdd {

  //
  // Arrays
  //

  implicit def canAddArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericPlus[V1,V2,RV])
  = new CanAddArrayArray[V1,V2,RV];

  class CanAddArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericPlus[V1,V2,RV])
  extends ArrayArrayOp[V1,V2,RV] with CanAdd[Array[V1],Array[V2],Array[RV]];

  implicit object CanAddArrayArrayII extends CanAddArrayArray[Int,Int,Int];
  implicit object CanAddArrayArrayDD extends CanAddArrayArray[Double,Double,Double];
  implicit object CanAddArrayArrayDI extends CanAddArrayArray[Double,Int,Double];
  implicit object CanAddArrayArrayID extends CanAddArrayArray[Int,Double,Double];

  implicit def canAddArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericPlus[V1,V2,RV])
  = new CanAddArrayScalar[V1,V2,RV];

  class CanAddArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericPlus[V1,V2,RV])
  extends ArrayScalarOp[V1,V2,RV] with CanAdd[Array[V1],V2,Array[RV]];

  implicit object CanAddArrayScalarII extends CanAddArrayScalar[Int,Int,Int];
  implicit object CanAddArrayScalarDD extends CanAddArrayScalar[Double,Double,Double];
  implicit object CanAddArrayScalarDI extends CanAddArrayScalar[Double,Int,Double];
  implicit object CanAddArrayScalarID extends CanAddArrayScalar[Int,Double,Double];

  //
  // Scala Maps
  //

  implicit def canAddMap[K,V1,V2,RV](implicit op : NumericPlus[V1,V2,RV]) =
    new CanAddMap[K,V1,V2,RV];

  class CanAddMap[K,V1,V2,RV](implicit op : NumericPlus[V1,V2,RV])
  extends MapMapOp[K,V1,V2,RV] with CanAdd[Map[K,V1],Map[K,V2],Map[K,RV]];


  //
  // Domain Maps
  // 

  implicit def canAddDomainMap
  [A,B,V2,RV,That]
  (implicit op : NumericPlus[B,V2,RV], bf : DomainMapCanMapValuesFrom[DomainMap[A,B],A,B,RV,That]) =
    new CanAddDomainMap[A,B,V2,RV,That]();

  class CanAddDomainMap
  [A,B,V2,RV,That]
  (implicit op : NumericPlus[B,V2,RV], bf : DomainMapCanMapValuesFrom[DomainMap[A,B],A,B,RV,That])
  extends CanAdd[DomainMap[A,B],DomainMap[A,V2],That] {
    def apply(a : DomainMap[A,B], b : DomainMap[A,V2]) = {
      if (a.domain != b.domain) {
        throw new DomainException(this.getClass.getSimpleName + ": different domains");
      }
      a.mapValues((k:A,v:B) => op(v,b(k)));
    }
  }

}

/** Construction delegate for A - B. @author dramage */
trait CanSubtract[-A,-B,+That] extends BinaryOp[A,B,That];

object CanSubtract {
  implicit def canSubtractArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericMinus[V1,V2,RV])
  = new CanSubtractArrayArray[V1,V2,RV];

  class CanSubtractArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericMinus[V1,V2,RV])
  extends ArrayArrayOp[V1,V2,RV] with CanSubtract[Array[V1],Array[V2],Array[RV]];

  implicit object CanSubtractArrayArrayII extends CanSubtractArrayArray[Int,Int,Int];
  implicit object CanSubtractArrayArrayDD extends CanSubtractArrayArray[Double,Double,Double];
  implicit object CanSubtractArrayArrayDI extends CanSubtractArrayArray[Double,Int,Double];
  implicit object CanSubtractArrayArrayID extends CanSubtractArrayArray[Int,Double,Double];

  implicit def canSubtractArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericMinus[V1,V2,RV])
  = new CanSubtractArrayScalar[V1,V2,RV];

  class CanSubtractArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericMinus[V1,V2,RV])
  extends ArrayScalarOp[V1,V2,RV] with CanSubtract[Array[V1],V2,Array[RV]];

  implicit object CanSubtractArrayScalarII extends CanSubtractArrayScalar[Int,Int,Int];
  implicit object CanSubtractArrayScalarDD extends CanSubtractArrayScalar[Double,Double,Double];
  implicit object CanSubtractArrayScalarDI extends CanSubtractArrayScalar[Double,Int,Double];
  implicit object CanSubtractArrayScalarID extends CanSubtractArrayScalar[Int,Double,Double];
}

/** Construction delegate for A * B. @author dramage */
trait CanMultiply[-A,-B,+That] extends BinaryOp[A,B,That];

object CanMultiply {
  implicit def canMultiplyArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericTimes[V1,V2,RV])
  = new CanMultiplyArrayArray[V1,V2,RV];

  class CanMultiplyArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericTimes[V1,V2,RV])
  extends ArrayArrayOp[V1,V2,RV] with CanMultiply[Array[V1],Array[V2],Array[RV]];

  implicit object CanMultiplyArrayArrayII extends CanMultiplyArrayArray[Int,Int,Int];
  implicit object CanMultiplyArrayArrayDD extends CanMultiplyArrayArray[Double,Double,Double];
  implicit object CanMultiplyArrayArrayDI extends CanMultiplyArrayArray[Double,Int,Double];
  implicit object CanMultiplyArrayArrayID extends CanMultiplyArrayArray[Int,Double,Double];

  implicit def canMultiplyArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericTimes[V1,V2,RV])
  = new CanMultiplyArrayScalar[V1,V2,RV];

  class CanMultiplyArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericTimes[V1,V2,RV])
  extends ArrayScalarOp[V1,V2,RV] with CanMultiply[Array[V1],V2,Array[RV]];

  implicit object CanMultiplyArrayScalarII extends CanMultiplyArrayScalar[Int,Int,Int];
  implicit object CanMultiplyArrayScalarDD extends CanMultiplyArrayScalar[Double,Double,Double];
  implicit object CanMultiplyArrayScalarDI extends CanMultiplyArrayScalar[Double,Int,Double];
  implicit object CanMultiplyArrayScalarID extends CanMultiplyArrayScalar[Int,Double,Double];
}


/** Construction delegate for A / B. @author dramage */
trait CanDivide[-A,-B,+That] extends BinaryOp[A,B,That];

object CanDivide {
  implicit def canDivideArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericDivide[V1,V2,RV])
  = new CanDivideArrayArray[V1,V2,RV];

  class CanDivideArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericDivide[V1,V2,RV])
  extends ArrayArrayOp[V1,V2,RV] with CanDivide[Array[V1],Array[V2],Array[RV]];

  implicit object CanDivideArrayArrayII extends CanDivideArrayArray[Int,Int,Int];
  implicit object CanDivideArrayArrayDD extends CanDivideArrayArray[Double,Double,Double];
  implicit object CanDivideArrayArrayDI extends CanDivideArrayArray[Double,Int,Double];
  implicit object CanDivideArrayArrayID extends CanDivideArrayArray[Int,Double,Double];

  implicit def canDivideArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericDivide[V1,V2,RV])
  = new CanDivideArrayScalar[V1,V2,RV];

  class CanDivideArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericDivide[V1,V2,RV])
  extends ArrayScalarOp[V1,V2,RV] with CanDivide[Array[V1],V2,Array[RV]];

  implicit object CanDivideArrayScalarII extends CanDivideArrayScalar[Int,Int,Int];
  implicit object CanDivideArrayScalarDD extends CanDivideArrayScalar[Double,Double,Double];
  implicit object CanDivideArrayScalarDI extends CanDivideArrayScalar[Double,Int,Double];
  implicit object CanDivideArrayScalarID extends CanDivideArrayScalar[Int,Double,Double];
}

/** Construction delegate for A % B. @author dramage */
trait CanModulo[-A,-B,+That] extends BinaryOp[A,B,That];

object CanModulo {
  implicit def canModuloArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericModulo[V1,V2,RV])
  = new CanModuloArrayArray[V1,V2,RV];

  class CanModuloArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericModulo[V1,V2,RV])
  extends ArrayArrayOp[V1,V2,RV] with CanModulo[Array[V1],Array[V2],Array[RV]];

  implicit object CanModuloArrayArrayII extends CanModuloArrayArray[Int,Int,Int];
  implicit object CanModuloArrayArrayDD extends CanModuloArrayArray[Double,Double,Double];
  implicit object CanModuloArrayArrayDI extends CanModuloArrayArray[Double,Int,Double];
  implicit object CanModuloArrayArrayID extends CanModuloArrayArray[Int,Double,Double];

  implicit def canModuloArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericModulo[V1,V2,RV])
  = new CanModuloArrayScalar[V1,V2,RV];

  class CanModuloArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : NumericModulo[V1,V2,RV])
  extends ArrayScalarOp[V1,V2,RV] with CanModulo[Array[V1],V2,Array[RV]];

  implicit object CanModuloArrayScalarII extends CanModuloArrayScalar[Int,Int,Int];
  implicit object CanModuloArrayScalarDD extends CanModuloArrayScalar[Double,Double,Double];
  implicit object CanModuloArrayScalarDI extends CanModuloArrayScalar[Double,Int,Double];
  implicit object CanModuloArrayScalarID extends CanModuloArrayScalar[Int,Double,Double];
}

trait NumericCollectionOps[+This] {
  def repr : This;

  def unary_-[That](implicit op : CanNegate[This,That]) : That = op(repr);

  def :+[B,That](b : B)(implicit op : CanAdd[This,B,That]) = op(repr,b);

  def :-[B,That](b : B)(implicit op : CanSubtract[This,B,That]) = op(repr,b);

  def :*[B,That](b : B)(implicit op : CanMultiply[This,B,That]) = op(repr,b);

  def :/[B,That](b : B)(implicit op : CanDivide[This,B,That]) = op(repr,b);

  def :%[B,That](b : B)(implicit op : CanModulo[This,B,That]) = op(repr,b);

  /** Final alias for this.:+(b) */
  final def +[B,That](b : B)(implicit op : CanAdd[This,B,That]) = this.:+(b);

  /** Final alias for this.:-(b) */
  final def -[B,That](b : B)(implicit op : CanSubtract[This,B,That]) = this.:-(b);

  // TODO: add :> :>= :< :<= :==
}


/** Operation that updates A using B. @author dramage */
trait BinaryUpdateOp[-A,-B] {
  def apply(a : A, b : B) : Unit;
}

/** Base class for BinaryUpdateOp on a pair of arrays. @author dramage */
class BinaryUpdateArrayOp[V1,V2](implicit op : PairedNumeric[V1,V2,V1])
extends BinaryUpdateOp[Array[V1],Array[V2]] {
  def apply(a : Array[V1], b : Array[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    var i = 0;
    while (i < a.length) {
      a(i) = op(a(i),b(i));
      i += 1;
    }
  }
}

/** Mutation delegate for A += B. @author dramage */
trait CanAddInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanAddInto {
  implicit def canAddIntoArrayArray[V1,V2](implicit op : NumericPlus[V1,V2,V1])
  = new CanAddIntoArrayArray[V1,V2];

  class CanAddIntoArrayArray[V1,V2](implicit op : NumericPlus[V1,V2,V1])
  extends BinaryUpdateArrayOp[V1,V2] with CanAddInto[Array[V1],Array[V2]];

  implicit object CanAddIntoArrayArrayII extends CanAddIntoArrayArray[Int,Int];
  implicit object CanAddIntoArrayArrayDD extends CanAddIntoArrayArray[Double,Double];
  implicit object CanAddIntoArrayArrayDI extends CanAddIntoArrayArray[Double,Int];
}

/** Mutation delegate for A -= B. @author dramage */
trait CanSubtractInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanSubtractInto {
  implicit def canSubtractIntoArrayArray[V1,V2](implicit op : NumericMinus[V1,V2,V1])
  = new CanSubtractIntoArrayArray[V1,V2];

  class CanSubtractIntoArrayArray[V1,V2](implicit op : NumericMinus[V1,V2,V1])
  extends BinaryUpdateArrayOp[V1,V2] with CanSubtractInto[Array[V1],Array[V2]];

  implicit object CanSubtractIntoArrayArrayII extends CanSubtractIntoArrayArray[Int,Int];
  implicit object CanSubtractIntoArrayArrayDD extends CanSubtractIntoArrayArray[Double,Double];
  implicit object CanSubtractIntoArrayArrayDI extends CanSubtractIntoArrayArray[Double,Int];
}

trait MutableNumericCollectionOps[+This] extends NumericCollectionOps[This] {
  def repr : This;

  def :+=[B,That](b : B)(implicit op : CanAddInto[This,B]) = op(repr,b);

  def :-=[B,That](b : B)(implicit op : CanSubtractInto[This,B]) = op(repr,b);

  /** Final alias for this.:+=(b) */
  final def +=[B,That](b : B)(implicit op : CanAddInto[This,B]) = this.:+=(b);

  /** Final alias for this.:-=(b) */
  final def -=[B,That](b : B)(implicit op : CanSubtractInto[This,B]) = this.:-=(b);

  // TODO: Add :*= :/= :%= :^= :=
}

//
// Shaped operations
// 

/** Construction delegate for outer (dot) product A * B. @author dramage */
trait CanMultiplyOuter[-A,-B,+That] extends BinaryOp[A,B,That];

object CanMultiplyOuter {
  implicit def canMultiplyOuterArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], mul : NumericTimes[V1,V2,RV])
  = new CanMultiplyOuterArrayArray[V1,V2,RV];

  class CanMultiplyOuterArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], mul : NumericTimes[V1,V2,RV])
  extends CanMultiplyOuter[Array[V1],Array[V2],Array[Array[RV]]] {
    override def apply(a : Array[V1], b : Array[V2]) = {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      Array.tabulate(a.length,a.length)((i,j) => mul(a(i),b(j)));
    }
  }

  implicit object CanMultiplyOuterArrayArrayII extends CanMultiplyOuterArrayArray[Int,Int,Int];
  implicit object CanMultiplyOuterArrayArrayDD extends CanMultiplyOuterArrayArray[Double,Double,Double];
  implicit object CanMultiplyOuterArrayArrayDI extends CanMultiplyOuterArrayArray[Double,Int,Double];
  implicit object CanMultiplyOuterArrayArrayID extends CanMultiplyOuterArrayArray[Int,Double,Double];
}

/** For A*B where A is a matrix. @author dramage */
trait CanMultiplyMatrixBy[-A,-B,+That] extends BinaryOp[A,B,That];

object CanMultiplyMatrixBy {
  implicit def canMultiplyArrayMatrixByArray[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : NumericTimes[V1,V2,RV],
   add : NumericPlus[RV,RV,RV], zero : NumericZero[RV]) =
     new CanMultiplyArrayMatrixByArray[V1,V2,RV];

  class CanMultiplyArrayMatrixByArray[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : NumericTimes[V1,V2,RV],
   add : NumericPlus[RV,RV,RV], zero : NumericZero[RV])
  extends CanMultiplyMatrixBy[Array[Array[V1]],Array[V2],Array[RV]] {
    override def apply(a : Array[Array[V1]], b : Array[V2]) = {
      val rv = Array.fill(a.length)(zero.value);
      var i = 0;
      while (i < rv.length) {
        val row = a(i);
        if (row.length != b.length) {
          throw new DomainException(this.getClass.getSimpleName + ": row "+i+" mismatched length: "+row.length+ " vs "+b.length+" entries");
        }
        var j = 0;
        while (j < b.length) {
          rv(i) = add(rv(i), mul(row(j), b(j)));
          j += 1;
        }
        i += 1;
      }
      rv;
    }
  }

  implicit object CanMultiplyArrayMatrixByArrayII extends CanMultiplyArrayMatrixByArray[Int,Int,Int];
  implicit object CanMultiplyArrayMatrixByArrayDD extends CanMultiplyArrayMatrixByArray[Double,Double,Double];
  implicit object CanMultiplyArrayMatrixByArrayDI extends CanMultiplyArrayMatrixByArray[Double,Int,Double];
  implicit object CanMultiplyArrayMatrixByArrayID extends CanMultiplyArrayMatrixByArray[Int,Double,Double];

  
  implicit def canMultiplyArrayMatrixByArrayMatrix[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : NumericTimes[V1,V2,RV],
   add : NumericPlus[RV,RV,RV], zero : NumericZero[RV]) =
     new CanMultiplyArrayMatrixByArrayMatrix[V1,V2,RV];

  class CanMultiplyArrayMatrixByArrayMatrix[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : NumericTimes[V1,V2,RV],
   add : NumericPlus[RV,RV,RV], zero : NumericZero[RV])
  extends CanMultiplyMatrixBy[Array[Array[V1]],Array[Array[V2]],Array[Array[RV]]] {
    override def apply(a : Array[Array[V1]], b : Array[Array[V2]]) = {
      val numRows = a.length;
      val numCols = b(0).length;
      val numInner = b.length;

      Array.tabulate(numRows, numCols){(i,j) =>
        var rv = zero.value;
        var k = 0;
        while (k < numInner) {
          rv = add(rv, mul(a(i)(k),b(k)(j)));
          k += 1;
        }
        rv;
      }
    }
  }

  implicit object CanMultiplyArrayMatrixByArrayMatrixII extends CanMultiplyArrayMatrixByArrayMatrix[Int,Int,Int];
  implicit object CanMultiplyArrayMatrixByArrayMatrixDD extends CanMultiplyArrayMatrixByArrayMatrix[Double,Double,Double];
  implicit object CanMultiplyArrayMatrixByArrayMatrixDI extends CanMultiplyArrayMatrixByArrayMatrix[Double,Int,Double];
  implicit object CanMultiplyArrayMatrixByArrayMatrixID extends CanMultiplyArrayMatrixByArrayMatrix[Int,Double,Double];
}

/**
 * Secialized NumericCollectionOps with shaped operations taking A is a column.
 * Note that columns are the default shape, so this trait simply extends
 * NumericCollectionOps[A].
 *
 * @author dramage
 */
trait ColumnTensorOps[+A] extends NumericCollectionOps[A] {
  def *[B,RV](b : RowTensorOps[B])(implicit op : CanMultiplyOuter[A,B,RV]) : RV =
    op(repr,b.column);

  def t : RowTensorOps[A] = RowTensorOps(repr);
}

/**
 * A column tensor whose underyling collection is mutable.  This trait
 * should be used instead of mixing in ColumNTensorOps with MutableNumeriCollectionOps
 * directly, because .t needs to return an instance of MutableRowTensorOps
 * insetad of RowTensorOps.
 *
 * @author dramage
 */
trait MutableColumnTensorOps[+A] extends ColumnTensorOps[A] with MutableNumericCollectionOps[A] {
  override def t : MutableRowTensorOps[A] = MutableRowTensorOps(repr);
}


/** For A*B where A is a matrix. @author dramage */
trait CanMultiplyRowBy[-A,-B,+That] extends BinaryOp[A,B,That];

object CanMultiplyRowBy {
  implicit def canMultiplyRowArrayByArray[V1,V2,RV](implicit mul : NumericTimes[V1,V2,RV], add : NumericPlus[RV,RV,RV], zero : NumericZero[RV])
  = new CanMultiplyRowArrayByArray[V1,V2,RV];

  /** Array inner product */
  class CanMultiplyRowArrayByArray[V1,V2,RV](implicit mul : NumericTimes[V1,V2,RV], add : NumericPlus[RV,RV,RV], zero : NumericZero[RV])
  extends CanMultiplyRowBy[Array[V1],Array[V2],RV] {
    override def apply(a : Array[V1], b : Array[V2]) = {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      var rv = zero.value;
      var i = 0;
      while (i < a.length) {
        rv = add(rv, mul(a(i),b(i)));
        i += 1;
      }
      rv;
    }
  }

  implicit object CanMultiplyRowArrayByArrayII extends CanMultiplyRowArrayByArray[Int,Int,Int];
  implicit object CanMultiplyRowArrayByArrayDD extends CanMultiplyRowArrayByArray[Double,Double,Double];
  implicit object CanMultiplyRowArrayByArrayDI extends CanMultiplyRowArrayByArray[Double,Int,Double];
  implicit object CanMultiplyRowArrayByArrayID extends CanMultiplyRowArrayByArray[Int,Double,Double];

  implicit def canMultiplyRowArrayByArrayMatrix[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : NumericTimes[V1,V2,RV],
   add : NumericPlus[RV,RV,RV], zero : NumericZero[RV]) =
     new CanMultiplyRowArrayByArrayMatrix[V1,V2,RV];

  /** Array matrix by array matrix */
  class CanMultiplyRowArrayByArrayMatrix[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : NumericTimes[V1,V2,RV],
   add : NumericPlus[RV,RV,RV], zero : NumericZero[RV])
  extends CanMultiplyRowBy[Array[V1],Array[Array[V2]],MutableRowTensorOps[Array[RV]]] {
    override def apply(a : Array[V1], b : Array[Array[V2]]) = {
      val rv = Array.fill(b.length)(zero.value);
      var i = 0;
      while (i < rv.length) {
        var j = 0;
        while (j < a.length) {
          rv(i) = add(rv(i), mul(a(j), b(j)(i)));
          j += 1;
        }
        i += 1;
      }
      MutableRowTensorOps(rv);
    }
  }

  implicit object CanMultiplyRowArrayByArrayMatrixII extends CanMultiplyRowArrayByArrayMatrix[Int,Int,Int];
  implicit object CanMultiplyRowArrayByArrayMatrixDD extends CanMultiplyRowArrayByArrayMatrix[Double,Double,Double];
  implicit object CanMultiplyRowArrayByArrayMatrixDI extends CanMultiplyRowArrayByArrayMatrix[Double,Int,Double];
  implicit object CanMultiplyRowArrayByArrayMatrixID extends CanMultiplyRowArrayByArrayMatrix[Int,Double,Double];
}

/*
 * Secialized NumericCollectionOps with shaped operations taking A is a row.
 * Note that there is an inherent asymmetry between ColumnTensorOps and
 * RowTensorOps: because tensors are assumed to be columns until reshaped
 * (e.g. by calling .t), that class extends NumericCollectionOps[A].  This
 * class, by contrast, must preserve the fact that the base numeric operations
 * like plus must honor the row shape, and that the return result should also
 * be a row.  Hence this class extends NumericCollectionOps[RowTensorOps[A]]
 * and provides implicit magic in the companion object to wrap the
 * corresponding construction delegates.
 *
 * @author dramage
 */
trait RowTensorOps[+A] extends NumericCollectionOps[RowTensorOps[A]] {
  override def repr : RowTensorOps[A] = this;

  def column : A;

  def *[B,RV](b : B)(implicit op : CanMultiplyRowBy[A,B,RV]) : RV =
    op(this.column,b);

  /** The transpose returns the underlying value, which assumed to be a column. */
  def t : A = column;
}

object RowTensorOps {
  def apply[This](v : This) : RowTensorOps[This] =
    new RowTensorOps[This] { override def column = v; }

  class RowBinaryOp[-A,-B,+That](implicit op : BinaryOp[A,B,That])
  extends BinaryOp[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]] {
    override def apply(a : RowTensorOps[A], b : RowTensorOps[B]) =
      RowTensorOps(op(a.column,b.column));
  }

  implicit def canAddRows[A,B,That](implicit op : CanAdd[A,B,That])
  = new RowBinaryOp[A,B,That] with CanAdd[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def canSubtractRows[A,B,That](implicit op : CanSubtract[A,B,That])
  = new RowBinaryOp[A,B,That] with CanSubtract[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def canMultiplyRows[A,B,That](implicit op : CanMultiply[A,B,That])
  = new RowBinaryOp[A,B,That] with CanMultiply[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def canDivideRows[A,B,That](implicit op : CanDivide[A,B,That])
  = new RowBinaryOp[A,B,That] with CanDivide[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def canModuloRows[A,B,That](implicit op : CanModulo[A,B,That])
  = new RowBinaryOp[A,B,That] with CanModulo[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  // TODO: add remaining wrapped conversions
}

/**
 * Specialized RowTensorOps support for RowTensors that have mutable
 * underlying collections.
 */
trait MutableRowTensorOps[+A]
extends RowTensorOps[A] with MutableNumericCollectionOps[RowTensorOps[A]] {
}

object MutableRowTensorOps {
  def apply[This](v : This) : MutableRowTensorOps[This] =
    new MutableRowTensorOps[This] { override def column = v; }

  // TODO: add all wrapped conversions
}

/**
 * Provides matrix-like operations for two dimensional collections.
 * @author dramage
 */
trait MatrixOps[+A] extends NumericCollectionOps[A] {
  def *[B,That](b : B)(implicit op : CanMultiplyMatrixBy[A,B,That]) =
    op.apply(repr,b);
}

// TODO: default DomainMap implementation of all operations should
// use reflection to find a more specialized runtime method.

//
// Enriched types
// 

/** Numeric operator support for numeric arrays. @author dramage */
class RichNumericArrayVector[V:ClassManifest](override val repr : Array[V])
extends MutableColumnTensorOps[Array[V]];

/** Numeric operator support for Array[Array] matrix. @author dramage */
class RichNumericArrayMatrix[V:ClassManifest](override val repr : Array[Array[V]])
extends MatrixOps[Array[Array[V]]];

/** Numeric operator support for solo scalars. @author dramage */
class RichScalar[@specialized(Int,Long,Float,Double) A](val scalar : A) {
  /** Commutative: defer to scalar + b. */
  def :+[B,That](b : B)(implicit op : CanAdd[B,A,That]) = op(b,scalar);

  /** Not commutative: need special implementation of scalar - b. */
  def :-[B,That](b : B)(implicit op : CanSubtract[A,B,That]) = op(scalar,b);

  /** Commutative: defer to scalar * b. */
  def :*[B,That](b : B)(implicit op : CanMultiply[B,A,That]) = op(b,scalar);

  /** Not commutative: need special implementation of scala / b. */
  def :/[B,That](b : B)(implicit op : CanDivide[A,B,That]) = op(scalar,b);

  /** Not commutative: need special implementation of scala % b. */
  def :%[B,That](b : B)(implicit op : CanModulo[A,B,That]) = op(scalar,b);

  /** Final alias for this.:+(b) */
  final def +[B,That](b : B)(implicit op : CanAdd[B,A,That]) = this.:+(b);

  /** Final alias for this.:-(b) */
  final def -[B,That](b : B)(implicit op : CanSubtract[A,B,That]) = this.:-(b);
}

/** Numeric operator support for scala maps. @athor dramage */
class RichNumericMap[K,V](override val repr : Map[K,V])
extends NumericCollectionOps[Map[K,V]];

class RichNumericDomainMap[A,V,M<:DomainMap[A,V]]
(override val repr : M)
extends NumericCollectionOps[M];

object Implicits {

  implicit def richScalar[@specialized(Int,Long,Float,Double) V](value : V) =
    new RichScalar(value);

  implicit def richNumericArrayVector[V:ClassManifest](value : Array[V]) =
    new RichNumericArrayVector(value);

  implicit def richNumericArrayMatrix[V:ClassManifest](value : Array[Array[V]]) =
    new RichNumericArrayMatrix(value);

  implicit def richNumericMap[K,V](value : Map[K,V]) =
    new RichNumericMap[K,V](value);

  implicit def richNumericDomainMap[A,V](value : DomainMap[A,V]) =
    new RichNumericDomainMap[A,V,DomainMap[A,V]](value);

  def main(args : Array[String]) {
    val x = Array(1,2,3,4);
    val y = Array(-2,-3,-4,-5);
    println(x mkString(" "));
    println((-x) mkString(" "));
    println((x + 1) mkString(" "));
    println((1 + x) mkString(" "));
    println((x + y) mkString(" "));
    println((x - y) mkString(" "));

    x :+= y;  println(x mkString(" "));
    x :-= y;  println(x mkString(" "));

    println((Array(1.0,2.0,3.0) :/ Array(2,2,2)) mkString(" "));
    println((Array(1,2,3) :/ Array(.5,.5,.5)) mkString(" "));
    println((Array(1.0,2.0,3.0) :* Array(2,2,2)) mkString(" "));
    println((Array(1,2,3) :* Array(.5,.5,.5)) mkString(" "));
    println((Array(1.0,2.0,3.0) :% Array(2,2,2)) mkString(" "));
    println((Array(1,2,3) :% Array(.5,.5,.5)) mkString(" "));

    println(Map("a"->1,"b"->2) + Map("a"->1,"b"->2));

    println(DomainMap("a"->1,"b"->2,"c"->3) + DomainMap("a"->1,"b"->2,"c"->3));

    println(x.t :+ y.t);
    println(x.t * y);

    println((x * y.t).map(_.mkString(" ")).mkString("\n"));

    val m : Array[Array[Int]] = x * y.t;
    println((m * Array(3,2,1,0)) mkString(" "));

    val a = Array(Array(1,2,3),Array(3,0,4));
    val b = Array(Array(-3,7),Array(6,1),Array(2,2));
    println((a * b).map(_.mkString(" ")).mkString("\n"));
  }
}

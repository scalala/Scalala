/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */

package scalala;
package operators;

import scala.reflect.ClassManifest;

import collection.DomainMap;
import collection.domain.{DomainException};
import collection.generic.DomainMapCanMapValuesFrom;

//
// Basic data creation
//

///**
// * Construction delegate for creating a new "0" valued numeric collection of
// * type That from a template instance of type A.
// */
//trait CanCreateZeroFrom[-A, +That] {
//  def apply(value : A) : That;
//}

//
// Unary operations
//

trait UnaryOp[@specialized(Int,Short,Long,Float,Double) -A, +That]
extends (A => That);

/** Construction delegate for -A */
trait CanNeg[-A, +That]
extends UnaryOp[A,That];

object CanNeg {
  //
  // Base numeric types
  //

  implicit object CanNegI extends CanNeg[Int,Int]
  { def apply(value : Int) = -value; }

  implicit object CanNegS extends CanNeg[Short,Short]
  { def apply(value : Short) = (-value).toShort; }

  implicit object CanNegL extends CanNeg[Long,Long]
  { def apply(value : Long) = -value; }

  implicit object CanNegF extends CanNeg[Float,Float]
  { def apply(value : Float) = -value; }

  implicit object CanNegD extends CanNeg[Double,Double]
  { def apply(value : Double) = -value; }

  //
  // Built-in tuple types
  //

  implicit def CanNegTuple2[A,RA,B,RB](nA : CanNeg[A,RA], nB : CanNeg[B,RB])
  : CanNeg[(A,B),(RA,RB)] = new CanNeg[(A,B),(RA,RB)] {
    def apply(value : (A,B)) = (nA(value._1), nB(value._2));
  }

  implicit def CanNegTuple3[A,RA,B,RB,C,RC](nA : CanNeg[A,RA], nB : CanNeg[B,RB], nC : CanNeg[C,RC])
  : CanNeg[(A,B,C),(RA,RB,RC)] = new CanNeg[(A,B,C),(RA,RB,RC)] {
    def apply(value : (A,B,C)) = (nA(value._1), nB(value._2), nC(value._3));
  }

  //
  // Built-in collection types
  // 

  implicit def CanNegArray[V,RV](implicit m : ClassManifest[RV], n : CanNeg[V,RV])
  : CanNeg[Array[V],Array[RV]] = new CanNegArray[V,RV];

  class CanNegArray[V,RV](implicit m : ClassManifest[RV], n : CanNeg[V,RV])
  extends CanNeg[Array[V],Array[RV]] {
    def apply(value : Array[V]) = value.map(n);
  }

  implicit object CanNegArrayI extends CanNegArray[Int,Int];
  implicit object CanNegArrayS extends CanNegArray[Short,Short];
  implicit object CanNegArrayL extends CanNegArray[Long,Long];
  implicit object CanNegArrayF extends CanNegArray[Float,Float];
  implicit object CanNegArrayD extends CanNegArray[Double,Double];
}

//
// Binary operations
//

/** Operation that creates That from A and B. @author dramage */
trait BinaryOp[-A, -B, +That] {
  def apply(a : A, b : B) : That;
}

/** Base class for BinaryOp on a pair of arrays. @author dramage */
class ArrayArrayOp[V1,V2,RV](implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV])
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

/** Base class for Array (op) Scalar. */
class ArrayScalarOp[V1,V2,RV]
(implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], s : Scalar[V2])
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

/** Base class for Scalar (op) Array. */
class ScalarArrayOp[V1,V2,RV]
(implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], s : Scalar[V1])
extends BinaryOp[V1,Array[V2],Array[RV]] {
  override def apply(a : V1, b : Array[V2]) = {
    val rv = new Array[RV](b.length);
    var i = 0;
    while (i < rv.length) {
      rv(i) = op(a,b(i));
      i += 1;
    }
    rv;
  }
}

/** Base class for BinaryOp on a pair of scala maps. @author dramage */
class MapMapOp[K,V1,V2,RV](implicit op : BinaryOp[V1,V2,RV])
extends BinaryOp[Map[K,V1],Map[K,V2],Map[K,RV]] {
  def apply(a : Map[K,V1], b : Map[K,V2]) =
    (a.keySet ++ b.keySet).map(k => (k,op(a(k),b(k)))).toMap;
}


/** Construction delegate for A :+ B. @author dramage */
trait CanAdd[-A,-B,+That] extends BinaryOp[A,B,That];

object CanAdd {
  //
  // Primitives
  //

  implicit object CanAddII extends CanAdd[Int,Int,Int]
  { def apply(a : Int, b : Int) = a + b; }

  implicit object CanAddIL extends CanAdd[Int,Long,Long]
  { def apply(a : Int, b : Long) = a + b; }

  implicit object CanAddIF extends CanAdd[Int,Float,Float]
  { def apply(a : Int, b : Float) = a + b; }

  implicit object CanAddID extends CanAdd[Int,Double,Double]
  { def apply(a : Int, b : Double) = a + b; }

  implicit object CanAddLI extends CanAdd[Long,Int,Long]
  { def apply(a : Long, b : Int) = a + b; }

  implicit object CanAddLL extends CanAdd[Long,Long,Long]
  { def apply(a : Long, b : Long) = a + b; }

  implicit object CanAddLF extends CanAdd[Long,Float,Double]
  { def apply(a : Long, b : Float) = a + b; }

  implicit object CanAddLD extends CanAdd[Long,Double,Double]
  { def apply(a : Long, b : Double) = a + b; }

  implicit object CanAddFI extends CanAdd[Float,Int,Float]
  { def apply(a : Float, b : Int) = a + b; }

  implicit object CanAddFL extends CanAdd[Float,Long,Double]
  { def apply(a : Float, b : Long) = a + b; }

  implicit object CanAddFF extends CanAdd[Float,Float,Float]
  { def apply(a : Float, b : Float) = a + b; }

  implicit object CanAddFD extends CanAdd[Float,Double,Double]
  { def apply(a : Float, b : Double) = a + b; }

  implicit object CanAddDI extends CanAdd[Double,Int,Double]
  { def apply(a : Double, b : Int) = a + b; }

  implicit object CanAddDL extends CanAdd[Double,Long,Double]
  { def apply(a : Double, b : Long) = a + b; }

  implicit object CanAddDF extends CanAdd[Double,Float,Double]
  { def apply(a : Double, b : Float) = a + b; }

  implicit object CanAddDD extends CanAdd[Double,Double,Double]
  { def apply(a : Double, b : Double) = a + b; }

  //
  // Arrays
  //

  implicit def canAddArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanAdd[V1,V2,RV])
  = new CanAddArrayArray[V1,V2,RV];

  class CanAddArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanAdd[V1,V2,RV])
  extends ArrayArrayOp[V1,V2,RV] with CanAdd[Array[V1],Array[V2],Array[RV]];

  implicit object CanAddArrayArrayII extends CanAddArrayArray[Int,Int,Int];
  implicit object CanAddArrayArrayDD extends CanAddArrayArray[Double,Double,Double];
  implicit object CanAddArrayArrayDI extends CanAddArrayArray[Double,Int,Double];
  implicit object CanAddArrayArrayID extends CanAddArrayArray[Int,Double,Double];

  implicit def canAddArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : CanAdd[V1,V2,RV], s : Scalar[V2])
  = new CanAddArrayScalar[V1,V2,RV];

  class CanAddArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : CanAdd[V1,V2,RV], s : Scalar[V2])
  extends ArrayScalarOp[V1,V2,RV] with CanAdd[Array[V1],V2,Array[RV]];

  implicit object CanAddArrayScalarII extends CanAddArrayScalar[Int,Int,Int];
  implicit object CanAddArrayScalarDD extends CanAddArrayScalar[Double,Double,Double];
  implicit object CanAddArrayScalarDI extends CanAddArrayScalar[Double,Int,Double];
  implicit object CanAddArrayScalarID extends CanAddArrayScalar[Int,Double,Double];

  //
  // Scala Maps
  //

  implicit def canAddMap[K,V1,V2,RV](implicit op : CanAdd[V1,V2,RV]) =
    new CanAddMap[K,V1,V2,RV];

  class CanAddMap[K,V1,V2,RV](implicit op : CanAdd[V1,V2,RV])
  extends MapMapOp[K,V1,V2,RV] with CanAdd[Map[K,V1],Map[K,V2],Map[K,RV]];


//  //
//  // Domain Maps
//  //
//
//  implicit def canAddDomainMap
//  [A,B,V2,RV,That]
//  (implicit op : NumericAdd[B,V2,RV], bf : DomainMapCanMapValuesFrom[DomainMap[A,B],A,B,RV,That]) =
//    new CanAddDomainMap[A,B,V2,RV,That]();
//
//  class CanAddDomainMap
//  [A,B,V2,RV,That]
//  (implicit op : NumericAdd[B,V2,RV], bf : DomainMapCanMapValuesFrom[DomainMap[A,B],A,B,RV,That])
//  extends CanAdd[DomainMap[A,B],DomainMap[A,V2],That] {
//    def apply(a : DomainMap[A,B], b : DomainMap[A,V2]) = {
//      if (a.domain != b.domain) {
//        throw new DomainException(this.getClass.getSimpleName + ": different domains");
//      }
//      a.mapValues((k:A,v:B) => op(v,b(k)));
//    }
//  }
}

/** Construction delegate for A :- B. @author dramage */
trait CanSub[-A,-B,+That] extends BinaryOp[A,B,That];

object CanSub {
  //
  // Primitives
  //

  implicit object CanSubII extends CanSub[Int,Int,Int]
  { def apply(a : Int, b : Int) = a - b; }

  implicit object CanSubIL extends CanSub[Int,Long,Long]
  { def apply(a : Int, b : Long) = a - b; }

  implicit object CanSubIF extends CanSub[Int,Float,Float]
  { def apply(a : Int, b : Float) = a - b; }

  implicit object CanSubID extends CanSub[Int,Double,Double]
  { def apply(a : Int, b : Double) = a - b; }

  implicit object CanSubLI extends CanSub[Long,Int,Long]
  { def apply(a : Long, b : Int) = a - b; }

  implicit object CanSubLL extends CanSub[Long,Long,Long]
  { def apply(a : Long, b : Long) = a - b; }

  implicit object CanSubLF extends CanSub[Long,Float,Double]
  { def apply(a : Long, b : Float) = a - b; }

  implicit object CanSubLD extends CanSub[Long,Double,Double]
  { def apply(a : Long, b : Double) = a - b; }

  implicit object CanSubFI extends CanSub[Float,Int,Float]
  { def apply(a : Float, b : Int) = a - b; }

  implicit object CanSubFL extends CanSub[Float,Long,Double]
  { def apply(a : Float, b : Long) = a - b; }

  implicit object CanSubFF extends CanSub[Float,Float,Float]
  { def apply(a : Float, b : Float) = a - b; }

  implicit object CanSubFD extends CanSub[Float,Double,Double]
  { def apply(a : Float, b : Double) = a - b; }

  implicit object CanSubDI extends CanSub[Double,Int,Double]
  { def apply(a : Double, b : Int) = a - b; }

  implicit object CanSubDL extends CanSub[Double,Long,Double]
  { def apply(a : Double, b : Long) = a - b; }

  implicit object CanSubDF extends CanSub[Double,Float,Double]
  { def apply(a : Double, b : Float) = a - b; }

  implicit object CanSubDD extends CanSub[Double,Double,Double]
  { def apply(a : Double, b : Double) = a - b; }

  //
  // Arrays
  //

  implicit def CanSubArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanSub[V1,V2,RV])
  = new CanSubArrayArray[V1,V2,RV];

  class CanSubArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanSub[V1,V2,RV])
  extends ArrayArrayOp[V1,V2,RV] with CanSub[Array[V1],Array[V2],Array[RV]];

  implicit object CanSubArrayArrayII extends CanSubArrayArray[Int,Int,Int];
  implicit object CanSubArrayArrayDD extends CanSubArrayArray[Double,Double,Double];
  implicit object CanSubArrayArrayDI extends CanSubArrayArray[Double,Int,Double];
  implicit object CanSubArrayArrayID extends CanSubArrayArray[Int,Double,Double];

  implicit def CanSubArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : CanSub[V1,V2,RV], s : Scalar[V2])
  = new CanSubArrayScalar[V1,V2,RV];

  class CanSubArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : CanSub[V1,V2,RV], s : Scalar[V2])
  extends ArrayScalarOp[V1,V2,RV] with CanSub[Array[V1],V2,Array[RV]];

  implicit object CanSubArrayScalarII extends CanSubArrayScalar[Int,Int,Int];
  implicit object CanSubArrayScalarDD extends CanSubArrayScalar[Double,Double,Double];
  implicit object CanSubArrayScalarDI extends CanSubArrayScalar[Double,Int,Double];
  implicit object CanSubArrayScalarID extends CanSubArrayScalar[Int,Double,Double];

  implicit def mkScalarArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanSub[V1,V2,RV], s : Scalar[V1])
  = new ScalarArray[V1,V2,RV];

  class ScalarArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanSub[V1,V2,RV], s : Scalar[V1])
  extends ScalarArrayOp[V1,V2,RV] with CanSub[V1,Array[V2],Array[RV]];

  implicit object ScalarArrayII extends ScalarArray[Int,Int,Int];
  implicit object ScalarArrayDD extends ScalarArray[Double,Double,Double];
  implicit object ScalarArrayDI extends ScalarArray[Double,Int,Double];
  implicit object ScalarArrayID extends ScalarArray[Int,Double,Double];
}

/** Construction delegate for A :* B. @author dramage */
trait CanMul[-A,-B,+That] extends BinaryOp[A,B,That];

object CanMul {
  //
  // Primitives
  //

  implicit object CanMulII extends CanMul[Int,Int,Int]
  { def apply(a : Int, b : Int) = a * b; }

  implicit object CanMulIL extends CanMul[Int,Long,Long]
  { def apply(a : Int, b : Long) = a * b; }

  implicit object CanMulIF extends CanMul[Int,Float,Float]
  { def apply(a : Int, b : Float) = a * b; }

  implicit object CanMulID extends CanMul[Int,Double,Double]
  { def apply(a : Int, b : Double) = a * b; }

  implicit object CanMulLI extends CanMul[Long,Int,Long]
  { def apply(a : Long, b : Int) = a * b; }

  implicit object CanMulLL extends CanMul[Long,Long,Long]
  { def apply(a : Long, b : Long) = a * b; }

  implicit object CanMulLF extends CanMul[Long,Float,Double]
  { def apply(a : Long, b : Float) = a * b; }

  implicit object CanMulLD extends CanMul[Long,Double,Double]
  { def apply(a : Long, b : Double) = a * b; }

  implicit object CanMulFI extends CanMul[Float,Int,Float]
  { def apply(a : Float, b : Int) = a * b; }

  implicit object CanMulFL extends CanMul[Float,Long,Double]
  { def apply(a : Float, b : Long) = a * b; }

  implicit object CanMulFF extends CanMul[Float,Float,Float]
  { def apply(a : Float, b : Float) = a * b; }

  implicit object CanMulFD extends CanMul[Float,Double,Double]
  { def apply(a : Float, b : Double) = a * b; }

  implicit object CanMulDI extends CanMul[Double,Int,Double]
  { def apply(a : Double, b : Int) = a * b; }

  implicit object CanMulDL extends CanMul[Double,Long,Double]
  { def apply(a : Double, b : Long) = a * b; }

  implicit object CanMulDF extends CanMul[Double,Float,Double]
  { def apply(a : Double, b : Float) = a * b; }

  implicit object CanMulDD extends CanMul[Double,Double,Double]
  { def apply(a : Double, b : Double) = a * b; }

  //
  // Arrays
  //

  implicit def CanMulArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanMul[V1,V2,RV])
  = new CanMulArrayArray[V1,V2,RV];

  class CanMulArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanMul[V1,V2,RV])
  extends ArrayArrayOp[V1,V2,RV] with CanMul[Array[V1],Array[V2],Array[RV]];

  implicit object CanMulArrayArrayII extends CanMulArrayArray[Int,Int,Int];
  implicit object CanMulArrayArrayDD extends CanMulArrayArray[Double,Double,Double];
  implicit object CanMulArrayArrayDI extends CanMulArrayArray[Double,Int,Double];
  implicit object CanMulArrayArrayID extends CanMulArrayArray[Int,Double,Double];

  implicit def CanMulArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : CanMul[V1,V2,RV], s : Scalar[V2])
  = new CanMulArrayScalar[V1,V2,RV];

  class CanMulArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : CanMul[V1,V2,RV], s : Scalar[V2])
  extends ArrayScalarOp[V1,V2,RV] with CanMul[Array[V1],V2,Array[RV]];

  implicit object CanMulArrayScalarII extends CanMulArrayScalar[Int,Int,Int];
  implicit object CanMulArrayScalarDD extends CanMulArrayScalar[Double,Double,Double];
  implicit object CanMulArrayScalarDI extends CanMulArrayScalar[Double,Int,Double];
  implicit object CanMulArrayScalarID extends CanMulArrayScalar[Int,Double,Double];
}


/** Construction delegate for A :/ B. @author dramage */
trait CanDiv[-A,-B,+That] extends BinaryOp[A,B,That];

object CanDiv {
  //
  // Primitives
  //

  implicit object CanDivII extends CanDiv[Int,Int,Int]
  { def apply(a : Int, b : Int) = a / b; }

  implicit object CanDivIL extends CanDiv[Int,Long,Long]
  { def apply(a : Int, b : Long) = a / b; }

  implicit object CanDivIF extends CanDiv[Int,Float,Float]
  { def apply(a : Int, b : Float) = a / b; }

  implicit object CanDivID extends CanDiv[Int,Double,Double]
  { def apply(a : Int, b : Double) = a / b; }

  implicit object CanDivLI extends CanDiv[Long,Int,Long]
  { def apply(a : Long, b : Int) = a / b; }

  implicit object CanDivLL extends CanDiv[Long,Long,Long]
  { def apply(a : Long, b : Long) = a / b; }

  implicit object CanDivLF extends CanDiv[Long,Float,Double]
  { def apply(a : Long, b : Float) = a / b; }

  implicit object CanDivLD extends CanDiv[Long,Double,Double]
  { def apply(a : Long, b : Double) = a / b; }

  implicit object CanDivFI extends CanDiv[Float,Int,Float]
  { def apply(a : Float, b : Int) = a / b; }

  implicit object CanDivFL extends CanDiv[Float,Long,Double]
  { def apply(a : Float, b : Long) = a / b; }

  implicit object CanDivFF extends CanDiv[Float,Float,Float]
  { def apply(a : Float, b : Float) = a / b; }

  implicit object CanDivFD extends CanDiv[Float,Double,Double]
  { def apply(a : Float, b : Double) = a / b; }

  implicit object CanDivDI extends CanDiv[Double,Int,Double]
  { def apply(a : Double, b : Int) = a / b; }

  implicit object CanDivDL extends CanDiv[Double,Long,Double]
  { def apply(a : Double, b : Long) = a / b; }

  implicit object CanDivDF extends CanDiv[Double,Float,Double]
  { def apply(a : Double, b : Float) = a / b; }

  implicit object CanDivDD extends CanDiv[Double,Double,Double]
  { def apply(a : Double, b : Double) = a / b; }

  //
  // Arrays
  //

  implicit def CanDivArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanDiv[V1,V2,RV])
  = new CanDivArrayArray[V1,V2,RV];

  class CanDivArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanDiv[V1,V2,RV])
  extends ArrayArrayOp[V1,V2,RV] with CanDiv[Array[V1],Array[V2],Array[RV]];

  implicit object CanDivArrayArrayII extends CanDivArrayArray[Int,Int,Int];
  implicit object CanDivArrayArrayDD extends CanDivArrayArray[Double,Double,Double];
  implicit object CanDivArrayArrayDI extends CanDivArrayArray[Double,Int,Double];
  implicit object CanDivArrayArrayID extends CanDivArrayArray[Int,Double,Double];

  implicit def CanDivArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : CanDiv[V1,V2,RV], s : Scalar[V2])
  = new CanDivArrayScalar[V1,V2,RV];

  class CanDivArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : CanDiv[V1,V2,RV], s : Scalar[V2])
  extends ArrayScalarOp[V1,V2,RV] with CanDiv[Array[V1],V2,Array[RV]];

  implicit object CanDivArrayScalarII extends CanDivArrayScalar[Int,Int,Int];
  implicit object CanDivArrayScalarDD extends CanDivArrayScalar[Double,Double,Double];
  implicit object CanDivArrayScalarDI extends CanDivArrayScalar[Double,Int,Double];
  implicit object CanDivArrayScalarID extends CanDivArrayScalar[Int,Double,Double];

  implicit def mkScalarArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanDiv[V1,V2,RV], s : Scalar[V1])
  = new ScalarArray[V1,V2,RV];

  class ScalarArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanDiv[V1,V2,RV], s : Scalar[V1])
  extends ScalarArrayOp[V1,V2,RV] with CanDiv[V1,Array[V2],Array[RV]];

  implicit object ScalarArrayII extends ScalarArray[Int,Int,Int];
  implicit object ScalarArrayDD extends ScalarArray[Double,Double,Double];
  implicit object ScalarArrayDI extends ScalarArray[Double,Int,Double];
  implicit object ScalarArrayID extends ScalarArray[Int,Double,Double];
}

/** Construction delegate for A :% B. @author dramage */
trait CanMod[-A,-B,+That] extends BinaryOp[A,B,That];

object CanMod {
  //
  // Primitives
  //

  implicit object CanModII extends CanMod[Int,Int,Int]
  { def apply(a : Int, b : Int) = a % b; }

  implicit object CanModIL extends CanMod[Int,Long,Long]
  { def apply(a : Int, b : Long) = a % b; }

  implicit object CanModIF extends CanMod[Int,Float,Float]
  { def apply(a : Int, b : Float) = a % b; }

  implicit object CanModID extends CanMod[Int,Double,Double]
  { def apply(a : Int, b : Double) = a % b; }

  implicit object CanModLI extends CanMod[Long,Int,Long]
  { def apply(a : Long, b : Int) = a % b; }

  implicit object CanModLL extends CanMod[Long,Long,Long]
  { def apply(a : Long, b : Long) = a % b; }

  implicit object CanModLF extends CanMod[Long,Float,Double]
  { def apply(a : Long, b : Float) = a % b; }

  implicit object CanModLD extends CanMod[Long,Double,Double]
  { def apply(a : Long, b : Double) = a % b; }

  implicit object CanModFI extends CanMod[Float,Int,Float]
  { def apply(a : Float, b : Int) = a % b; }

  implicit object CanModFL extends CanMod[Float,Long,Double]
  { def apply(a : Float, b : Long) = a % b; }

  implicit object CanModFF extends CanMod[Float,Float,Float]
  { def apply(a : Float, b : Float) = a % b; }

  implicit object CanModFD extends CanMod[Float,Double,Double]
  { def apply(a : Float, b : Double) = a % b; }

  implicit object CanModDI extends CanMod[Double,Int,Double]
  { def apply(a : Double, b : Int) = a % b; }

  implicit object CanModDL extends CanMod[Double,Long,Double]
  { def apply(a : Double, b : Long) = a % b; }

  implicit object CanModDF extends CanMod[Double,Float,Double]
  { def apply(a : Double, b : Float) = a % b; }

  implicit object CanModDD extends CanMod[Double,Double,Double]
  { def apply(a : Double, b : Double) = a % b; }

  //
  // Arrays
  //

  implicit def ArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanMod[V1,V2,RV])
  = new ArrayArray[V1,V2,RV];

  class ArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanMod[V1,V2,RV])
  extends ArrayArrayOp[V1,V2,RV] with CanMod[Array[V1],Array[V2],Array[RV]];

  implicit object ArrayArrayII extends ArrayArray[Int,Int,Int];
  implicit object ArrayArrayDD extends ArrayArray[Double,Double,Double];
  implicit object ArrayArrayDI extends ArrayArray[Double,Int,Double];
  implicit object ArrayArrayID extends ArrayArray[Int,Double,Double];

  implicit def ArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : CanMod[V1,V2,RV], s : Scalar[V2])
  = new ArrayScalar[V1,V2,RV];

  class ArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : CanMod[V1,V2,RV], s : Scalar[V2])
  extends ArrayScalarOp[V1,V2,RV] with CanMod[Array[V1],V2,Array[RV]];

  implicit object ArrayScalarII extends ArrayScalar[Int,Int,Int];
  implicit object ArrayScalarDD extends ArrayScalar[Double,Double,Double];
  implicit object ArrayScalarDI extends ArrayScalar[Double,Int,Double];
  implicit object ArrayScalarID extends ArrayScalar[Int,Double,Double];

  implicit def mkScalarArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanMod[V1,V2,RV], s : Scalar[V1])
  = new ScalarArray[V1,V2,RV];

  class ScalarArray[V1,V2,RV](implicit m : ClassManifest[RV], op : CanMod[V1,V2,RV], s : Scalar[V1])
  extends ScalarArrayOp[V1,V2,RV] with CanMod[V1,Array[V2],Array[RV]];

  implicit object ScalarArrayII extends ScalarArray[Int,Int,Int];
  implicit object ScalarArrayDD extends ScalarArray[Double,Double,Double];
  implicit object ScalarArrayDI extends ScalarArray[Double,Int,Double];
  implicit object ScalarArrayID extends ScalarArray[Int,Double,Double];
}

/** Construction delegate for A :^ B. @author dramage */
trait CanPow[-A,-B,+That] extends BinaryOp[A,B,That];

/** Construction delegate for A :< B. @author dramage */
trait CanLT[-A,-B,+That] extends BinaryOp[A,B,That];

/** Construction delegate for A :<= B. @author dramage */
trait CanLTE[-A,-B,+That] extends BinaryOp[A,B,That];

/** Construction delegate for A :> B. @author dramage */
trait CanGT[-A,-B,+That] extends BinaryOp[A,B,That];

/** Construction delegate for A :>= B. @author dramage */
trait CanGTE[-A,-B,+That] extends BinaryOp[A,B,That];

/** Construction delegate for A :== B. @author dramage */
trait CanEQ[-A,-B,+That] extends BinaryOp[A,B,That];

/** Construction delegate for A :!= B. @author dramage */
trait CanNE[-A,-B,+That] extends BinaryOp[A,B,That];


trait NumericCollectionOps[+This] {
  def repr : This;

  def unary_-[That](implicit op : CanNeg[This,That]) : That = op(repr);

  def :+[B,That](b : B)(implicit op : CanAdd[This,B,That]) = op(repr,b);

  def :-[B,That](b : B)(implicit op : CanSub[This,B,That]) = op(repr,b);

  def :*[B,That](b : B)(implicit op : CanMul[This,B,That]) = op(repr,b);

  def :/[B,That](b : B)(implicit op : CanDiv[This,B,That]) = op(repr,b);

  def :%[B,That](b : B)(implicit op : CanMod[This,B,That]) = op(repr,b);

  def :<[B,That](b : B)(implicit op : CanLT[This,B,That]) = op(repr,b);

  def :<=[B,That](b : B)(implicit op : CanLTE[This,B,That]) = op(repr,b);

  /** Final alias for this.:+(b) */
  final def +[B,That](b : B)(implicit op : CanAdd[This,B,That]) = this.:+(b);

  /** Final alias for this.:-(b) */
  final def -[B,That](b : B)(implicit op : CanSub[This,B,That]) = this.:-(b);

  // TODO: add :^ :< :<= :> :>= :== :!=
}


/** Operation that updates A using B. @author dramage */
trait BinaryUpdateOp[-A,-B] {
  def apply(a : A, b : B) : Unit;
}

/** Base class for BinaryUpdateOp on a pair of arrays. @author dramage */
class ArrayArrayUpdateOp[V1,V2](implicit op : BinaryOp[V1,V2,V1])
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

/** Base class for BinaryUpdateOp on a pair of arrays. @author dramage */
class ArrayScalarUpdateOp[V1,B](implicit op : BinaryOp[V1,B,V1], sb : Scalar[B])
extends BinaryUpdateOp[Array[V1],B] {
  def apply(a : Array[V1], b : B) = {
    var i = 0;
    while (i < a.length) {
      a(i) = op(a(i),b);
      i += 1;
    }
  }
}

/** Mutation delegate for A := B. @author dramage */
trait CanAssignInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanAssignInto {
  implicit def mkIntoArrayInnerArrayInner[A,B](implicit op : CanAssignInto[A,B])
  = new IntoArrayInnerArrayInner[A,B];

  class IntoArrayInnerArrayInner[A,B](implicit op : CanAssignInto[A,B])
  extends CanAssignInto[Array[A],Array[B]] {
    override def apply(a : Array[A], b : Array[B]) = {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      var i = 0;
      while (i < a.length) {
        op(a(i), b(i));
        i += 1;
      }
      i;
    }
  }

  implicit def mkIntoArrayArray[V1,V2]
  (implicit fn : V2=>V1, s1 : Scalar[V1], s2 : Scalar[V2])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray
  [@specialized(Int,Long,Float,Double) V1,
   @specialized(Int,Long,Float,Double) V2]
  (implicit fn : V2=>V1, s1 : Scalar[V1], s2 : Scalar[V2])
  extends CanAssignInto[Array[V1],Array[V2]] {
    override def apply(a : Array[V1], b : Array[V2]) = {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      var i = 0;
      while (i < a.length) {
        a(i) = b(i);
        i += 1;
      }
      i;
    }
  }

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];

  implicit def mkIntoArrayScalar[V1,V2](implicit fn : V2=>V1, s2 : Scalar[V2])
  = new IntoArrayScalar[V1,V2];

  class IntoArrayScalar
  [@specialized(Int,Long,Float,Double) V1,
   @specialized(Int,Long,Float,Double) V2]
  (implicit fn : V2=>V1, s2 : Scalar[V2])
  extends CanAssignInto[Array[V1],V2] {
    override def apply(a : Array[V1], b : V2) = {
      var i = 0;
      while (i < a.length) {
        a(i) = b;
        i += 1;
      }
      i;
    }
  }

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
}

/** Mutation delegate for A :+= B. @author dramage */
trait CanAddInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanAddInto {
  implicit def mkIntoArrayArray[V1,V2](implicit op : CanAdd[V1,V2,V1])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : CanAdd[V1,V2,V1])
  extends ArrayArrayUpdateOp[V1,V2] with CanAddInto[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];

  implicit def mkIntoArrayScalar[V1,B](implicit op : CanAdd[V1,B,V1], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : CanAdd[V1,B,V1], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with CanAddInto[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
}

/** Mutation delegate for A :-= B. @author dramage */
trait CanSubInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanSubInto {
  implicit def mkIntoArrayArray[V1,V2](implicit op : CanSub[V1,V2,V1])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : CanSub[V1,V2,V1])
  extends ArrayArrayUpdateOp[V1,V2] with CanSubInto[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];

  implicit def mkIntoArrayScalar[V1,B](implicit op : CanSub[V1,B,V1], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : CanSub[V1,B,V1], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with CanSubInto[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
}

/** Mutation delegate for A :*= B. @author dramage */
trait CanMulInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanMulInto {
  implicit def mkIntoArrayArray[V1,V2](implicit op : CanMul[V1,V2,V1])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : CanMul[V1,V2,V1])
  extends ArrayArrayUpdateOp[V1,V2] with CanMulInto[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];

  implicit def mkIntoArrayScalar[V1,B](implicit op : CanMul[V1,B,V1], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : CanMul[V1,B,V1], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with CanMulInto[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
}

/** Mutation delegate for A :/= B. @author dramage */
trait CanDivInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanDivInto {
  implicit def mkIntoArrayArray[V1,V2](implicit op : CanDiv[V1,V2,V1])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : CanDiv[V1,V2,V1])
  extends ArrayArrayUpdateOp[V1,V2] with CanDivInto[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];

  implicit def mkIntoArrayScalar[V1,B](implicit op : CanDiv[V1,B,V1], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : CanDiv[V1,B,V1], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with CanDivInto[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
}

/** Mutation delegate for A :/= B. @author dramage */
trait CanModInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanModInto {
  implicit def mkIntoArrayArray[V1,V2](implicit op : CanMod[V1,V2,V1])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : CanMod[V1,V2,V1])
  extends ArrayArrayUpdateOp[V1,V2] with CanModInto[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];

  implicit def mkIntoArrayScalar[V1,B](implicit op : CanMod[V1,B,V1], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : CanMod[V1,B,V1], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with CanModInto[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
}



trait MutableNumericCollectionOps[+This] extends NumericCollectionOps[This] {
  def repr : This;

  def :=[B](b : B)(implicit op : CanAssignInto[This,B]) = op(repr,b);

  def :+=[B](b : B)(implicit op : CanAddInto[This,B]) = op(repr,b);

  def :-=[B](b : B)(implicit op : CanSubInto[This,B]) = op(repr,b);

  def :*=[B](b : B)(implicit op : CanMulInto[This,B]) = op(repr,b);

  def :/=[B](b : B)(implicit op : CanDivInto[This,B]) = op(repr,b);

  def :%=[B](b : B)(implicit op : CanModInto[This,B]) = op(repr,b);

  // TODO: Add :^=

  /** Final alias for this.:+=(b) */
  final def +=[B](b : B)(implicit op : CanAddInto[This,B]) = this.:+=(b);

  /** Final alias for this.:-=(b) */
  final def -=[B](b : B)(implicit op : CanSubInto[This,B]) = this.:-=(b);
}

//
// Shaped operations
// 


/** Construction delegate for outer (dot) product A * B. @author dramage */
trait CanMulOuter[-A,-B,+That] extends BinaryOp[A,B,That];

object CanMulOuter {
  implicit def CanMulOuterArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV])
  = new CanMulOuterArrayArray[V1,V2,RV];

  class CanMulOuterArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV])
  extends CanMulOuter[Array[V1],Array[V2],Array[Array[RV]]] {
    override def apply(a : Array[V1], b : Array[V2]) = {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      Array.tabulate(a.length,a.length)((i,j) => mul(a(i),b(j)));
    }
  }

  implicit object CanMulOuterArrayArrayII extends CanMulOuterArrayArray[Int,Int,Int];
  implicit object CanMulOuterArrayArrayDD extends CanMulOuterArrayArray[Double,Double,Double];
  implicit object CanMulOuterArrayArrayDI extends CanMulOuterArrayArray[Double,Int,Double];
  implicit object CanMulOuterArrayArrayID extends CanMulOuterArrayArray[Int,Double,Double];
}

/** For A*B where A is a matrix. @author dramage */
trait CanMulMatrixBy[-A,-B,+That] extends BinaryOp[A,B,That];

object CanMulMatrixBy {
  implicit def CanMulArrayMatrixByArray[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV],
   add : CanAdd[RV,RV,RV], srv : Scalar[RV]) =
     new CanMulArrayMatrixByArray[V1,V2,RV];

  class CanMulArrayMatrixByArray[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV],
   add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  extends CanMulMatrixBy[Array[Array[V1]],Array[V2],Array[RV]] {
    override def apply(a : Array[Array[V1]], b : Array[V2]) = {
      val rv = Array.fill(a.length)(srv.zero);
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

  implicit object CanMulArrayMatrixByArrayII extends CanMulArrayMatrixByArray[Int,Int,Int];
  implicit object CanMulArrayMatrixByArrayDD extends CanMulArrayMatrixByArray[Double,Double,Double];
  implicit object CanMulArrayMatrixByArrayDI extends CanMulArrayMatrixByArray[Double,Int,Double];
  implicit object CanMulArrayMatrixByArrayID extends CanMulArrayMatrixByArray[Int,Double,Double];

  
  implicit def CanMulArrayMatrixByArrayMatrix[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV],
   add : CanAdd[RV,RV,RV], srv : Scalar[RV]) =
     new CanMulArrayMatrixByArrayMatrix[V1,V2,RV];

  class CanMulArrayMatrixByArrayMatrix[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV],
   add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  extends CanMulMatrixBy[Array[Array[V1]],Array[Array[V2]],Array[Array[RV]]] {
    override def apply(a : Array[Array[V1]], b : Array[Array[V2]]) = {
      val numRows = a.length;
      val numCols = b(0).length;
      val numInner = b.length;

      Array.tabulate(numRows, numCols){(i,j) =>
        var rv = srv.zero;
        var k = 0;
        while (k < numInner) {
          rv = add(rv, mul(a(i)(k),b(k)(j)));
          k += 1;
        }
        rv;
      }
    }
  }

  implicit object CanMulArrayMatrixByArrayMatrixII extends CanMulArrayMatrixByArrayMatrix[Int,Int,Int];
  implicit object CanMulArrayMatrixByArrayMatrixDD extends CanMulArrayMatrixByArrayMatrix[Double,Double,Double];
  implicit object CanMulArrayMatrixByArrayMatrixDI extends CanMulArrayMatrixByArrayMatrix[Double,Int,Double];
  implicit object CanMulArrayMatrixByArrayMatrixID extends CanMulArrayMatrixByArrayMatrix[Int,Double,Double];
}

/**
 * Secialized NumericCollectionOps with shaped operations taking A is a column.
 * Note that columns are the default shape, so this trait simply extends
 * NumericCollectionOps[A].
 *
 * @author dramage
 */
trait ColumnTensorOps[+A] extends NumericCollectionOps[A] {
  def *[B,RV](b : RowTensorOps[B])(implicit op : CanMulOuter[A,B,RV]) : RV =
    op(repr,b.column);

  def t : RowTensorOps[A] = RowTensorOps(repr);
}

/**
 * A column tensor whose underyling collection is mutable.  This trait
 * should be used instead of mixing in "ColumnTensorOps with
 * MutableNumeriCollectionOps" directly, because .t needs to return an instance
 * of MutableRowTensorOps insetad of RowTensorOps.
 *
 * @author dramage
 */
trait MutableColumnTensorOps[+A] extends ColumnTensorOps[A] with MutableNumericCollectionOps[A] {
  override def t : MutableRowTensorOps[A] = MutableRowTensorOps(repr);
}


/** For A*B where A is a matrix. @author dramage */
trait CanMulRowBy[-A,-B,+That] extends BinaryOp[A,B,That];

object CanMulRowBy {
  implicit def CanMulRowArrayByArray[V1,V2,RV]
  (implicit mul : CanMul[V1,V2,RV], add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  = new CanMulRowArrayByArray[V1,V2,RV];

  /** Array inner product */
  class CanMulRowArrayByArray[V1,V2,RV]
  (implicit mul : CanMul[V1,V2,RV], add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  extends CanMulRowBy[Array[V1],Array[V2],RV] {
    override def apply(a : Array[V1], b : Array[V2]) = {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      var rv = srv.zero;
      var i = 0;
      while (i < a.length) {
        rv = add(rv, mul(a(i),b(i)));
        i += 1;
      }
      rv;
    }
  }

  implicit object CanMulRowArrayByArrayII extends CanMulRowArrayByArray[Int,Int,Int];
  implicit object CanMulRowArrayByArrayDD extends CanMulRowArrayByArray[Double,Double,Double];
  implicit object CanMulRowArrayByArrayDI extends CanMulRowArrayByArray[Double,Int,Double];
  implicit object CanMulRowArrayByArrayID extends CanMulRowArrayByArray[Int,Double,Double];

  implicit def CanMulRowArrayByArrayMatrix[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV],
   add : CanAdd[RV,RV,RV], srv : Scalar[RV]) =
     new CanMulRowArrayByArrayMatrix[V1,V2,RV];

  /** Array matrix by array matrix */
  class CanMulRowArrayByArrayMatrix[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV],
   add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  extends CanMulRowBy[Array[V1],Array[Array[V2]],MutableRowTensorOps[Array[RV]]] {
    override def apply(a : Array[V1], b : Array[Array[V2]]) = {
      val rv = Array.fill(b.length)(srv.zero);
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

  implicit object CanMulRowArrayByArrayMatrixII extends CanMulRowArrayByArrayMatrix[Int,Int,Int];
  implicit object CanMulRowArrayByArrayMatrixDD extends CanMulRowArrayByArrayMatrix[Double,Double,Double];
  implicit object CanMulRowArrayByArrayMatrixDI extends CanMulRowArrayByArrayMatrix[Double,Int,Double];
  implicit object CanMulRowArrayByArrayMatrixID extends CanMulRowArrayByArrayMatrix[Int,Double,Double];
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

  def *[B,RV](b : B)(implicit op : CanMulRowBy[A,B,RV]) : RV =
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

  implicit def CanSubRows[A,B,That](implicit op : CanSub[A,B,That])
  = new RowBinaryOp[A,B,That] with CanSub[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanMulRows[A,B,That](implicit op : CanMul[A,B,That])
  = new RowBinaryOp[A,B,That] with CanMul[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanDivRows[A,B,That](implicit op : CanDiv[A,B,That])
  = new RowBinaryOp[A,B,That] with CanDiv[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanModRows[A,B,That](implicit op : CanMod[A,B,That])
  = new RowBinaryOp[A,B,That] with CanMod[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

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
  def *[B,That](b : B)(implicit op : CanMulMatrixBy[A,B,That]) =
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

/**
 * Numeric operator support for solo scalars.  Note: we do not support
 * raw "+" to avoid ambiguity with the any2String implicit that comes built
 * in with scala and is used for string concatenation.  Use :+ instead.
 * Similarly, use ":-" instead of "-", which we exclude for consistency.
 *
 * @author dramage
 */
class RichScalar[@specialized(Int,Long,Float,Double) A:Scalar](val scalar : A) {
  /** Commutative: defer to scalar + b. */
  def :+[B,That](b : B)(implicit op : CanAdd[B,A,That]) = op(b,scalar);

  /** Not commutative: need special implementation of scalar - b. */
  def :-[B,That](b : B)(implicit op : CanSub[A,B,That]) = op(scalar,b);

  /** Commutative: defer to scalar * b. */
  def :*[B,That](b : B)(implicit op : CanMul[B,A,That]) = op(b,scalar);

  /** Not commutative: need special implementation of scala / b. */
  def :/[B,That](b : B)(implicit op : CanDiv[A,B,That]) = op(scalar,b);

  /** Not commutative: need special implementation of scala % b. */
  def :%[B,That](b : B)(implicit op : CanMod[A,B,That]) = op(scalar,b);
}

/** Numeric operator support for scala maps. @athor dramage */
class RichNumericMap[K,V](override val repr : Map[K,V])
extends NumericCollectionOps[Map[K,V]];

class RichNumericDomainMap[A,V,M<:DomainMap[A,V]]
(override val repr : M)
extends NumericCollectionOps[M];

object Implicits {
  implicit def richScalar[@specialized(Int,Long,Float,Double) V:Scalar](value : V) =
    new RichScalar(value);

  implicit def richNumericArrayVector[V:ClassManifest](value : Array[V]) =
    new RichNumericArrayVector(value);

  implicit def richNumericArrayMatrix[V:ClassManifest](value : Array[Array[V]]) =
    new RichNumericArrayMatrix(value);

  implicit def richNumericMap[K,V](value : Map[K,V]) =
    new RichNumericMap[K,V](value);

  implicit def richNumericDomainMap[A,V](value : DomainMap[A,V]) =
    new RichNumericDomainMap[A,V,DomainMap[A,V]](value);
}

object OpsTest {
  import Implicits._;

  def main(args : Array[String]) {
    val x = Array(1,2,3,4);
    val y = Array(-2,-3,-4,-5);
    println(x mkString(" "));
    println(x :/ 2.0 mkString(" "));
    println((-x) mkString(" "));
    println((x :+ 1) mkString(" "));
    println((1 :+ x) mkString(" "));
    println((x + y) mkString(" "));
    println((x - y) mkString(" "));

    x :+= y;  println(x mkString(" "));
    x :-= y;  println(x mkString(" "));
    x :*= y;  println(x mkString(" "));
    x :/= y;  println(x mkString(" "));

    println((Array(1.0,2.0,3.0) :/ Array(2,2,2)) mkString(" "));
    println((Array(1,2,3) :/ Array(.5,.5,.5)) mkString(" "));
    println((Array(1.0,2.0,3.0) :* Array(2,2,2)) mkString(" "));
    println((Array(1,2,3) :* Array(.5,.5,.5)) mkString(" "));
    println((Array(1.0,2.0,3.0) :% Array(2,2,2)) mkString(" "));
    println((Array(1,2,3) :% Array(.5,.5,.5)) mkString(" "));

    println(Map("a"->1,"b"->2) + Map("a"->1,"b"->2));

    // println(DomainMap("a"->1,"b"->2,"c"->3) + DomainMap("a"->1,"b"->2,"c"->3));

    println(x.t :+ y.t);
    println(x.t * y);

    println((x * y.t).map(_.mkString(" ")).mkString("\n"));

    val m : Array[Array[Int]] = x * y.t;
    println((m * Array(3,2,1,0)) mkString(" "));

    m :+= m;
    println(m.map(_.mkString(" ")).mkString("\n"));

    val a = Array(Array(1,2,3),Array(3,0,4));
    val b = Array(Array(-3,7),Array(6,1),Array(2,2));
    println((a * b).map(_.mkString(" ")).mkString("\n"));
  }
}

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
package generic;

import scalala.scalar.Scalar;
import scalala.tensor.domain.DomainException;
import scalala.collection.sparse.{SparseArray,DefaultArrayValue};

/**
 * Construction delegate for A :^ B.
 *
 * @author dramage
 */
trait CanPow[A,-B,+That] extends BinaryOp[A,B,That];

object CanPow {
  type Op[A,B,That] = CanPow[A,B,That]
  type SparseArraySparseArrayBase[A,B,That] = SparseArraySparseArrayEitherNZOp[A,B,That];
  type SparseArrayArrayBase[A,B,That] = SparseArrayArrayAllToArrayOp[A,B,That];
  type ArraySparseArrayBase[A,B,That] = ArraySparseArrayAllOp[A,B,That];
  type SparseArrayArrayOutput[A] = Array[A];
  type ArraySparseArrayOutput[A] = Array[A];
  type MapMapBase[K,V1,V2,RV] = MapMapEitherNonZeroOp[K,V1,V2,RV];

  //
  // Primitives
  //

  implicit object OpII extends Op[Int,Int,Double]
  { def apply(a : Int, b : Int) = scala.math.pow(a,b); }

  implicit object OpIL extends Op[Int,Long,Double]
  { def apply(a : Int, b : Long) = scala.math.pow(a,b); }

  implicit object OpIF extends Op[Int,Float,Double]
  { def apply(a : Int, b : Float) = scala.math.pow(a,b); }

  implicit object OpID extends Op[Int,Double,Double]
  { def apply(a : Int, b : Double) = scala.math.pow(a,b); }

  implicit object OpLI extends Op[Long,Int,Double]
  { def apply(a : Long, b : Int) = scala.math.pow(a,b); }

  implicit object OpLL extends Op[Long,Long,Double]
  { def apply(a : Long, b : Long) = scala.math.pow(a,b); }

  implicit object OpLF extends Op[Long,Float,Double]
  { def apply(a : Long, b : Float) = scala.math.pow(a,b); }

  implicit object OpLD extends Op[Long,Double,Double]
  { def apply(a : Long, b : Double) = scala.math.pow(a,b); }

  implicit object OpFI extends Op[Float,Int,Double]
  { def apply(a : Float, b : Int) = scala.math.pow(a,b); }

  implicit object OpFL extends Op[Float,Long,Double]
  { def apply(a : Float, b : Long) = scala.math.pow(a,b); }

  implicit object OpFF extends Op[Float,Float,Double]
  { def apply(a : Float, b : Float) = scala.math.pow(a,b); }

  implicit object OpFD extends Op[Float,Double,Double]
  { def apply(a : Float, b : Double) = scala.math.pow(a,b); }

  implicit object OpDI extends Op[Double,Int,Double]
  { def apply(a : Double, b : Int) = scala.math.pow(a,b); }

  implicit object OpDL extends Op[Double,Long,Double]
  { def apply(a : Double, b : Long) = scala.math.pow(a,b); }

  implicit object OpDF extends Op[Double,Float,Double]
  { def apply(a : Double, b : Float) = scala.math.pow(a,b); }

  implicit object OpDD extends Op[Double,Double,Double]
  { def apply(a : Double, b : Double) = scala.math.pow(a,b); }

  //
  //
  // Below this line is copy-and-paste between ops' companion objects
  //
  //

  //
  // Arrays
  //

  implicit def opArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV])
  = new OpArrayArray[V1,V2,RV];

  class OpArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV])
  extends ArrayArrayOp[V1,V2,RV] with Op[Array[V1],Array[V2],Array[RV]];

  implicit object OpArrayArrayII extends OpArrayArray[Int,Int,Double];
  implicit object OpArrayArrayDD extends OpArrayArray[Double,Double,Double];
  implicit object OpArrayArrayDI extends OpArrayArray[Double,Int,Double];
  implicit object OpArrayArrayID extends OpArrayArray[Int,Double,Double];

  implicit def opArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], s : Scalar[V2])
  = new OpArrayScalar[V1,V2,RV];

  class OpArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], s : Scalar[V2])
  extends ArrayScalarOp[V1,V2,RV] with Op[Array[V1],V2,Array[RV]];

  implicit object OpArrayScalarII extends OpArrayScalar[Int,Int,Double];
  implicit object OpArrayScalarDD extends OpArrayScalar[Double,Double,Double];
  implicit object OpArrayScalarDI extends OpArrayScalar[Double,Int,Double];
  implicit object OpArrayScalarID extends OpArrayScalar[Int,Double,Double];

  implicit def opScalarArray[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], s : Scalar[V1])
  = new OpScalarArray[V1,V2,RV];

  class OpScalarArray[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], s : Scalar[V1])
  extends ScalarArrayOp[V1,V2,RV] with Op[V1,Array[V2],Array[RV]];

  implicit object OpScalarArrayII extends OpScalarArray[Int,Int,Double];
  implicit object OpScalarArrayDD extends OpScalarArray[Double,Double,Double];
  implicit object OpScalarArrayDI extends OpScalarArray[Double,Int,Double];
  implicit object OpScalarArrayID extends OpScalarArray[Int,Double,Double];

  //
  // Sparse arrays
  //

  implicit def opSparseArraySparseArray[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], dv : DefaultArrayValue[RV])
  = new OpSparseArraySparseArray[V1,V2,RV];

  class OpSparseArraySparseArray[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], dv : DefaultArrayValue[RV])
  extends SparseArraySparseArrayBase[V1,V2,RV] with Op[SparseArray[V1],SparseArray[V2],SparseArray[RV]];

  implicit object OpSparseArraySparseArrayII extends OpSparseArraySparseArray[Int,Int,Double];
  implicit object OpSparseArraySparseArrayID extends OpSparseArraySparseArray[Int,Double,Double];
  implicit object OpSparseArraySparseArrayDI extends OpSparseArraySparseArray[Double,Int,Double];
  implicit object OpSparseArraySparseArrayDD extends OpSparseArraySparseArray[Double,Double,Double];

  implicit def opSparseArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], s : Scalar[V2], dv : DefaultArrayValue[RV])
  = new OpSparseArrayScalar[V1,V2,RV];

  class OpSparseArrayScalar[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], s : Scalar[V2], dv : DefaultArrayValue[RV])
  extends SparseArrayScalarOp[V1,V2,RV] with Op[SparseArray[V1],V2,SparseArray[RV]];

  implicit object OpSparseArrayScalarII extends OpSparseArrayScalar[Int,Int,Double];
  implicit object OpSparseArrayScalarDD extends OpSparseArrayScalar[Double,Double,Double];
  implicit object OpSparseArrayScalarDI extends OpSparseArrayScalar[Double,Int,Double];
  implicit object OpSparseArrayScalarID extends OpSparseArrayScalar[Int,Double,Double];

  implicit def opScalarSparseArray[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], s : Scalar[V1], dv : DefaultArrayValue[RV])
  = new OpScalarSparseArray[V1,V2,RV];

  class OpScalarSparseArray[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], s : Scalar[V1], dv : DefaultArrayValue[RV])
  extends ScalarSparseArrayOp[V1,V2,RV] with Op[V1,SparseArray[V2],SparseArray[RV]];

  implicit object OpScalarSparseArrayII extends OpScalarSparseArray[Int,Int,Double];
  implicit object OpScalarSparseArrayDD extends OpScalarSparseArray[Double,Double,Double];
  implicit object OpScalarSparseArrayDI extends OpScalarSparseArray[Double,Int,Double];
  implicit object OpScalarSparseArrayID extends OpScalarSparseArray[Int,Double,Double];

  implicit def opSparseArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], dv : DefaultArrayValue[RV])
  = new OpSparseArrayArray[V1,V2,RV];

  class OpSparseArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], dv : DefaultArrayValue[RV])
  extends SparseArrayArrayBase[V1,V2,RV] with Op[SparseArray[V1],Array[V2],SparseArrayArrayOutput[RV]];

  implicit object OpSparseArrayArrayII extends OpSparseArrayArray[Int,Int,Double];
  implicit object OpSparseArrayArrayID extends OpSparseArrayArray[Int,Double,Double];
  implicit object OpSparseArrayArrayDI extends OpSparseArrayArray[Double,Int,Double];
  implicit object OpSparseArrayArrayDD extends OpSparseArrayArray[Double,Double,Double];

  implicit def opArraySparseArray[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], dv : DefaultArrayValue[RV])
  = new OpArraySparseArray[V1,V2,RV];

  class OpArraySparseArray[V1,V2,RV](implicit m : ClassManifest[RV], op : Op[V1,V2,RV], dv : DefaultArrayValue[RV])
  extends ArraySparseArrayBase[V1,V2,RV] with Op[Array[V1],SparseArray[V2],ArraySparseArrayOutput[RV]];

  implicit object OpArraySparseArrayII extends OpArraySparseArray[Int,Int,Double];
  implicit object OpArraySparseArrayID extends OpArraySparseArray[Int,Double,Double];
  implicit object OpArraySparseArrayDI extends OpArraySparseArray[Double,Int,Double];
  implicit object OpArraySparseArrayDD extends OpArraySparseArray[Double,Double,Double];

  //
  // Tuples
  //

  implicit def opTuple2Tuple2[VA1,VA2,VB1,VB2,RV1,RV2]
  (implicit op1 : Op[VA1,VB1,RV1], op2 : Op[VA2,VB2,RV2]) =
    new OpTuple2Tuple2[VA1,VA2,VB1,VB2,RV1,RV2];

  class OpTuple2Tuple2[VA1,VA2,VB1,VB2,RV1,RV2]
  (implicit op1 : BinaryOp[VA1,VB1,RV1], op2 : BinaryOp[VA2,VB2,RV2])
  extends Tuple2Tuple2Op[VA1,VA2,VB1,VB2,RV1,RV2]
     with Op[(VA1,VA2),(VB1,VB2),(RV1,RV2)];

  implicit def opTuple3Tuple3[VA1,VA2,VA3,VB1,VB2,VB3,RV1,RV2,RV3]
  (implicit op1 : Op[VA1,VB1,RV1], op2 : Op[VA2,VB2,RV2], op3 : Op[VA3,VB3,RV3]) =
    new OpTuple3Tuple3[VA1,VA2,VA3,VB1,VB2,VB3,RV1,RV2,RV3];

  class OpTuple3Tuple3[VA1,VA2,VA3,VB1,VB2,VB3,RV1,RV2,RV3]
  (implicit op1 : Op[VA1,VB1,RV1], op2 : Op[VA2,VB2,RV2], op3 : Op[VA3,VB3,RV3])
  extends Tuple3Tuple3Op[VA1,VA2,VA3,VB1,VB2,VB3,RV1,RV2,RV3]
     with Op[(VA1,VA2,VA3),(VB1,VB2,VB3),(RV1,RV2,RV3)];

  implicit def opTuple4Tuple4[VA1,VA2,VA3,VA4,VB1,VB2,VB3,VB4,RV1,RV2,RV3,RV4]
  (implicit op1 : Op[VA1,VB1,RV1], op2 : Op[VA2,VB2,RV2], op3 : Op[VA3,VB3,RV3], op4 : Op[VA4,VB4,RV4]) =
    new OpTuple4Tuple4[VA1,VA2,VA3,VA4,VB1,VB2,VB3,VB4,RV1,RV2,RV3,RV4];

  class OpTuple4Tuple4[VA1,VA2,VA3,VA4,VB1,VB2,VB3,VB4,RV1,RV2,RV3,RV4]
  (implicit op1 : Op[VA1,VB1,RV1], op2 : Op[VA2,VB2,RV2], op3 : Op[VA3,VB3,RV3], op4 : Op[VA4,VB4,RV4])
  extends Tuple4Tuple4Op[VA1,VA2,VA3,VA4,VB1,VB2,VB3,VB4,RV1,RV2,RV3,RV4]
     with Op[(VA1,VA2,VA3,VA4),(VB1,VB2,VB3,VB4),(RV1,RV2,RV3,RV4)];


  //
  // Scala Maps
  //

  implicit def opMap[K,V1,V2,RV](implicit op : Op[V1,V2,RV], sa : Scalar[V1], sb : Scalar[V2]) =
    new OpMap[K,V1,V2,RV];

  class OpMap[K,V1,V2,RV](implicit op : Op[V1,V2,RV], sa : Scalar[V1], sb : Scalar[V2])
  extends MapMapBase[K,V1,V2,RV] with Op[Map[K,V1],Map[K,V2],Map[K,RV]];
}

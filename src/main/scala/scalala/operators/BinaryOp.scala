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

import scala.annotation.implicitNotFound;
import scala.collection.generic.CanBuildFrom;

import scalala.scalar.Scalar;

/**
 * Operation of type O that returns That from arguments A and B.
 *
 * @author dramage
 */
@implicitNotFound(msg="Could not find a way to ${O} values of type ${A} and ${B}")
trait BinaryOp[@specialized -A, @specialized -B, O<:OpType, +To]
extends ((A,B) => To) {
  def opType : O;
}

trait LowPriorityBinaryOpImplicits {
  /**
   * Recursively add together the values from two maps.  Assumes all keys
   * are in both maps.
   */
  implicit def OpRecurseMapMap[K,V1,V2,RV,O<:OpType,M,That]
  (implicit view : M=>scala.collection.Map[K,V1],
    op : BinaryOp[V1,V2,O,RV], c : CompatibleShape[V1,V2],
    bf : CanBuildFrom[M,(K,RV),That])
  : BinaryOp[M, scala.collection.Map[K,V2], O, That]
  = new BinaryOp[M, scala.collection.Map[K,V2], O, That] {
    def opType = op.opType;
    def apply(a : M, b : scala.collection.Map[K,V2]) = {
      val builder = bf(a);
      for (k <- (a.keySet | b.keySet)) {
        builder += k -> op(a(k),b(k));
      }
      builder.result;
    }
  }
}

object BinaryOp extends LowPriorityBinaryOpImplicits {

  // Int <-> Int

  implicit object OpAddII extends BinaryOp[Int,Int,OpAdd,Int]
    { def opType = OpAdd; def apply(a : Int, b : Int) = a + b; }
  
  implicit object OpSubII extends BinaryOp[Int,Int,OpSub,Int]
    { def opType = OpSub; def apply(a : Int, b : Int) = a - b; }
  
  implicit object OpMulII extends BinaryOp[Int,Int,OpMul,Int]
    { def opType = OpMul; def apply(a : Int, b : Int) = a * b; }
  
  implicit object OpDivII extends BinaryOp[Int,Int,OpDiv,Int]
    { def opType = OpDiv; def apply(a : Int, b : Int) = a / b; }
  
  implicit object OpModII extends BinaryOp[Int,Int,OpMod,Int]
    { def opType = OpMod; def apply(a : Int, b : Int) = a % b; }
  
  implicit object OpPowII extends BinaryOp[Int,Int,OpPow,Double]
    { def opType = OpPow; def apply(a : Int, b : Int) = math.pow(a,b); }
  
  implicit object OpLTII extends BinaryOp[Int,Int,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Int, b : Int) = a < b; }
  
  implicit object OpLTEII extends BinaryOp[Int,Int,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Int, b : Int) = a <= b; }
  
  implicit object OpGTII extends BinaryOp[Int,Int,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Int, b : Int) = a > b; }
  
  implicit object OpGTEII extends BinaryOp[Int,Int,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Int, b : Int) = a >= b; }
  
  implicit object OpEqII extends BinaryOp[Int,Int,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Int, b : Int) = a == b; }
  
  implicit object OpNeII extends BinaryOp[Int,Int,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Int, b : Int) = a != b; }

  // Long <-> Long

  implicit object OpAddLL extends BinaryOp[Long,Long,OpAdd,Long]
    { def opType = OpAdd; def apply(a : Long, b : Long) = a + b; }
  
  implicit object OpSubLL extends BinaryOp[Long,Long,OpSub,Long]
    { def opType = OpSub; def apply(a : Long, b : Long) = a - b; }
  
  implicit object OpMulLL extends BinaryOp[Long,Long,OpMul,Long]
    { def opType = OpMul; def apply(a : Long, b : Long) = a * b; }
  
  implicit object OpDivLL extends BinaryOp[Long,Long,OpDiv,Long]
    { def opType = OpDiv; def apply(a : Long, b : Long) = a / b; }
  
  implicit object OpModLL extends BinaryOp[Long,Long,OpMod,Long]
    { def opType = OpMod; def apply(a : Long, b : Long) = a % b; }
  
  implicit object OpPowLL extends BinaryOp[Long,Long,OpPow,Double]
    { def opType = OpPow; def apply(a : Long, b : Long) = math.pow(a,b); }
  
  implicit object OpLTLL extends BinaryOp[Long,Long,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Long, b : Long) = a < b; }
  
  implicit object OpLTELL extends BinaryOp[Long,Long,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Long, b : Long) = a <= b; }
  
  implicit object OpGTLL extends BinaryOp[Long,Long,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Long, b : Long) = a > b; }
  
  implicit object OpGTELL extends BinaryOp[Long,Long,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Long, b : Long) = a >= b; }
  
  implicit object OpEqLL extends BinaryOp[Long,Long,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Long, b : Long) = a == b; }
  
  implicit object OpNeLL extends BinaryOp[Long,Long,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Long, b : Long) = a != b; }
    
  // Float <-> Float

  implicit object OpAddFF extends BinaryOp[Float,Float,OpAdd,Float]
    { def opType = OpAdd; def apply(a : Float, b : Float) = a + b; }
  
  implicit object OpSubFF extends BinaryOp[Float,Float,OpSub,Float]
    { def opType = OpSub; def apply(a : Float, b : Float) = a - b; }
  
  implicit object OpMulFF extends BinaryOp[Float,Float,OpMul,Float]
    { def opType = OpMul; def apply(a : Float, b : Float) = a * b; }
  
  implicit object OpDivFF extends BinaryOp[Float,Float,OpDiv,Float]
    { def opType = OpDiv; def apply(a : Float, b : Float) = a / b; }
  
  implicit object OpModFF extends BinaryOp[Float,Float,OpMod,Float]
    { def opType = OpMod; def apply(a : Float, b : Float) = a % b; }
  
  implicit object OpPowFF extends BinaryOp[Float,Float,OpPow,Double]
    { def opType = OpPow; def apply(a : Float, b : Float) = math.pow(a,b); }
  
  implicit object OpLTFF extends BinaryOp[Float,Float,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Float, b : Float) = a < b; }
  
  implicit object OpLTEFF extends BinaryOp[Float,Float,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Float, b : Float) = a <= b; }
  
  implicit object OpGTFF extends BinaryOp[Float,Float,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Float, b : Float) = a > b; }
  
  implicit object OpGTEFF extends BinaryOp[Float,Float,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Float, b : Float) = a >= b; }
  
  implicit object OpEqFF extends BinaryOp[Float,Float,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Float, b : Float) = a == b; }
  
  implicit object OpNeFF extends BinaryOp[Float,Float,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Float, b : Float) = a != b; }
    
  // Double <-> Double
  
  implicit object OpAddDD extends BinaryOp[Double,Double,OpAdd,Double]
    { def opType = OpAdd; def apply(a : Double, b : Double) = a + b; }
  
  implicit object OpSubDD extends BinaryOp[Double,Double,OpSub,Double]
    { def opType = OpSub; def apply(a : Double, b : Double) = a - b; }
  
  implicit object OpMulDD extends BinaryOp[Double,Double,OpMul,Double]
    { def opType = OpMul; def apply(a : Double, b : Double) = a * b; }
  
  implicit object OpDivDD extends BinaryOp[Double,Double,OpDiv,Double]
    { def opType = OpDiv; def apply(a : Double, b : Double) = a / b; }
  
  implicit object OpModDD extends BinaryOp[Double,Double,OpMod,Double]
    { def opType = OpMod; def apply(a : Double, b : Double) = a % b; }
  
  implicit object OpPowDD extends BinaryOp[Double,Double,OpPow,Double]
    { def opType = OpPow; def apply(a : Double, b : Double) = math.pow(a,b); }
  
  implicit object OpLTDD extends BinaryOp[Double,Double,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Double, b : Double) = a < b; }
  
  implicit object OpLTEDD extends BinaryOp[Double,Double,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Double, b : Double) = a <= b; }
  
  implicit object OpGTDD extends BinaryOp[Double,Double,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Double, b : Double) = a > b; }
  
  implicit object OpGTEDD extends BinaryOp[Double,Double,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Double, b : Double) = a >= b; }
  
  implicit object OpEqDD extends BinaryOp[Double,Double,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Double, b : Double) = a == b; }
  
  implicit object OpNeDD extends BinaryOp[Double,Double,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Double, b : Double) = a != b; }
  
  // Int <-> Double
  
  implicit object OpAddID extends BinaryOp[Int,Double,OpAdd,Double]
    { def opType = OpAdd; def apply(a : Int, b : Double) = a + b; }
  
  implicit object OpSubID extends BinaryOp[Int,Double,OpSub,Double]
    { def opType = OpSub; def apply(a : Int, b : Double) = a - b; }
  
  implicit object OpMulID extends BinaryOp[Int,Double,OpMul,Double]
    { def opType = OpMul; def apply(a : Int, b : Double) = a * b; }
  
  implicit object OpDivID extends BinaryOp[Int,Double,OpDiv,Double]
    { def opType = OpDiv; def apply(a : Int, b : Double) = a / b; }
  
  implicit object OpModID extends BinaryOp[Int,Double,OpMod,Double]
    { def opType = OpMod; def apply(a : Int, b : Double) = a % b; }
  
  implicit object OpPowID extends BinaryOp[Int,Double,OpPow,Double]
    { def opType = OpPow; def apply(a : Int, b : Double) = math.pow(a,b); }
    
  implicit object OpLTID extends BinaryOp[Int,Double,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Int, b : Double) = a < b; }
  
  implicit object OpLTEID extends BinaryOp[Int,Double,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Int, b : Double) = a <= b; }
  
  implicit object OpGTID extends BinaryOp[Int,Double,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Int, b : Double) = a > b; }
  
  implicit object OpGTEID extends BinaryOp[Int,Double,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Int, b : Double) = a >= b; }
  
  implicit object OpEqID extends BinaryOp[Int,Double,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Int, b : Double) = a == b; }
  
  implicit object OpNeID extends BinaryOp[Int,Double,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Int, b : Double) = a != b; }
    
  // Double <-> Int
  
  implicit object OpAddDI extends BinaryOp[Double,Int,OpAdd,Double]
    { def opType = OpAdd; def apply(a : Double, b : Int) = a + b; }
  
  implicit object OpSubDI extends BinaryOp[Double,Int,OpSub,Double]
    { def opType = OpSub; def apply(a : Double, b : Int) = a - b; }
  
  implicit object OpMulDI extends BinaryOp[Double,Int,OpMul,Double]
    { def opType = OpMul; def apply(a : Double, b : Int) = a * b; }
  
  implicit object OpDivDI extends BinaryOp[Double,Int,OpDiv,Double]
    { def opType = OpDiv; def apply(a : Double, b : Int) = a / b; }
  
  implicit object OpModDI extends BinaryOp[Double,Int,OpMod,Double]
    { def opType = OpMod; def apply(a : Double, b : Int) = a % b; }
  
  implicit object OpPowDI extends BinaryOp[Double,Int,OpPow,Double]
    { def opType = OpPow; def apply(a : Double, b : Int) = math.pow(a,b); }
    
  implicit object OpLTDI extends BinaryOp[Double,Int,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Double, b : Int) = a < b; }
  
  implicit object OpLTEDI extends BinaryOp[Double,Int,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Double, b : Int) = a <= b; }
  
  implicit object OpGTDI extends BinaryOp[Double,Int,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Double, b : Int) = a > b; }
  
  implicit object OpGTEDI extends BinaryOp[Double,Int,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Double, b : Int) = a >= b; }
  
  implicit object OpEqDI extends BinaryOp[Double,Int,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Double, b : Int) = a == b; }
  
  implicit object OpNeDI extends BinaryOp[Double,Int,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Double, b : Int) = a != b; }

  // Boolean * Anything and vice versa

  implicit object OpMulBB extends BinaryOp[Boolean,Boolean,OpMul,Boolean]
    { def opType = OpMul; def apply(a : Boolean, b : Boolean) = a && b; }

  implicit object OpMulBI extends BinaryOp[Boolean,Int,OpMul,Int]
    { def opType = OpMul; def apply(a : Boolean, b : Int) = if (a) b else 0; }

  implicit object OpMulBL extends BinaryOp[Boolean,Long,OpMul,Long]
    { def opType = OpMul; def apply(a : Boolean, b : Long) = if (a) b else 0; }

  implicit object OpMulBF extends BinaryOp[Boolean,Float,OpMul,Float]
    { def opType = OpMul; def apply(a : Boolean, b : Float) = if (a) b else 0; }

  implicit object OpMulBD extends BinaryOp[Boolean,Double,OpMul,Double]
    { def opType = OpMul; def apply(a : Boolean, b : Double) = if (a) b else 0; }

  implicit object OpMulIB extends BinaryOp[Int,Boolean,OpMul,Int]
    { def opType = OpMul; def apply(a : Int, b : Boolean) = if (b) a else 0; }

  implicit object OpMulLB extends BinaryOp[Long,Boolean,OpMul,Long]
    { def opType = OpMul; def apply(a : Long, b : Boolean) = if (b) a else 0; }

  implicit object OpMulFB extends BinaryOp[Float,Boolean,OpMul,Float]
    { def opType = OpMul; def apply(a : Float, b : Boolean) = if (b) a else 0; }

  implicit object OpMulDB extends BinaryOp[Double,Boolean,OpMul,Double]
    { def opType = OpMul; def apply(a : Double, b : Boolean) = if (b) a else 0; }

  implicit def OpMulBooleanAny[V](implicit s : Scalar[V])
  : BinaryOp[Boolean,V,OpMul,V]
  = new BinaryOp[Boolean,V,OpMul,V] {
    def opType = OpMul; def apply(a : Boolean, b : V) = if (a) b else s.zero;
  }

  implicit def OpMulAnyBoolean[V](implicit s : Scalar[V])
  : BinaryOp[V,Boolean,OpMul,V]
  = new BinaryOp[V,Boolean,OpMul,V] {
    def opType = OpMul; def apply(a : V, b : Boolean) = if (b) a else s.zero;
  }

    // Double <-> Boolean

  implicit object OpAddDB extends BinaryOp[Double,Boolean,OpAdd,Double]
    { def opType = OpAdd; def apply(a : Double, b : Boolean) = if(b) a + 1 else a; }

  implicit object OpSubDB extends BinaryOp[Double,Boolean,OpSub,Double]
    { def opType = OpSub; def apply(a : Double, b : Boolean) = if(b) a - 1 else a; }

  implicit object OpPowDB extends BinaryOp[Double,Boolean,OpPow,Double]
    { def opType = OpPow; def apply(a : Double, b : Boolean) = math.pow(a,if(b) 1.0 else 0.0); }

  //
  // Promote regular scalar multiplications to shaped multiplications
  //
  
  implicit def promoteScalarMulToMulRowVectorBy[A,B,RV]
  (implicit view : A=>scalala.tensor.Tensor1Row[_,_],
   op : BinaryOp[A,B,OpMul,RV], s : Scalar[B])
  : BinaryOp[A,B,OpMulRowVectorBy,RV]
  = new BinaryOp[A,B,OpMulRowVectorBy,RV] {
    def opType = OpMulRowVectorBy; def apply(a : A, b : B) = op(a,b);
  }

  implicit def promoteScalarMulToMulColVectorBy[A,B,RV]
  (implicit view : A=>scalala.tensor.Tensor1Col[_,_],
   op : BinaryOp[A,B,OpMul,RV], s : Scalar[B])
  : BinaryOp[A,B,OpMulColVectorBy,RV]
  = new BinaryOp[A,B,OpMulColVectorBy,RV] {
    def opType = OpMulColVectorBy; def apply(a : A, b : B) = op(a,b);
  }

  implicit def promoteScalarMulToMulMatrixBy[A,B,RV]
  (implicit view : A=>scalala.tensor.Tensor2[_,_,_],
   op : BinaryOp[A,B,OpMul,RV], s : Scalar[B])
  : BinaryOp[A,B,OpMulMatrixBy,RV]
  = new BinaryOp[A,B,OpMulMatrixBy,RV] {
    def opType = OpMulMatrixBy; def apply(a : A, b : B) = op(a,b);
  }
    
  //
  // Tuples
  //

  implicit def OpTuple2Tuple2[VA1,VA2,VB1,VB2,RV1,RV2,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2],
   c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2])
  : BinaryOp[(VA1,VA2),(VB1,VB2),O,(RV1,RV2)]
  = new BinaryOp[(VA1,VA2),(VB1,VB2),O,(RV1,RV2)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2), b : (VB1,VB2)) = (op1(a._1,b._1), op2(a._2,b._2));
  }
  
  implicit def OpTuple2Scalar[VA1,VA2,VB,RV1,RV2,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB,O,RV1], op2 : BinaryOp[VA2,VB,O,RV2], s : Scalar[VB])
  : BinaryOp[(VA1,VA2),VB,O,(RV1,RV2)]
  = new BinaryOp[(VA1,VA2),VB,O,(RV1,RV2)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2), b : VB) = (op1(a._1,b), op2(a._2,b));
  }
  
  implicit def OpScalarTuple2[VA,VB1,VB2,RV1,RV2,O<:OpType]
  (implicit op1 : BinaryOp[VA,VB1,O,RV1], op2 : BinaryOp[VA,VB2,O,RV2], s : Scalar[VA])
  : BinaryOp[VA,(VB1,VB2),O,(RV1,RV2)]
  = new BinaryOp[VA,(VB1,VB2),O,(RV1,RV2)] {
    def opType = op1.opType;
    def apply(a : VA, b : (VB1,VB2)) = (op1(a,b._1), op2(a,b._2));
  }
  
  //
  // Maps
  //
  
  implicit def OpLeafMapMap[K,V1,V2,RV,O<:OpType,M,That]
  (implicit view : M=>scala.collection.Map[K,V1],
    op : BinaryOp[V1,V2,O,RV], s1 : Scalar[V1], s2 : Scalar[V2],
    bf : CanBuildFrom[M,(K,RV),That])
  : BinaryOp[M, scala.collection.Map[K,V2], O, That]
  = new BinaryOp[M, scala.collection.Map[K,V2], O, That] {
    def opType = op.opType;
    def apply(a : M, b : scala.collection.Map[K,V2]) = {
      val builder = bf(a);
      for (k <- a.keySet) {
        builder += k -> op(a(k), b.getOrElse(k, s2.zero));
      }
      for (k <- b.keySet) {
        if (!a.contains(k)) {
          builder += k -> op(s1.zero, b(k));
        }
      }
      builder.result;
    }
  }
  
  implicit def OpMapScalar[K,V1,V2,RV,O<:OpType,M,That]
  (implicit view : M=>scala.collection.Map[K,V1],
    op : BinaryOp[V1,V2,O,RV], s : Scalar[V2],
    bf : CanBuildFrom[M,(K,RV),That])
  : BinaryOp[M, V2, O, That]
  = new BinaryOp[M, V2, O, That] {
    def opType = op.opType;
    def apply(a : M, b : V2) = {
      val builder = bf(a);
      for ((k,v) <- a) {
        builder += k -> op(v,b);
      }
      builder.result;
    }
  }
  
  implicit def OpScalarMap[K,V1,V2,RV,O<:OpType,M,That]
  (implicit view : M=>scala.collection.Map[K,V2],
    op : BinaryOp[V1,V2,O,RV], s : Scalar[V1],
    bf : CanBuildFrom[M,(K,RV),That])
  : BinaryOp[V1, M, O, That]
  = new BinaryOp[V1, M, O, That] {
    def opType = op.opType;
    def apply(a : V1, b : M) = {
      val builder = bf(b);
      for ((k,v) <- b) {
        builder += k -> op(a,v);
      }
      builder.result;
    }
  }
  
  //
  // Seqs
  //

  implicit def OpSeqSeq[V1,V2,RV,O<:OpType,S,That]
  (implicit view : S=>scala.collection.Seq[V1],
   op : BinaryOp[V1,V2,O,RV], c : CompatibleShape[V1,V2],
   bf : CanBuildFrom[S,RV,That])
  : BinaryOp[S,scala.collection.Seq[V2],O,That]
  = new BinaryOp[S,scala.collection.Seq[V2],O,That] {
    def opType = op.opType;
    def apply(a : S, b : scala.collection.Seq[V2]) = {
      require(a.length == b.length, "Inputs must be the same length");
      val builder = bf(a);
      for ((ai,bi) <- a.iterator zip b.iterator) {
        builder += op(ai,bi);
      }
      builder.result;
    }
  }
  
  implicit def OpSeqScalar[V1,V2,RV,O<:OpType,S,That]
  (implicit view : S=>scala.collection.Seq[V1],
   op : BinaryOp[V1,V2,O,RV], s : Scalar[V2],
   bf : CanBuildFrom[S,RV,That])
  : BinaryOp[S, V2, O, That]
  = new BinaryOp[S, V2, O, That] {
    def opType = op.opType;
    def apply(a : S, b : V2) = {
      val builder = bf(a);
      for (v <- a) builder += op(v, b);
      builder.result;
    }
  }
  
  implicit def OpScalarSeq[V1,V2,RV,O<:OpType,S,That]
  (implicit view : S=>scala.collection.Seq[V2],
   op : BinaryOp[V1,V2,O,RV], s : Scalar[V1],
   bf : CanBuildFrom[S,RV,That])
  : BinaryOp[V1, S, O, That]
  = new BinaryOp[V1, S, O, That] {
    def opType = op.opType;
    def apply(a : V1, b : S) = {
      val builder = bf(b);
      for (v <- b) builder += op(a, v);
      builder.result;
    }
  }


  //
  // Arrays
  //

  implicit def OpArrayArray[V1,V2,RV,O<:OpType]
  (implicit op : BinaryOp[V1,V2,O,RV], c : CompatibleShape[V1,V2], mf : Manifest[RV])
  : BinaryOp[Array[V1],Array[V2],O,Array[RV]]
  = new BinaryOp[Array[V1],Array[V2],O,Array[RV]] {
    def opType = op.opType;
    def apply(a : Array[V1], b : Array[V2]) = {
      require(a.length == b.length, "Inputs must be the same length");
      var rv = new Array[RV](a.length);
      var i = 0;
      while (i < rv.length) {
        rv(i) = op(a(i),b(i));
        i += 1;
      }
      rv;
    }
  }
  
  implicit def OpArrayScalar[V1,V2,RV,O<:OpType]
  (implicit op : BinaryOp[V1,V2,O,RV], s : Scalar[V2], mf : Manifest[RV])
  : BinaryOp[Array[V1], V2, O, Array[RV]]
  = new BinaryOp[Array[V1], V2, O, Array[RV]] {
    def opType = op.opType;
    def apply(a : Array[V1], b : V2) = {
      var rv = new Array[RV](a.length);
      var i = 0;
      while (i < rv.length) {
        rv(i) = op(a(i),b);
        i += 1;
      }
      rv;
    }
  }
  
  implicit def OpScalarArray[V1,V2,RV,O<:OpType]
  (implicit op : BinaryOp[V1,V2,O,RV], s : Scalar[V1], mf : Manifest[RV])
  : BinaryOp[V1, Array[V2], O, Array[RV]]
  = new BinaryOp[V1, Array[V2], O, Array[RV]] {
    def opType = op.opType;
    def apply(a : V1, b : Array[V2]) = {
      var rv = new Array[RV](b.length);
      var i = 0;
      while (i < rv.length) {
        rv(i) = op(a,b(i));
        i += 1;
      }
      rv;
    }
  }
  
//  //
//  // Functions
//  //
//  
//  /** Allows function compisition e.g. math.cos _ * math.sin _ */
//  implicit def OpFunctionFunction[K,V1,V2,RV,O<:OpType]
//  (implicit op : BinaryOp[V1,V2,O,RV])
//  : BinaryOp[K=>V1, K=>V2, O, K=>RV]
//  = new BinaryOp[K=>V1, K=>V2, O, K=>RV] {
//    def apply(a : K=>V1, b : K=>V2) = (k : K) => op(a(k),b(k));
//  }
}

//
///**
// * Base class for BinaryOp on a pair of scala maps on all values where at
// * least one map contains the given key.
// *
// * @author dramage
// */
//class MapMapEitherNonZeroOp[K,V1,V2,RV](op : BinaryOp[V1,V2,RV], ul : V1 => RV, ur : V2 => RV)
//extends BinaryOp[Map[K,V1],scala.collection.Map[K,V2],Map[K,RV]] {
//  def apply(a : Map[K,V1], b : scala.collection.Map[K,V2]) : Map[K,RV] = {
//    (a.keySet ++ b.keySet).map(
//      k => {
////        (k, op(a.getOrElse(k, sa.zero), b.getOrElse(k, sb.zero)));
//        if (a contains k) {
//          if (b contains k) {
//            // in both a and b
//            (k, op(a(k),b(k)));
//          } else {
//            // only in a
//            (k, ul(a(k)));
//          }
//        } else {
//          // only in b
//          (k, ur(b(k)));
//        }
//      }
//    ).toMap;
//  }
//}
//
///**
// * Base class for BinaryOp on a pair of scala maps on all values where at
// * both maps contains the given key.
// *
// * @author dramage
// */
//class MapMapBothNonZeroOp[K,V1,V2,RV](implicit op : BinaryOp[V1,V2,RV])
//extends BinaryOp[Map[K,V1],Map[K,V2],Map[K,RV]] {
//  def apply(a : Map[K,V1], b : Map[K,V2]) =
//    (a.keySet & b.keySet).map(k => (k,op(a(k),b(k)))).toMap;
//}


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

trait BinaryOpImplicitsLevel0 {
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

object BinaryOp extends BinaryTupleOps with BinaryOpImplicitsLevel0 {

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
  
  implicit object OpAndII extends BinaryOp[Int,Int,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Int, b : Int) = a != 0 && b != 0; }
  
  implicit object OpOrII extends BinaryOp[Int,Int,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Int, b : Int) = a != 0 || b != 0; }

  implicit object OpXorII extends BinaryOp[Int,Int,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Int, b : Int) = (a != 0) ^ (b != 0); }

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
  
  implicit object OpAndLL extends BinaryOp[Long,Long,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Long, b : Long) = a != 0 && b != 0; }
  
  implicit object OpOrLL extends BinaryOp[Long,Long,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Long, b : Long) = a != 0 || b != 0; }

  implicit object OpXorLL extends BinaryOp[Long,Long,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Long, b : Long) = (a != 0) ^ (b != 0); }
    
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
  
  implicit object OpAndFF extends BinaryOp[Float,Float,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Float, b : Float) = a != 0 && b != 0; }
  
  implicit object OpOrFF extends BinaryOp[Float,Float,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Float, b : Float) = a != 0 || b != 0; }

  implicit object OpXorFF extends BinaryOp[Float,Float,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Float, b : Float) = (a != 0) ^ (b != 0); }
    
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
  
  implicit object OpAndDD extends BinaryOp[Double,Double,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Double, b : Double) = a != 0 && b != 0; }
  
  implicit object OpOrDD extends BinaryOp[Double,Double,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Double, b : Double) = a != 0 || b != 0; }

  implicit object OpXorDD extends BinaryOp[Double,Double,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Double, b : Double) = (a != 0) ^ (b != 0); }
  
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
  
  implicit object OpAndID extends BinaryOp[Int,Double,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Int, b : Double) = a != 0 && b != 0; }
  
  implicit object OpOrID extends BinaryOp[Int,Double,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Int, b : Double) = a != 0 || b != 0; }

  implicit object OpXorID extends BinaryOp[Int,Double,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Int, b : Double) = (a != 0) ^ (b != 0); }
    
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
  
  implicit object OpAndDI extends BinaryOp[Double,Int,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Double, b : Int) = a != 0 && b != 0; }
  
  implicit object OpOrDI extends BinaryOp[Double,Int,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Double, b : Int) = a != 0 || b != 0; }

  implicit object OpXorDI extends BinaryOp[Double,Int,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Double, b : Int) = (a != 0) ^ (b != 0); }

  // Int <-> Float
  
  implicit object OpAddIF extends BinaryOp[Int,Float,OpAdd,Float]
    { def opType = OpAdd; def apply(a : Int, b : Float) = a + b; }
  
  implicit object OpSubIF extends BinaryOp[Int,Float,OpSub,Float]
    { def opType = OpSub; def apply(a : Int, b : Float) = a - b; }
  
  implicit object OpMulIF extends BinaryOp[Int,Float,OpMul,Float]
    { def opType = OpMul; def apply(a : Int, b : Float) = a * b; }
  
  implicit object OpDivIF extends BinaryOp[Int,Float,OpDiv,Float]
    { def opType = OpDiv; def apply(a : Int, b : Float) = a / b; }
  
  implicit object OpModIF extends BinaryOp[Int,Float,OpMod,Float]
    { def opType = OpMod; def apply(a : Int, b : Float) = a % b; }
  
  implicit object OpPowIF extends BinaryOp[Int,Float,OpPow,Double]
    { def opType = OpPow; def apply(a : Int, b : Float) = math.pow(a,b); }
    
  implicit object OpLTIF extends BinaryOp[Int,Float,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Int, b : Float) = a < b; }
  
  implicit object OpLTEIF extends BinaryOp[Int,Float,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Int, b : Float) = a <= b; }
  
  implicit object OpGTIF extends BinaryOp[Int,Float,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Int, b : Float) = a > b; }
  
  implicit object OpGTEIF extends BinaryOp[Int,Float,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Int, b : Float) = a >= b; }
  
  implicit object OpEqIF extends BinaryOp[Int,Float,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Int, b : Float) = a == b; }
  
  implicit object OpNeIF extends BinaryOp[Int,Float,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Int, b : Float) = a != b; }
  
  implicit object OpAndIF extends BinaryOp[Int,Float,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Int, b : Float) = a != 0 && b != 0; }
  
  implicit object OpOrIF extends BinaryOp[Int,Float,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Int, b : Float) = a != 0 || b != 0; }

  implicit object OpXorIF extends BinaryOp[Int,Float,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Int, b : Float) = (a != 0) ^ (b != 0); }
    
  // Float <-> Int
  
  implicit object OpAddFI extends BinaryOp[Float,Int,OpAdd,Float]
    { def opType = OpAdd; def apply(a : Float, b : Int) = a + b; }
  
  implicit object OpSubFI extends BinaryOp[Float,Int,OpSub,Float]
    { def opType = OpSub; def apply(a : Float, b : Int) = a - b; }
  
  implicit object OpMulFI extends BinaryOp[Float,Int,OpMul,Float]
    { def opType = OpMul; def apply(a : Float, b : Int) = a * b; }
  
  implicit object OpDivFI extends BinaryOp[Float,Int,OpDiv,Float]
    { def opType = OpDiv; def apply(a : Float, b : Int) = a / b; }
  
  implicit object OpModFI extends BinaryOp[Float,Int,OpMod,Float]
    { def opType = OpMod; def apply(a : Float, b : Int) = a % b; }
  
  implicit object OpPowFI extends BinaryOp[Float,Int,OpPow,Double]
    { def opType = OpPow; def apply(a : Float, b : Int) = math.pow(a,b); }
    
  implicit object OpLTFI extends BinaryOp[Float,Int,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Float, b : Int) = a < b; }
  
  implicit object OpLTEFI extends BinaryOp[Float,Int,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Float, b : Int) = a <= b; }
  
  implicit object OpGTFI extends BinaryOp[Float,Int,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Float, b : Int) = a > b; }
  
  implicit object OpGTEFI extends BinaryOp[Float,Int,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Float, b : Int) = a >= b; }
  
  implicit object OpEqFI extends BinaryOp[Float,Int,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Float, b : Int) = a == b; }
  
  implicit object OpNeFI extends BinaryOp[Float,Int,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Float, b : Int) = a != b; }
  
  implicit object OpAndFI extends BinaryOp[Float,Int,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Float, b : Int) = a != 0 && b != 0; }
  
  implicit object OpOrFI extends BinaryOp[Float,Int,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Float, b : Int) = a != 0 || b != 0; }

  implicit object OpXorFI extends BinaryOp[Float,Int,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Float, b : Int) = (a != 0) ^ (b != 0); }
  
  // Int <-> Long
  
  implicit object OpAddIL extends BinaryOp[Int,Long,OpAdd,Long]
    { def opType = OpAdd; def apply(a : Int, b : Long) = a + b; }
  
  implicit object OpSubIL extends BinaryOp[Int,Long,OpSub,Long]
    { def opType = OpSub; def apply(a : Int, b : Long) = a - b; }
  
  implicit object OpMulIL extends BinaryOp[Int,Long,OpMul,Long]
    { def opType = OpMul; def apply(a : Int, b : Long) = a * b; }
  
  implicit object OpDivIL extends BinaryOp[Int,Long,OpDiv,Long]
    { def opType = OpDiv; def apply(a : Int, b : Long) = a / b; }
  
  implicit object OpModIL extends BinaryOp[Int,Long,OpMod,Long]
    { def opType = OpMod; def apply(a : Int, b : Long) = a % b; }
  
  implicit object OpPowIL extends BinaryOp[Int,Long,OpPow,Double]
    { def opType = OpPow; def apply(a : Int, b : Long) = math.pow(a,b); }
    
  implicit object OpLTIL extends BinaryOp[Int,Long,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Int, b : Long) = a < b; }
  
  implicit object OpLTEIL extends BinaryOp[Int,Long,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Int, b : Long) = a <= b; }
  
  implicit object OpGTIL extends BinaryOp[Int,Long,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Int, b : Long) = a > b; }
  
  implicit object OpGTEIL extends BinaryOp[Int,Long,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Int, b : Long) = a >= b; }
  
  implicit object OpEqIL extends BinaryOp[Int,Long,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Int, b : Long) = a == b; }
  
  implicit object OpNeIL extends BinaryOp[Int,Long,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Int, b : Long) = a != b; }
  
  implicit object OpAndIL extends BinaryOp[Int,Long,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Int, b : Long) = a != 0 && b != 0; }
  
  implicit object OpOrIL extends BinaryOp[Int,Long,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Int, b : Long) = a != 0 || b != 0; }

  implicit object OpXorIL extends BinaryOp[Int,Long,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Int, b : Long) = (a != 0) ^ (b != 0); }
    
  // Long <-> Int
  
  implicit object OpAddLI extends BinaryOp[Long,Int,OpAdd,Long]
    { def opType = OpAdd; def apply(a : Long, b : Int) = a + b; }
  
  implicit object OpSubLI extends BinaryOp[Long,Int,OpSub,Long]
    { def opType = OpSub; def apply(a : Long, b : Int) = a - b; }
  
  implicit object OpMulLI extends BinaryOp[Long,Int,OpMul,Long]
    { def opType = OpMul; def apply(a : Long, b : Int) = a * b; }
  
  implicit object OpDivLI extends BinaryOp[Long,Int,OpDiv,Long]
    { def opType = OpDiv; def apply(a : Long, b : Int) = a / b; }
  
  implicit object OpModLI extends BinaryOp[Long,Int,OpMod,Long]
    { def opType = OpMod; def apply(a : Long, b : Int) = a % b; }
  
  implicit object OpPowLI extends BinaryOp[Long,Int,OpPow,Double]
    { def opType = OpPow; def apply(a : Long, b : Int) = math.pow(a,b); }
    
  implicit object OpLTLI extends BinaryOp[Long,Int,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Long, b : Int) = a < b; }
  
  implicit object OpLTELI extends BinaryOp[Long,Int,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Long, b : Int) = a <= b; }
  
  implicit object OpGTLI extends BinaryOp[Long,Int,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Long, b : Int) = a > b; }
  
  implicit object OpGTELI extends BinaryOp[Long,Int,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Long, b : Int) = a >= b; }
  
  implicit object OpEqLI extends BinaryOp[Long,Int,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Long, b : Int) = a == b; }
  
  implicit object OpNeLI extends BinaryOp[Long,Int,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Long, b : Int) = a != b; }
  
  implicit object OpAndLI extends BinaryOp[Long,Int,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Long, b : Int) = a != 0 && b != 0; }
  
  implicit object OpOrLI extends BinaryOp[Long,Int,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Long, b : Int) = a != 0 || b != 0; }

  implicit object OpXorLI extends BinaryOp[Long,Int,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Long, b : Int) = (a != 0) ^ (b != 0); }
  
  // Float <-> Double
  
  implicit object OpAddFD extends BinaryOp[Float,Double,OpAdd,Double]
    { def opType = OpAdd; def apply(a : Float, b : Double) = a + b; }
  
  implicit object OpSubFD extends BinaryOp[Float,Double,OpSub,Double]
    { def opType = OpSub; def apply(a : Float, b : Double) = a - b; }
  
  implicit object OpMulFD extends BinaryOp[Float,Double,OpMul,Double]
    { def opType = OpMul; def apply(a : Float, b : Double) = a * b; }
  
  implicit object OpDivFD extends BinaryOp[Float,Double,OpDiv,Double]
    { def opType = OpDiv; def apply(a : Float, b : Double) = a / b; }
  
  implicit object OpModFD extends BinaryOp[Float,Double,OpMod,Double]
    { def opType = OpMod; def apply(a : Float, b : Double) = a % b; }
  
  implicit object OpPowFD extends BinaryOp[Float,Double,OpPow,Double]
    { def opType = OpPow; def apply(a : Float, b : Double) = math.pow(a,b); }
    
  implicit object OpLTFD extends BinaryOp[Float,Double,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Float, b : Double) = a < b; }
  
  implicit object OpLTEFD extends BinaryOp[Float,Double,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Float, b : Double) = a <= b; }
  
  implicit object OpGTFD extends BinaryOp[Float,Double,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Float, b : Double) = a > b; }
  
  implicit object OpGTEFD extends BinaryOp[Float,Double,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Float, b : Double) = a >= b; }
  
  implicit object OpEqFD extends BinaryOp[Float,Double,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Float, b : Double) = a == b; }
  
  implicit object OpNeFD extends BinaryOp[Float,Double,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Float, b : Double) = a != b; }
  
  implicit object OpAndFD extends BinaryOp[Float,Double,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Float, b : Double) = a != 0 && b != 0; }
  
  implicit object OpOrFD extends BinaryOp[Float,Double,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Float, b : Double) = a != 0 || b != 0; }

  implicit object OpXorFD extends BinaryOp[Float,Double,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Float, b : Double) = (a != 0) ^ (b != 0); }
    
  // Double <-> Float
  
  implicit object OpAddDF extends BinaryOp[Double,Float,OpAdd,Double]
    { def opType = OpAdd; def apply(a : Double, b : Float) = a + b; }
  
  implicit object OpSubDF extends BinaryOp[Double,Float,OpSub,Double]
    { def opType = OpSub; def apply(a : Double, b : Float) = a - b; }
  
  implicit object OpMulDF extends BinaryOp[Double,Float,OpMul,Double]
    { def opType = OpMul; def apply(a : Double, b : Float) = a * b; }
  
  implicit object OpDivDF extends BinaryOp[Double,Float,OpDiv,Double]
    { def opType = OpDiv; def apply(a : Double, b : Float) = a / b; }
  
  implicit object OpModDF extends BinaryOp[Double,Float,OpMod,Double]
    { def opType = OpMod; def apply(a : Double, b : Float) = a % b; }
  
  implicit object OpPowDF extends BinaryOp[Double,Float,OpPow,Double]
    { def opType = OpPow; def apply(a : Double, b : Float) = math.pow(a,b); }
    
  implicit object OpLTDF extends BinaryOp[Double,Float,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Double, b : Float) = a < b; }
  
  implicit object OpLTEDF extends BinaryOp[Double,Float,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Double, b : Float) = a <= b; }
  
  implicit object OpGTDF extends BinaryOp[Double,Float,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Double, b : Float) = a > b; }
  
  implicit object OpGTEDF extends BinaryOp[Double,Float,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Double, b : Float) = a >= b; }
  
  implicit object OpEqDF extends BinaryOp[Double,Float,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Double, b : Float) = a == b; }
  
  implicit object OpNeDF extends BinaryOp[Double,Float,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Double, b : Float) = a != b; }
  
  implicit object OpAndDF extends BinaryOp[Double,Float,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Double, b : Float) = a != 0 && b != 0; }
  
  implicit object OpOrDF extends BinaryOp[Double,Float,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Double, b : Float) = a != 0 || b != 0; }

  implicit object OpXorDF extends BinaryOp[Double,Float,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Double, b : Float) = (a != 0) ^ (b != 0); }

  // Long <-> Double
  
  implicit object OpAddLD extends BinaryOp[Long,Double,OpAdd,Double]
    { def opType = OpAdd; def apply(a : Long, b : Double) = a + b; }
  
  implicit object OpSubLD extends BinaryOp[Long,Double,OpSub,Double]
    { def opType = OpSub; def apply(a : Long, b : Double) = a - b; }
  
  implicit object OpMulLD extends BinaryOp[Long,Double,OpMul,Double]
    { def opType = OpMul; def apply(a : Long, b : Double) = a * b; }
  
  implicit object OpDivLD extends BinaryOp[Long,Double,OpDiv,Double]
    { def opType = OpDiv; def apply(a : Long, b : Double) = a / b; }
  
  implicit object OpModLD extends BinaryOp[Long,Double,OpMod,Double]
    { def opType = OpMod; def apply(a : Long, b : Double) = a % b; }
  
  implicit object OpPowLD extends BinaryOp[Long,Double,OpPow,Double]
    { def opType = OpPow; def apply(a : Long, b : Double) = math.pow(a,b); }
    
  implicit object OpLTLD extends BinaryOp[Long,Double,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Long, b : Double) = a < b; }
  
  implicit object OpLTELD extends BinaryOp[Long,Double,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Long, b : Double) = a <= b; }
  
  implicit object OpGTLD extends BinaryOp[Long,Double,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Long, b : Double) = a > b; }
  
  implicit object OpGTELD extends BinaryOp[Long,Double,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Long, b : Double) = a >= b; }
  
  implicit object OpEqLD extends BinaryOp[Long,Double,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Long, b : Double) = a == b; }
  
  implicit object OpNeLD extends BinaryOp[Long,Double,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Long, b : Double) = a != b; }
  
  implicit object OpAndLD extends BinaryOp[Long,Double,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Long, b : Double) = a != 0 && b != 0; }
  
  implicit object OpOrLD extends BinaryOp[Long,Double,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Long, b : Double) = a != 0 || b != 0; }

  implicit object OpXorLD extends BinaryOp[Long,Double,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Long, b : Double) = (a != 0) ^ (b != 0); }
    
  // Double <-> Long
  
  implicit object OpAddDL extends BinaryOp[Double,Long,OpAdd,Double]
    { def opType = OpAdd; def apply(a : Double, b : Long) = a + b; }
  
  implicit object OpSubDL extends BinaryOp[Double,Long,OpSub,Double]
    { def opType = OpSub; def apply(a : Double, b : Long) = a - b; }
  
  implicit object OpMulDL extends BinaryOp[Double,Long,OpMul,Double]
    { def opType = OpMul; def apply(a : Double, b : Long) = a * b; }
  
  implicit object OpDivDL extends BinaryOp[Double,Long,OpDiv,Double]
    { def opType = OpDiv; def apply(a : Double, b : Long) = a / b; }
  
  implicit object OpModDL extends BinaryOp[Double,Long,OpMod,Double]
    { def opType = OpMod; def apply(a : Double, b : Long) = a % b; }
  
  implicit object OpPowDL extends BinaryOp[Double,Long,OpPow,Double]
    { def opType = OpPow; def apply(a : Double, b : Long) = math.pow(a,b); }
    
  implicit object OpLTDL extends BinaryOp[Double,Long,OpLT,Boolean]
    { def opType = OpLT; def apply(a : Double, b : Long) = a < b; }
  
  implicit object OpLTEDL extends BinaryOp[Double,Long,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : Double, b : Long) = a <= b; }
  
  implicit object OpGTDL extends BinaryOp[Double,Long,OpGT,Boolean]
    { def opType = OpGT; def apply(a : Double, b : Long) = a > b; }
  
  implicit object OpGTEDL extends BinaryOp[Double,Long,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : Double, b : Long) = a >= b; }
  
  implicit object OpEqDL extends BinaryOp[Double,Long,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Double, b : Long) = a == b; }
  
  implicit object OpNeDL extends BinaryOp[Double,Long,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Double, b : Long) = a != b; }
  
  implicit object OpAndDL extends BinaryOp[Double,Long,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Double, b : Long) = a != 0 && b != 0; }
  
  implicit object OpOrDL extends BinaryOp[Double,Long,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Double, b : Long) = a != 0 || b != 0; }

  implicit object OpXorDL extends BinaryOp[Double,Long,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Double, b : Long) = (a != 0) ^ (b != 0); }

  // Boolean <-> Boolean
  
  implicit object OpEqBB extends BinaryOp[Boolean,Boolean,OpEq,Boolean]
    { def opType = OpEq; def apply(a : Boolean, b : Boolean) = a == b; }
  
  implicit object OpNeBB extends BinaryOp[Boolean,Boolean,OpNe,Boolean]
    { def opType = OpNe; def apply(a : Boolean, b : Boolean) = a != b; }
  
  implicit object OpAndBB extends BinaryOp[Boolean,Boolean,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : Boolean, b : Boolean) = a && b; }
  
  implicit object OpOrBB extends BinaryOp[Boolean,Boolean,OpOr,Boolean]
    { def opType = OpOr; def apply(a : Boolean, b : Boolean) = a || b; }

  implicit object OpXorBB extends BinaryOp[Boolean,Boolean,OpXor,Boolean]
    { def opType = OpXor; def apply(a : Boolean, b : Boolean) = a ^ b; }

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
}


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
package codegen;

import scalala.tensor.dense._;


class GeneratedBinaryOp[A, B, O<:OpType, To](override val opType : O)
(implicit a : Manifest[A], b : Manifest[B], op : Manifest[O], to : Manifest[To],
 gen : BinaryOpGenerator[A,B,O,To])
extends BinaryOp[A,B,O,To] {
  lazy val instance = gen.getInstance

  override def apply(a : A, b : B) =
    instance(a,b);
  
  override def toString =
    gen.toString;
}

object GeneratedBinaryOpDefinitions {
  def main(args : Array[String]) {
    val ops = Array("OpAdd","OpSub","OpMul","OpDiv")
    val scalars = Array("Int","Long","Float","Double")
    val tensors = Array("Array","DenseVectorRow","DenseVectorCol")
    
    // returns the scalar result of combining the two operands
    def sr(s1 : String, s2 : String) =
      if (scalars.indexOf(s1) < scalars.indexOf(s2)) s2 else s1;
    
    val opsTensorTensorSame = 
      for (t <- tensors; s1 <- scalars; s2 <- scalars; op <- ops) yield """
        |  implicit object %s%s%s%s%s extends GeneratedBinaryOp[%s[%s],%s[%s],%s,%s[%s]](%s)
        |  BinaryOpRegistry.register[%s[%s],%s[%s],%s,%s[%s]]
        """.stripMargin.format(op,t,s1,t,s2,t,s1,t,s2,op,t,sr(s1,s2),op,t,s1,t,s2,op,t,sr(s1,s2))

    val opsTensorScalar =
      for (t <- tensors; s1 <- scalars; s2 <- scalars; op <- ops) yield
        "implicit object %s%s%s%s extends GeneratedBinaryOp[%s[%s],%s,%s,%s[%s]](%s)".
          format(op,t,s1,s2,t,s1,s2,op,t,sr(s1,s2),op)
      
    val opsScalarTensor =
      for (t <- tensors; s1 <- scalars; s2 <- scalars; op <- ops) yield
        "implicit object %s%s%s%s extends GeneratedBinaryOp[%s,%s[%s],%s,%s[%s]](%s)".
          format(op,s1,t,s2,s1,t,s2,op,t,sr(s1,s2),op);

    for (line <- opsTensorTensorSame ++ opsTensorScalar ++ opsScalarTensor) {
      println("  "+line)
    }
  }
}

object GeneratedBinaryOps {
  
  def init() {}
  
  implicit object OpAddArrayIntArrayInt extends GeneratedBinaryOp[Array[Int],Array[Int],OpAdd,Array[Int]](OpAdd)
  BinaryOpRegistry.register[Array[Int],Array[Int],OpAdd,Array[Int]]
        
  
  implicit object OpSubArrayIntArrayInt extends GeneratedBinaryOp[Array[Int],Array[Int],OpSub,Array[Int]](OpSub)
  BinaryOpRegistry.register[Array[Int],Array[Int],OpSub,Array[Int]]
        
  
  implicit object OpMulArrayIntArrayInt extends GeneratedBinaryOp[Array[Int],Array[Int],OpMul,Array[Int]](OpMul)
  BinaryOpRegistry.register[Array[Int],Array[Int],OpMul,Array[Int]]
        
  
  implicit object OpDivArrayIntArrayInt extends GeneratedBinaryOp[Array[Int],Array[Int],OpDiv,Array[Int]](OpDiv)
  BinaryOpRegistry.register[Array[Int],Array[Int],OpDiv,Array[Int]]
        
  
  implicit object OpAddArrayIntArrayLong extends GeneratedBinaryOp[Array[Int],Array[Long],OpAdd,Array[Long]](OpAdd)
  BinaryOpRegistry.register[Array[Int],Array[Long],OpAdd,Array[Long]]
        
  
  implicit object OpSubArrayIntArrayLong extends GeneratedBinaryOp[Array[Int],Array[Long],OpSub,Array[Long]](OpSub)
  BinaryOpRegistry.register[Array[Int],Array[Long],OpSub,Array[Long]]
        
  
  implicit object OpMulArrayIntArrayLong extends GeneratedBinaryOp[Array[Int],Array[Long],OpMul,Array[Long]](OpMul)
  BinaryOpRegistry.register[Array[Int],Array[Long],OpMul,Array[Long]]
        
  
  implicit object OpDivArrayIntArrayLong extends GeneratedBinaryOp[Array[Int],Array[Long],OpDiv,Array[Long]](OpDiv)
  BinaryOpRegistry.register[Array[Int],Array[Long],OpDiv,Array[Long]]
        
  
  implicit object OpAddArrayIntArrayFloat extends GeneratedBinaryOp[Array[Int],Array[Float],OpAdd,Array[Float]](OpAdd)
  BinaryOpRegistry.register[Array[Int],Array[Float],OpAdd,Array[Float]]
        
  
  implicit object OpSubArrayIntArrayFloat extends GeneratedBinaryOp[Array[Int],Array[Float],OpSub,Array[Float]](OpSub)
  BinaryOpRegistry.register[Array[Int],Array[Float],OpSub,Array[Float]]
        
  
  implicit object OpMulArrayIntArrayFloat extends GeneratedBinaryOp[Array[Int],Array[Float],OpMul,Array[Float]](OpMul)
  BinaryOpRegistry.register[Array[Int],Array[Float],OpMul,Array[Float]]
        
  
  implicit object OpDivArrayIntArrayFloat extends GeneratedBinaryOp[Array[Int],Array[Float],OpDiv,Array[Float]](OpDiv)
  BinaryOpRegistry.register[Array[Int],Array[Float],OpDiv,Array[Float]]
        
  
  implicit object OpAddArrayIntArrayDouble extends GeneratedBinaryOp[Array[Int],Array[Double],OpAdd,Array[Double]](OpAdd)
  BinaryOpRegistry.register[Array[Int],Array[Double],OpAdd,Array[Double]]
        
  
  implicit object OpSubArrayIntArrayDouble extends GeneratedBinaryOp[Array[Int],Array[Double],OpSub,Array[Double]](OpSub)
  BinaryOpRegistry.register[Array[Int],Array[Double],OpSub,Array[Double]]
        
  
  implicit object OpMulArrayIntArrayDouble extends GeneratedBinaryOp[Array[Int],Array[Double],OpMul,Array[Double]](OpMul)
  BinaryOpRegistry.register[Array[Int],Array[Double],OpMul,Array[Double]]
        
  
  implicit object OpDivArrayIntArrayDouble extends GeneratedBinaryOp[Array[Int],Array[Double],OpDiv,Array[Double]](OpDiv)
  BinaryOpRegistry.register[Array[Int],Array[Double],OpDiv,Array[Double]]
        
  
  implicit object OpAddArrayLongArrayInt extends GeneratedBinaryOp[Array[Long],Array[Int],OpAdd,Array[Long]](OpAdd)
  BinaryOpRegistry.register[Array[Long],Array[Int],OpAdd,Array[Long]]
        
  
  implicit object OpSubArrayLongArrayInt extends GeneratedBinaryOp[Array[Long],Array[Int],OpSub,Array[Long]](OpSub)
  BinaryOpRegistry.register[Array[Long],Array[Int],OpSub,Array[Long]]
        
  
  implicit object OpMulArrayLongArrayInt extends GeneratedBinaryOp[Array[Long],Array[Int],OpMul,Array[Long]](OpMul)
  BinaryOpRegistry.register[Array[Long],Array[Int],OpMul,Array[Long]]
        
  
  implicit object OpDivArrayLongArrayInt extends GeneratedBinaryOp[Array[Long],Array[Int],OpDiv,Array[Long]](OpDiv)
  BinaryOpRegistry.register[Array[Long],Array[Int],OpDiv,Array[Long]]
        
  
  implicit object OpAddArrayLongArrayLong extends GeneratedBinaryOp[Array[Long],Array[Long],OpAdd,Array[Long]](OpAdd)
  BinaryOpRegistry.register[Array[Long],Array[Long],OpAdd,Array[Long]]
        
  
  implicit object OpSubArrayLongArrayLong extends GeneratedBinaryOp[Array[Long],Array[Long],OpSub,Array[Long]](OpSub)
  BinaryOpRegistry.register[Array[Long],Array[Long],OpSub,Array[Long]]
        
  
  implicit object OpMulArrayLongArrayLong extends GeneratedBinaryOp[Array[Long],Array[Long],OpMul,Array[Long]](OpMul)
  BinaryOpRegistry.register[Array[Long],Array[Long],OpMul,Array[Long]]
        
  
  implicit object OpDivArrayLongArrayLong extends GeneratedBinaryOp[Array[Long],Array[Long],OpDiv,Array[Long]](OpDiv)
  BinaryOpRegistry.register[Array[Long],Array[Long],OpDiv,Array[Long]]
        
  
  implicit object OpAddArrayLongArrayFloat extends GeneratedBinaryOp[Array[Long],Array[Float],OpAdd,Array[Float]](OpAdd)
  BinaryOpRegistry.register[Array[Long],Array[Float],OpAdd,Array[Float]]
        
  
  implicit object OpSubArrayLongArrayFloat extends GeneratedBinaryOp[Array[Long],Array[Float],OpSub,Array[Float]](OpSub)
  BinaryOpRegistry.register[Array[Long],Array[Float],OpSub,Array[Float]]
        
  
  implicit object OpMulArrayLongArrayFloat extends GeneratedBinaryOp[Array[Long],Array[Float],OpMul,Array[Float]](OpMul)
  BinaryOpRegistry.register[Array[Long],Array[Float],OpMul,Array[Float]]
        
  
  implicit object OpDivArrayLongArrayFloat extends GeneratedBinaryOp[Array[Long],Array[Float],OpDiv,Array[Float]](OpDiv)
  BinaryOpRegistry.register[Array[Long],Array[Float],OpDiv,Array[Float]]
        
  
  implicit object OpAddArrayLongArrayDouble extends GeneratedBinaryOp[Array[Long],Array[Double],OpAdd,Array[Double]](OpAdd)
  BinaryOpRegistry.register[Array[Long],Array[Double],OpAdd,Array[Double]]
        
  
  implicit object OpSubArrayLongArrayDouble extends GeneratedBinaryOp[Array[Long],Array[Double],OpSub,Array[Double]](OpSub)
  BinaryOpRegistry.register[Array[Long],Array[Double],OpSub,Array[Double]]
        
  
  implicit object OpMulArrayLongArrayDouble extends GeneratedBinaryOp[Array[Long],Array[Double],OpMul,Array[Double]](OpMul)
  BinaryOpRegistry.register[Array[Long],Array[Double],OpMul,Array[Double]]
        
  
  implicit object OpDivArrayLongArrayDouble extends GeneratedBinaryOp[Array[Long],Array[Double],OpDiv,Array[Double]](OpDiv)
  BinaryOpRegistry.register[Array[Long],Array[Double],OpDiv,Array[Double]]
        
  
  implicit object OpAddArrayFloatArrayInt extends GeneratedBinaryOp[Array[Float],Array[Int],OpAdd,Array[Float]](OpAdd)
  BinaryOpRegistry.register[Array[Float],Array[Int],OpAdd,Array[Float]]
        
  
  implicit object OpSubArrayFloatArrayInt extends GeneratedBinaryOp[Array[Float],Array[Int],OpSub,Array[Float]](OpSub)
  BinaryOpRegistry.register[Array[Float],Array[Int],OpSub,Array[Float]]
        
  
  implicit object OpMulArrayFloatArrayInt extends GeneratedBinaryOp[Array[Float],Array[Int],OpMul,Array[Float]](OpMul)
  BinaryOpRegistry.register[Array[Float],Array[Int],OpMul,Array[Float]]
        
  
  implicit object OpDivArrayFloatArrayInt extends GeneratedBinaryOp[Array[Float],Array[Int],OpDiv,Array[Float]](OpDiv)
  BinaryOpRegistry.register[Array[Float],Array[Int],OpDiv,Array[Float]]
        
  
  implicit object OpAddArrayFloatArrayLong extends GeneratedBinaryOp[Array[Float],Array[Long],OpAdd,Array[Float]](OpAdd)
  BinaryOpRegistry.register[Array[Float],Array[Long],OpAdd,Array[Float]]
        
  
  implicit object OpSubArrayFloatArrayLong extends GeneratedBinaryOp[Array[Float],Array[Long],OpSub,Array[Float]](OpSub)
  BinaryOpRegistry.register[Array[Float],Array[Long],OpSub,Array[Float]]
        
  
  implicit object OpMulArrayFloatArrayLong extends GeneratedBinaryOp[Array[Float],Array[Long],OpMul,Array[Float]](OpMul)
  BinaryOpRegistry.register[Array[Float],Array[Long],OpMul,Array[Float]]
        
  
  implicit object OpDivArrayFloatArrayLong extends GeneratedBinaryOp[Array[Float],Array[Long],OpDiv,Array[Float]](OpDiv)
  BinaryOpRegistry.register[Array[Float],Array[Long],OpDiv,Array[Float]]
        
  
  implicit object OpAddArrayFloatArrayFloat extends GeneratedBinaryOp[Array[Float],Array[Float],OpAdd,Array[Float]](OpAdd)
  BinaryOpRegistry.register[Array[Float],Array[Float],OpAdd,Array[Float]]
        
  
  implicit object OpSubArrayFloatArrayFloat extends GeneratedBinaryOp[Array[Float],Array[Float],OpSub,Array[Float]](OpSub)
  BinaryOpRegistry.register[Array[Float],Array[Float],OpSub,Array[Float]]
        
  
  implicit object OpMulArrayFloatArrayFloat extends GeneratedBinaryOp[Array[Float],Array[Float],OpMul,Array[Float]](OpMul)
  BinaryOpRegistry.register[Array[Float],Array[Float],OpMul,Array[Float]]
        
  
  implicit object OpDivArrayFloatArrayFloat extends GeneratedBinaryOp[Array[Float],Array[Float],OpDiv,Array[Float]](OpDiv)
  BinaryOpRegistry.register[Array[Float],Array[Float],OpDiv,Array[Float]]
        
  
  implicit object OpAddArrayFloatArrayDouble extends GeneratedBinaryOp[Array[Float],Array[Double],OpAdd,Array[Double]](OpAdd)
  BinaryOpRegistry.register[Array[Float],Array[Double],OpAdd,Array[Double]]
        
  
  implicit object OpSubArrayFloatArrayDouble extends GeneratedBinaryOp[Array[Float],Array[Double],OpSub,Array[Double]](OpSub)
  BinaryOpRegistry.register[Array[Float],Array[Double],OpSub,Array[Double]]
        
  
  implicit object OpMulArrayFloatArrayDouble extends GeneratedBinaryOp[Array[Float],Array[Double],OpMul,Array[Double]](OpMul)
  BinaryOpRegistry.register[Array[Float],Array[Double],OpMul,Array[Double]]
        
  
  implicit object OpDivArrayFloatArrayDouble extends GeneratedBinaryOp[Array[Float],Array[Double],OpDiv,Array[Double]](OpDiv)
  BinaryOpRegistry.register[Array[Float],Array[Double],OpDiv,Array[Double]]
        
  
  implicit object OpAddArrayDoubleArrayInt extends GeneratedBinaryOp[Array[Double],Array[Int],OpAdd,Array[Double]](OpAdd)
  BinaryOpRegistry.register[Array[Double],Array[Int],OpAdd,Array[Double]]
        
  
  implicit object OpSubArrayDoubleArrayInt extends GeneratedBinaryOp[Array[Double],Array[Int],OpSub,Array[Double]](OpSub)
  BinaryOpRegistry.register[Array[Double],Array[Int],OpSub,Array[Double]]
        
  
  implicit object OpMulArrayDoubleArrayInt extends GeneratedBinaryOp[Array[Double],Array[Int],OpMul,Array[Double]](OpMul)
  BinaryOpRegistry.register[Array[Double],Array[Int],OpMul,Array[Double]]
        
  
  implicit object OpDivArrayDoubleArrayInt extends GeneratedBinaryOp[Array[Double],Array[Int],OpDiv,Array[Double]](OpDiv)
  BinaryOpRegistry.register[Array[Double],Array[Int],OpDiv,Array[Double]]
        
  
  implicit object OpAddArrayDoubleArrayLong extends GeneratedBinaryOp[Array[Double],Array[Long],OpAdd,Array[Double]](OpAdd)
  BinaryOpRegistry.register[Array[Double],Array[Long],OpAdd,Array[Double]]
        
  
  implicit object OpSubArrayDoubleArrayLong extends GeneratedBinaryOp[Array[Double],Array[Long],OpSub,Array[Double]](OpSub)
  BinaryOpRegistry.register[Array[Double],Array[Long],OpSub,Array[Double]]
        
  
  implicit object OpMulArrayDoubleArrayLong extends GeneratedBinaryOp[Array[Double],Array[Long],OpMul,Array[Double]](OpMul)
  BinaryOpRegistry.register[Array[Double],Array[Long],OpMul,Array[Double]]
        
  
  implicit object OpDivArrayDoubleArrayLong extends GeneratedBinaryOp[Array[Double],Array[Long],OpDiv,Array[Double]](OpDiv)
  BinaryOpRegistry.register[Array[Double],Array[Long],OpDiv,Array[Double]]
        
  
  implicit object OpAddArrayDoubleArrayFloat extends GeneratedBinaryOp[Array[Double],Array[Float],OpAdd,Array[Double]](OpAdd)
  BinaryOpRegistry.register[Array[Double],Array[Float],OpAdd,Array[Double]]
        
  
  implicit object OpSubArrayDoubleArrayFloat extends GeneratedBinaryOp[Array[Double],Array[Float],OpSub,Array[Double]](OpSub)
  BinaryOpRegistry.register[Array[Double],Array[Float],OpSub,Array[Double]]
        
  
  implicit object OpMulArrayDoubleArrayFloat extends GeneratedBinaryOp[Array[Double],Array[Float],OpMul,Array[Double]](OpMul)
  BinaryOpRegistry.register[Array[Double],Array[Float],OpMul,Array[Double]]
        
  
  implicit object OpDivArrayDoubleArrayFloat extends GeneratedBinaryOp[Array[Double],Array[Float],OpDiv,Array[Double]](OpDiv)
  BinaryOpRegistry.register[Array[Double],Array[Float],OpDiv,Array[Double]]
        
  
  implicit object OpAddArrayDoubleArrayDouble extends GeneratedBinaryOp[Array[Double],Array[Double],OpAdd,Array[Double]](OpAdd)
  BinaryOpRegistry.register[Array[Double],Array[Double],OpAdd,Array[Double]]
        
  
  implicit object OpSubArrayDoubleArrayDouble extends GeneratedBinaryOp[Array[Double],Array[Double],OpSub,Array[Double]](OpSub)
  BinaryOpRegistry.register[Array[Double],Array[Double],OpSub,Array[Double]]
        
  
  implicit object OpMulArrayDoubleArrayDouble extends GeneratedBinaryOp[Array[Double],Array[Double],OpMul,Array[Double]](OpMul)
  BinaryOpRegistry.register[Array[Double],Array[Double],OpMul,Array[Double]]
        
  
  implicit object OpDivArrayDoubleArrayDouble extends GeneratedBinaryOp[Array[Double],Array[Double],OpDiv,Array[Double]](OpDiv)
  BinaryOpRegistry.register[Array[Double],Array[Double],OpDiv,Array[Double]]
        
  
  implicit object OpAddDenseVectorRowIntDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Int],OpAdd,DenseVectorRow[Int]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Int],OpAdd,DenseVectorRow[Int]]
        
  
  implicit object OpSubDenseVectorRowIntDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Int],OpSub,DenseVectorRow[Int]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Int],OpSub,DenseVectorRow[Int]]
        
  
  implicit object OpMulDenseVectorRowIntDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Int],OpMul,DenseVectorRow[Int]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Int],OpMul,DenseVectorRow[Int]]
        
  
  implicit object OpDivDenseVectorRowIntDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Int],OpDiv,DenseVectorRow[Int]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Int],OpDiv,DenseVectorRow[Int]]
        
  
  implicit object OpAddDenseVectorRowIntDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Long],OpAdd,DenseVectorRow[Long]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Long],OpAdd,DenseVectorRow[Long]]
        
  
  implicit object OpSubDenseVectorRowIntDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Long],OpSub,DenseVectorRow[Long]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Long],OpSub,DenseVectorRow[Long]]
        
  
  implicit object OpMulDenseVectorRowIntDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Long],OpMul,DenseVectorRow[Long]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Long],OpMul,DenseVectorRow[Long]]
        
  
  implicit object OpDivDenseVectorRowIntDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Long],OpDiv,DenseVectorRow[Long]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Long],OpDiv,DenseVectorRow[Long]]
        
  
  implicit object OpAddDenseVectorRowIntDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Float],OpAdd,DenseVectorRow[Float]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Float],OpAdd,DenseVectorRow[Float]]
        
  
  implicit object OpSubDenseVectorRowIntDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Float],OpSub,DenseVectorRow[Float]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Float],OpSub,DenseVectorRow[Float]]
        
  
  implicit object OpMulDenseVectorRowIntDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Float],OpMul,DenseVectorRow[Float]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Float],OpMul,DenseVectorRow[Float]]
        
  
  implicit object OpDivDenseVectorRowIntDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Float],OpDiv,DenseVectorRow[Float]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Float],OpDiv,DenseVectorRow[Float]]
        
  
  implicit object OpAddDenseVectorRowIntDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Double],OpAdd,DenseVectorRow[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Double],OpAdd,DenseVectorRow[Double]]
        
  
  implicit object OpSubDenseVectorRowIntDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Double],OpSub,DenseVectorRow[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Double],OpSub,DenseVectorRow[Double]]
        
  
  implicit object OpMulDenseVectorRowIntDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Double],OpMul,DenseVectorRow[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Double],OpMul,DenseVectorRow[Double]]
        
  
  implicit object OpDivDenseVectorRowIntDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Int],DenseVectorRow[Double],OpDiv,DenseVectorRow[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Int],DenseVectorRow[Double],OpDiv,DenseVectorRow[Double]]
        
  
  implicit object OpAddDenseVectorRowLongDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Int],OpAdd,DenseVectorRow[Long]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Int],OpAdd,DenseVectorRow[Long]]
        
  
  implicit object OpSubDenseVectorRowLongDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Int],OpSub,DenseVectorRow[Long]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Int],OpSub,DenseVectorRow[Long]]
        
  
  implicit object OpMulDenseVectorRowLongDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Int],OpMul,DenseVectorRow[Long]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Int],OpMul,DenseVectorRow[Long]]
        
  
  implicit object OpDivDenseVectorRowLongDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Int],OpDiv,DenseVectorRow[Long]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Int],OpDiv,DenseVectorRow[Long]]
        
  
  implicit object OpAddDenseVectorRowLongDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Long],OpAdd,DenseVectorRow[Long]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Long],OpAdd,DenseVectorRow[Long]]
        
  
  implicit object OpSubDenseVectorRowLongDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Long],OpSub,DenseVectorRow[Long]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Long],OpSub,DenseVectorRow[Long]]
        
  
  implicit object OpMulDenseVectorRowLongDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Long],OpMul,DenseVectorRow[Long]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Long],OpMul,DenseVectorRow[Long]]
        
  
  implicit object OpDivDenseVectorRowLongDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Long],OpDiv,DenseVectorRow[Long]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Long],OpDiv,DenseVectorRow[Long]]
        
  
  implicit object OpAddDenseVectorRowLongDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Float],OpAdd,DenseVectorRow[Float]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Float],OpAdd,DenseVectorRow[Float]]
        
  
  implicit object OpSubDenseVectorRowLongDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Float],OpSub,DenseVectorRow[Float]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Float],OpSub,DenseVectorRow[Float]]
        
  
  implicit object OpMulDenseVectorRowLongDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Float],OpMul,DenseVectorRow[Float]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Float],OpMul,DenseVectorRow[Float]]
        
  
  implicit object OpDivDenseVectorRowLongDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Float],OpDiv,DenseVectorRow[Float]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Float],OpDiv,DenseVectorRow[Float]]
        
  
  implicit object OpAddDenseVectorRowLongDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Double],OpAdd,DenseVectorRow[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Double],OpAdd,DenseVectorRow[Double]]
        
  
  implicit object OpSubDenseVectorRowLongDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Double],OpSub,DenseVectorRow[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Double],OpSub,DenseVectorRow[Double]]
        
  
  implicit object OpMulDenseVectorRowLongDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Double],OpMul,DenseVectorRow[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Double],OpMul,DenseVectorRow[Double]]
        
  
  implicit object OpDivDenseVectorRowLongDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Long],DenseVectorRow[Double],OpDiv,DenseVectorRow[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Long],DenseVectorRow[Double],OpDiv,DenseVectorRow[Double]]
        
  
  implicit object OpAddDenseVectorRowFloatDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Int],OpAdd,DenseVectorRow[Float]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Int],OpAdd,DenseVectorRow[Float]]
        
  
  implicit object OpSubDenseVectorRowFloatDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Int],OpSub,DenseVectorRow[Float]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Int],OpSub,DenseVectorRow[Float]]
        
  
  implicit object OpMulDenseVectorRowFloatDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Int],OpMul,DenseVectorRow[Float]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Int],OpMul,DenseVectorRow[Float]]
        
  
  implicit object OpDivDenseVectorRowFloatDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Int],OpDiv,DenseVectorRow[Float]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Int],OpDiv,DenseVectorRow[Float]]
        
  
  implicit object OpAddDenseVectorRowFloatDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Long],OpAdd,DenseVectorRow[Float]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Long],OpAdd,DenseVectorRow[Float]]
        
  
  implicit object OpSubDenseVectorRowFloatDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Long],OpSub,DenseVectorRow[Float]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Long],OpSub,DenseVectorRow[Float]]
        
  
  implicit object OpMulDenseVectorRowFloatDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Long],OpMul,DenseVectorRow[Float]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Long],OpMul,DenseVectorRow[Float]]
        
  
  implicit object OpDivDenseVectorRowFloatDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Long],OpDiv,DenseVectorRow[Float]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Long],OpDiv,DenseVectorRow[Float]]
        
  
  implicit object OpAddDenseVectorRowFloatDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Float],OpAdd,DenseVectorRow[Float]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Float],OpAdd,DenseVectorRow[Float]]
        
  
  implicit object OpSubDenseVectorRowFloatDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Float],OpSub,DenseVectorRow[Float]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Float],OpSub,DenseVectorRow[Float]]
        
  
  implicit object OpMulDenseVectorRowFloatDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Float],OpMul,DenseVectorRow[Float]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Float],OpMul,DenseVectorRow[Float]]
        
  
  implicit object OpDivDenseVectorRowFloatDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Float],OpDiv,DenseVectorRow[Float]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Float],OpDiv,DenseVectorRow[Float]]
        
  
  implicit object OpAddDenseVectorRowFloatDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Double],OpAdd,DenseVectorRow[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Double],OpAdd,DenseVectorRow[Double]]
        
  
  implicit object OpSubDenseVectorRowFloatDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Double],OpSub,DenseVectorRow[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Double],OpSub,DenseVectorRow[Double]]
        
  
  implicit object OpMulDenseVectorRowFloatDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Double],OpMul,DenseVectorRow[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Double],OpMul,DenseVectorRow[Double]]
        
  
  implicit object OpDivDenseVectorRowFloatDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Float],DenseVectorRow[Double],OpDiv,DenseVectorRow[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Float],DenseVectorRow[Double],OpDiv,DenseVectorRow[Double]]
        
  
  implicit object OpAddDenseVectorRowDoubleDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Int],OpAdd,DenseVectorRow[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Int],OpAdd,DenseVectorRow[Double]]
        
  
  implicit object OpSubDenseVectorRowDoubleDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Int],OpSub,DenseVectorRow[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Int],OpSub,DenseVectorRow[Double]]
        
  
  implicit object OpMulDenseVectorRowDoubleDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Int],OpMul,DenseVectorRow[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Int],OpMul,DenseVectorRow[Double]]
        
  
  implicit object OpDivDenseVectorRowDoubleDenseVectorRowInt extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Int],OpDiv,DenseVectorRow[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Int],OpDiv,DenseVectorRow[Double]]
        
  
  implicit object OpAddDenseVectorRowDoubleDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Long],OpAdd,DenseVectorRow[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Long],OpAdd,DenseVectorRow[Double]]
        
  
  implicit object OpSubDenseVectorRowDoubleDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Long],OpSub,DenseVectorRow[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Long],OpSub,DenseVectorRow[Double]]
        
  
  implicit object OpMulDenseVectorRowDoubleDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Long],OpMul,DenseVectorRow[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Long],OpMul,DenseVectorRow[Double]]
        
  
  implicit object OpDivDenseVectorRowDoubleDenseVectorRowLong extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Long],OpDiv,DenseVectorRow[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Long],OpDiv,DenseVectorRow[Double]]
        
  
  implicit object OpAddDenseVectorRowDoubleDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Float],OpAdd,DenseVectorRow[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Float],OpAdd,DenseVectorRow[Double]]
        
  
  implicit object OpSubDenseVectorRowDoubleDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Float],OpSub,DenseVectorRow[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Float],OpSub,DenseVectorRow[Double]]
        
  
  implicit object OpMulDenseVectorRowDoubleDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Float],OpMul,DenseVectorRow[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Float],OpMul,DenseVectorRow[Double]]
        
  
  implicit object OpDivDenseVectorRowDoubleDenseVectorRowFloat extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Float],OpDiv,DenseVectorRow[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Float],OpDiv,DenseVectorRow[Double]]
        
  
  implicit object OpAddDenseVectorRowDoubleDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Double],OpAdd,DenseVectorRow[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Double],OpAdd,DenseVectorRow[Double]]
        
  
  implicit object OpSubDenseVectorRowDoubleDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Double],OpSub,DenseVectorRow[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Double],OpSub,DenseVectorRow[Double]]
        
  
  implicit object OpMulDenseVectorRowDoubleDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Double],OpMul,DenseVectorRow[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Double],OpMul,DenseVectorRow[Double]]
        
  
  implicit object OpDivDenseVectorRowDoubleDenseVectorRowDouble extends GeneratedBinaryOp[DenseVectorRow[Double],DenseVectorRow[Double],OpDiv,DenseVectorRow[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorRow[Double],DenseVectorRow[Double],OpDiv,DenseVectorRow[Double]]
        
  
  implicit object OpAddDenseVectorColIntDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Int],OpAdd,DenseVectorCol[Int]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Int],OpAdd,DenseVectorCol[Int]]
        
  
  implicit object OpSubDenseVectorColIntDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Int],OpSub,DenseVectorCol[Int]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Int],OpSub,DenseVectorCol[Int]]
        
  
  implicit object OpMulDenseVectorColIntDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Int],OpMul,DenseVectorCol[Int]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Int],OpMul,DenseVectorCol[Int]]
        
  
  implicit object OpDivDenseVectorColIntDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Int],OpDiv,DenseVectorCol[Int]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Int],OpDiv,DenseVectorCol[Int]]
        
  
  implicit object OpAddDenseVectorColIntDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Long],OpAdd,DenseVectorCol[Long]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Long],OpAdd,DenseVectorCol[Long]]
        
  
  implicit object OpSubDenseVectorColIntDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Long],OpSub,DenseVectorCol[Long]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Long],OpSub,DenseVectorCol[Long]]
        
  
  implicit object OpMulDenseVectorColIntDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Long],OpMul,DenseVectorCol[Long]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Long],OpMul,DenseVectorCol[Long]]
        
  
  implicit object OpDivDenseVectorColIntDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Long],OpDiv,DenseVectorCol[Long]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Long],OpDiv,DenseVectorCol[Long]]
        
  
  implicit object OpAddDenseVectorColIntDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Float],OpAdd,DenseVectorCol[Float]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Float],OpAdd,DenseVectorCol[Float]]
        
  
  implicit object OpSubDenseVectorColIntDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Float],OpSub,DenseVectorCol[Float]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Float],OpSub,DenseVectorCol[Float]]
        
  
  implicit object OpMulDenseVectorColIntDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Float],OpMul,DenseVectorCol[Float]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Float],OpMul,DenseVectorCol[Float]]
        
  
  implicit object OpDivDenseVectorColIntDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Float],OpDiv,DenseVectorCol[Float]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Float],OpDiv,DenseVectorCol[Float]]
        
  
  implicit object OpAddDenseVectorColIntDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Double],OpAdd,DenseVectorCol[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Double],OpAdd,DenseVectorCol[Double]]
        
  
  implicit object OpSubDenseVectorColIntDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Double],OpSub,DenseVectorCol[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Double],OpSub,DenseVectorCol[Double]]
        
  
  implicit object OpMulDenseVectorColIntDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Double],OpMul,DenseVectorCol[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Double],OpMul,DenseVectorCol[Double]]
        
  
  implicit object OpDivDenseVectorColIntDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Int],DenseVectorCol[Double],OpDiv,DenseVectorCol[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Int],DenseVectorCol[Double],OpDiv,DenseVectorCol[Double]]
        
  
  implicit object OpAddDenseVectorColLongDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Int],OpAdd,DenseVectorCol[Long]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Int],OpAdd,DenseVectorCol[Long]]
        
  
  implicit object OpSubDenseVectorColLongDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Int],OpSub,DenseVectorCol[Long]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Int],OpSub,DenseVectorCol[Long]]
        
  
  implicit object OpMulDenseVectorColLongDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Int],OpMul,DenseVectorCol[Long]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Int],OpMul,DenseVectorCol[Long]]
        
  
  implicit object OpDivDenseVectorColLongDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Int],OpDiv,DenseVectorCol[Long]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Int],OpDiv,DenseVectorCol[Long]]
        
  
  implicit object OpAddDenseVectorColLongDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Long],OpAdd,DenseVectorCol[Long]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Long],OpAdd,DenseVectorCol[Long]]
        
  
  implicit object OpSubDenseVectorColLongDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Long],OpSub,DenseVectorCol[Long]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Long],OpSub,DenseVectorCol[Long]]
        
  
  implicit object OpMulDenseVectorColLongDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Long],OpMul,DenseVectorCol[Long]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Long],OpMul,DenseVectorCol[Long]]
        
  
  implicit object OpDivDenseVectorColLongDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Long],OpDiv,DenseVectorCol[Long]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Long],OpDiv,DenseVectorCol[Long]]
        
  
  implicit object OpAddDenseVectorColLongDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Float],OpAdd,DenseVectorCol[Float]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Float],OpAdd,DenseVectorCol[Float]]
        
  
  implicit object OpSubDenseVectorColLongDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Float],OpSub,DenseVectorCol[Float]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Float],OpSub,DenseVectorCol[Float]]
        
  
  implicit object OpMulDenseVectorColLongDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Float],OpMul,DenseVectorCol[Float]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Float],OpMul,DenseVectorCol[Float]]
        
  
  implicit object OpDivDenseVectorColLongDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Float],OpDiv,DenseVectorCol[Float]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Float],OpDiv,DenseVectorCol[Float]]
        
  
  implicit object OpAddDenseVectorColLongDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Double],OpAdd,DenseVectorCol[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Double],OpAdd,DenseVectorCol[Double]]
        
  
  implicit object OpSubDenseVectorColLongDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Double],OpSub,DenseVectorCol[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Double],OpSub,DenseVectorCol[Double]]
        
  
  implicit object OpMulDenseVectorColLongDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Double],OpMul,DenseVectorCol[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Double],OpMul,DenseVectorCol[Double]]
        
  
  implicit object OpDivDenseVectorColLongDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Long],DenseVectorCol[Double],OpDiv,DenseVectorCol[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Long],DenseVectorCol[Double],OpDiv,DenseVectorCol[Double]]
        
  
  implicit object OpAddDenseVectorColFloatDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Int],OpAdd,DenseVectorCol[Float]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Int],OpAdd,DenseVectorCol[Float]]
        
  
  implicit object OpSubDenseVectorColFloatDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Int],OpSub,DenseVectorCol[Float]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Int],OpSub,DenseVectorCol[Float]]
        
  
  implicit object OpMulDenseVectorColFloatDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Int],OpMul,DenseVectorCol[Float]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Int],OpMul,DenseVectorCol[Float]]
        
  
  implicit object OpDivDenseVectorColFloatDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Int],OpDiv,DenseVectorCol[Float]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Int],OpDiv,DenseVectorCol[Float]]
        
  
  implicit object OpAddDenseVectorColFloatDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Long],OpAdd,DenseVectorCol[Float]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Long],OpAdd,DenseVectorCol[Float]]
        
  
  implicit object OpSubDenseVectorColFloatDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Long],OpSub,DenseVectorCol[Float]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Long],OpSub,DenseVectorCol[Float]]
        
  
  implicit object OpMulDenseVectorColFloatDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Long],OpMul,DenseVectorCol[Float]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Long],OpMul,DenseVectorCol[Float]]
        
  
  implicit object OpDivDenseVectorColFloatDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Long],OpDiv,DenseVectorCol[Float]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Long],OpDiv,DenseVectorCol[Float]]
        
  
  implicit object OpAddDenseVectorColFloatDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Float],OpAdd,DenseVectorCol[Float]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Float],OpAdd,DenseVectorCol[Float]]
        
  
  implicit object OpSubDenseVectorColFloatDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Float],OpSub,DenseVectorCol[Float]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Float],OpSub,DenseVectorCol[Float]]
        
  
  implicit object OpMulDenseVectorColFloatDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Float],OpMul,DenseVectorCol[Float]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Float],OpMul,DenseVectorCol[Float]]
        
  
  implicit object OpDivDenseVectorColFloatDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Float],OpDiv,DenseVectorCol[Float]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Float],OpDiv,DenseVectorCol[Float]]
        
  
  implicit object OpAddDenseVectorColFloatDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Double],OpAdd,DenseVectorCol[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Double],OpAdd,DenseVectorCol[Double]]
        
  
  implicit object OpSubDenseVectorColFloatDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Double],OpSub,DenseVectorCol[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Double],OpSub,DenseVectorCol[Double]]
        
  
  implicit object OpMulDenseVectorColFloatDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Double],OpMul,DenseVectorCol[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Double],OpMul,DenseVectorCol[Double]]
        
  
  implicit object OpDivDenseVectorColFloatDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Float],DenseVectorCol[Double],OpDiv,DenseVectorCol[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Float],DenseVectorCol[Double],OpDiv,DenseVectorCol[Double]]
        
  
  implicit object OpAddDenseVectorColDoubleDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Int],OpAdd,DenseVectorCol[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Int],OpAdd,DenseVectorCol[Double]]
        
  
  implicit object OpSubDenseVectorColDoubleDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Int],OpSub,DenseVectorCol[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Int],OpSub,DenseVectorCol[Double]]
        
  
  implicit object OpMulDenseVectorColDoubleDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Int],OpMul,DenseVectorCol[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Int],OpMul,DenseVectorCol[Double]]
        
  
  implicit object OpDivDenseVectorColDoubleDenseVectorColInt extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Int],OpDiv,DenseVectorCol[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Int],OpDiv,DenseVectorCol[Double]]
        
  
  implicit object OpAddDenseVectorColDoubleDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Long],OpAdd,DenseVectorCol[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Long],OpAdd,DenseVectorCol[Double]]
        
  
  implicit object OpSubDenseVectorColDoubleDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Long],OpSub,DenseVectorCol[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Long],OpSub,DenseVectorCol[Double]]
        
  
  implicit object OpMulDenseVectorColDoubleDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Long],OpMul,DenseVectorCol[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Long],OpMul,DenseVectorCol[Double]]
        
  
  implicit object OpDivDenseVectorColDoubleDenseVectorColLong extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Long],OpDiv,DenseVectorCol[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Long],OpDiv,DenseVectorCol[Double]]
        
  
  implicit object OpAddDenseVectorColDoubleDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Float],OpAdd,DenseVectorCol[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Float],OpAdd,DenseVectorCol[Double]]
        
  
  implicit object OpSubDenseVectorColDoubleDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Float],OpSub,DenseVectorCol[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Float],OpSub,DenseVectorCol[Double]]
        
  
  implicit object OpMulDenseVectorColDoubleDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Float],OpMul,DenseVectorCol[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Float],OpMul,DenseVectorCol[Double]]
        
  
  implicit object OpDivDenseVectorColDoubleDenseVectorColFloat extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Float],OpDiv,DenseVectorCol[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Float],OpDiv,DenseVectorCol[Double]]
        
  
  implicit object OpAddDenseVectorColDoubleDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Double],OpAdd,DenseVectorCol[Double]](OpAdd)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Double],OpAdd,DenseVectorCol[Double]]
        
  
  implicit object OpSubDenseVectorColDoubleDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Double],OpSub,DenseVectorCol[Double]](OpSub)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Double],OpSub,DenseVectorCol[Double]]
        
  
  implicit object OpMulDenseVectorColDoubleDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Double],OpMul,DenseVectorCol[Double]](OpMul)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Double],OpMul,DenseVectorCol[Double]]
        
  
  implicit object OpDivDenseVectorColDoubleDenseVectorColDouble extends GeneratedBinaryOp[DenseVectorCol[Double],DenseVectorCol[Double],OpDiv,DenseVectorCol[Double]](OpDiv)
  BinaryOpRegistry.register[DenseVectorCol[Double],DenseVectorCol[Double],OpDiv,DenseVectorCol[Double]]
        
  implicit object OpAddArrayIntInt extends GeneratedBinaryOp[Array[Int],Int,OpAdd,Array[Int]](OpAdd)
  implicit object OpSubArrayIntInt extends GeneratedBinaryOp[Array[Int],Int,OpSub,Array[Int]](OpSub)
  implicit object OpMulArrayIntInt extends GeneratedBinaryOp[Array[Int],Int,OpMul,Array[Int]](OpMul)
  implicit object OpDivArrayIntInt extends GeneratedBinaryOp[Array[Int],Int,OpDiv,Array[Int]](OpDiv)
  implicit object OpAddArrayIntLong extends GeneratedBinaryOp[Array[Int],Long,OpAdd,Array[Long]](OpAdd)
  implicit object OpSubArrayIntLong extends GeneratedBinaryOp[Array[Int],Long,OpSub,Array[Long]](OpSub)
  implicit object OpMulArrayIntLong extends GeneratedBinaryOp[Array[Int],Long,OpMul,Array[Long]](OpMul)
  implicit object OpDivArrayIntLong extends GeneratedBinaryOp[Array[Int],Long,OpDiv,Array[Long]](OpDiv)
  implicit object OpAddArrayIntFloat extends GeneratedBinaryOp[Array[Int],Float,OpAdd,Array[Float]](OpAdd)
  implicit object OpSubArrayIntFloat extends GeneratedBinaryOp[Array[Int],Float,OpSub,Array[Float]](OpSub)
  implicit object OpMulArrayIntFloat extends GeneratedBinaryOp[Array[Int],Float,OpMul,Array[Float]](OpMul)
  implicit object OpDivArrayIntFloat extends GeneratedBinaryOp[Array[Int],Float,OpDiv,Array[Float]](OpDiv)
  implicit object OpAddArrayIntDouble extends GeneratedBinaryOp[Array[Int],Double,OpAdd,Array[Double]](OpAdd)
  implicit object OpSubArrayIntDouble extends GeneratedBinaryOp[Array[Int],Double,OpSub,Array[Double]](OpSub)
  implicit object OpMulArrayIntDouble extends GeneratedBinaryOp[Array[Int],Double,OpMul,Array[Double]](OpMul)
  implicit object OpDivArrayIntDouble extends GeneratedBinaryOp[Array[Int],Double,OpDiv,Array[Double]](OpDiv)
  implicit object OpAddArrayLongInt extends GeneratedBinaryOp[Array[Long],Int,OpAdd,Array[Long]](OpAdd)
  implicit object OpSubArrayLongInt extends GeneratedBinaryOp[Array[Long],Int,OpSub,Array[Long]](OpSub)
  implicit object OpMulArrayLongInt extends GeneratedBinaryOp[Array[Long],Int,OpMul,Array[Long]](OpMul)
  implicit object OpDivArrayLongInt extends GeneratedBinaryOp[Array[Long],Int,OpDiv,Array[Long]](OpDiv)
  implicit object OpAddArrayLongLong extends GeneratedBinaryOp[Array[Long],Long,OpAdd,Array[Long]](OpAdd)
  implicit object OpSubArrayLongLong extends GeneratedBinaryOp[Array[Long],Long,OpSub,Array[Long]](OpSub)
  implicit object OpMulArrayLongLong extends GeneratedBinaryOp[Array[Long],Long,OpMul,Array[Long]](OpMul)
  implicit object OpDivArrayLongLong extends GeneratedBinaryOp[Array[Long],Long,OpDiv,Array[Long]](OpDiv)
  implicit object OpAddArrayLongFloat extends GeneratedBinaryOp[Array[Long],Float,OpAdd,Array[Float]](OpAdd)
  implicit object OpSubArrayLongFloat extends GeneratedBinaryOp[Array[Long],Float,OpSub,Array[Float]](OpSub)
  implicit object OpMulArrayLongFloat extends GeneratedBinaryOp[Array[Long],Float,OpMul,Array[Float]](OpMul)
  implicit object OpDivArrayLongFloat extends GeneratedBinaryOp[Array[Long],Float,OpDiv,Array[Float]](OpDiv)
  implicit object OpAddArrayLongDouble extends GeneratedBinaryOp[Array[Long],Double,OpAdd,Array[Double]](OpAdd)
  implicit object OpSubArrayLongDouble extends GeneratedBinaryOp[Array[Long],Double,OpSub,Array[Double]](OpSub)
  implicit object OpMulArrayLongDouble extends GeneratedBinaryOp[Array[Long],Double,OpMul,Array[Double]](OpMul)
  implicit object OpDivArrayLongDouble extends GeneratedBinaryOp[Array[Long],Double,OpDiv,Array[Double]](OpDiv)
  implicit object OpAddArrayFloatInt extends GeneratedBinaryOp[Array[Float],Int,OpAdd,Array[Float]](OpAdd)
  implicit object OpSubArrayFloatInt extends GeneratedBinaryOp[Array[Float],Int,OpSub,Array[Float]](OpSub)
  implicit object OpMulArrayFloatInt extends GeneratedBinaryOp[Array[Float],Int,OpMul,Array[Float]](OpMul)
  implicit object OpDivArrayFloatInt extends GeneratedBinaryOp[Array[Float],Int,OpDiv,Array[Float]](OpDiv)
  implicit object OpAddArrayFloatLong extends GeneratedBinaryOp[Array[Float],Long,OpAdd,Array[Float]](OpAdd)
  implicit object OpSubArrayFloatLong extends GeneratedBinaryOp[Array[Float],Long,OpSub,Array[Float]](OpSub)
  implicit object OpMulArrayFloatLong extends GeneratedBinaryOp[Array[Float],Long,OpMul,Array[Float]](OpMul)
  implicit object OpDivArrayFloatLong extends GeneratedBinaryOp[Array[Float],Long,OpDiv,Array[Float]](OpDiv)
  implicit object OpAddArrayFloatFloat extends GeneratedBinaryOp[Array[Float],Float,OpAdd,Array[Float]](OpAdd)
  implicit object OpSubArrayFloatFloat extends GeneratedBinaryOp[Array[Float],Float,OpSub,Array[Float]](OpSub)
  implicit object OpMulArrayFloatFloat extends GeneratedBinaryOp[Array[Float],Float,OpMul,Array[Float]](OpMul)
  implicit object OpDivArrayFloatFloat extends GeneratedBinaryOp[Array[Float],Float,OpDiv,Array[Float]](OpDiv)
  implicit object OpAddArrayFloatDouble extends GeneratedBinaryOp[Array[Float],Double,OpAdd,Array[Double]](OpAdd)
  implicit object OpSubArrayFloatDouble extends GeneratedBinaryOp[Array[Float],Double,OpSub,Array[Double]](OpSub)
  implicit object OpMulArrayFloatDouble extends GeneratedBinaryOp[Array[Float],Double,OpMul,Array[Double]](OpMul)
  implicit object OpDivArrayFloatDouble extends GeneratedBinaryOp[Array[Float],Double,OpDiv,Array[Double]](OpDiv)
  implicit object OpAddArrayDoubleInt extends GeneratedBinaryOp[Array[Double],Int,OpAdd,Array[Double]](OpAdd)
  implicit object OpSubArrayDoubleInt extends GeneratedBinaryOp[Array[Double],Int,OpSub,Array[Double]](OpSub)
  implicit object OpMulArrayDoubleInt extends GeneratedBinaryOp[Array[Double],Int,OpMul,Array[Double]](OpMul)
  implicit object OpDivArrayDoubleInt extends GeneratedBinaryOp[Array[Double],Int,OpDiv,Array[Double]](OpDiv)
  implicit object OpAddArrayDoubleLong extends GeneratedBinaryOp[Array[Double],Long,OpAdd,Array[Double]](OpAdd)
  implicit object OpSubArrayDoubleLong extends GeneratedBinaryOp[Array[Double],Long,OpSub,Array[Double]](OpSub)
  implicit object OpMulArrayDoubleLong extends GeneratedBinaryOp[Array[Double],Long,OpMul,Array[Double]](OpMul)
  implicit object OpDivArrayDoubleLong extends GeneratedBinaryOp[Array[Double],Long,OpDiv,Array[Double]](OpDiv)
  implicit object OpAddArrayDoubleFloat extends GeneratedBinaryOp[Array[Double],Float,OpAdd,Array[Double]](OpAdd)
  implicit object OpSubArrayDoubleFloat extends GeneratedBinaryOp[Array[Double],Float,OpSub,Array[Double]](OpSub)
  implicit object OpMulArrayDoubleFloat extends GeneratedBinaryOp[Array[Double],Float,OpMul,Array[Double]](OpMul)
  implicit object OpDivArrayDoubleFloat extends GeneratedBinaryOp[Array[Double],Float,OpDiv,Array[Double]](OpDiv)
  implicit object OpAddArrayDoubleDouble extends GeneratedBinaryOp[Array[Double],Double,OpAdd,Array[Double]](OpAdd)
  implicit object OpSubArrayDoubleDouble extends GeneratedBinaryOp[Array[Double],Double,OpSub,Array[Double]](OpSub)
  implicit object OpMulArrayDoubleDouble extends GeneratedBinaryOp[Array[Double],Double,OpMul,Array[Double]](OpMul)
  implicit object OpDivArrayDoubleDouble extends GeneratedBinaryOp[Array[Double],Double,OpDiv,Array[Double]](OpDiv)
  implicit object OpAddDenseVectorRowIntInt extends GeneratedBinaryOp[DenseVectorRow[Int],Int,OpAdd,DenseVectorRow[Int]](OpAdd)
  implicit object OpSubDenseVectorRowIntInt extends GeneratedBinaryOp[DenseVectorRow[Int],Int,OpSub,DenseVectorRow[Int]](OpSub)
  implicit object OpMulDenseVectorRowIntInt extends GeneratedBinaryOp[DenseVectorRow[Int],Int,OpMul,DenseVectorRow[Int]](OpMul)
  implicit object OpDivDenseVectorRowIntInt extends GeneratedBinaryOp[DenseVectorRow[Int],Int,OpDiv,DenseVectorRow[Int]](OpDiv)
  implicit object OpAddDenseVectorRowIntLong extends GeneratedBinaryOp[DenseVectorRow[Int],Long,OpAdd,DenseVectorRow[Long]](OpAdd)
  implicit object OpSubDenseVectorRowIntLong extends GeneratedBinaryOp[DenseVectorRow[Int],Long,OpSub,DenseVectorRow[Long]](OpSub)
  implicit object OpMulDenseVectorRowIntLong extends GeneratedBinaryOp[DenseVectorRow[Int],Long,OpMul,DenseVectorRow[Long]](OpMul)
  implicit object OpDivDenseVectorRowIntLong extends GeneratedBinaryOp[DenseVectorRow[Int],Long,OpDiv,DenseVectorRow[Long]](OpDiv)
  implicit object OpAddDenseVectorRowIntFloat extends GeneratedBinaryOp[DenseVectorRow[Int],Float,OpAdd,DenseVectorRow[Float]](OpAdd)
  implicit object OpSubDenseVectorRowIntFloat extends GeneratedBinaryOp[DenseVectorRow[Int],Float,OpSub,DenseVectorRow[Float]](OpSub)
  implicit object OpMulDenseVectorRowIntFloat extends GeneratedBinaryOp[DenseVectorRow[Int],Float,OpMul,DenseVectorRow[Float]](OpMul)
  implicit object OpDivDenseVectorRowIntFloat extends GeneratedBinaryOp[DenseVectorRow[Int],Float,OpDiv,DenseVectorRow[Float]](OpDiv)
  implicit object OpAddDenseVectorRowIntDouble extends GeneratedBinaryOp[DenseVectorRow[Int],Double,OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubDenseVectorRowIntDouble extends GeneratedBinaryOp[DenseVectorRow[Int],Double,OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulDenseVectorRowIntDouble extends GeneratedBinaryOp[DenseVectorRow[Int],Double,OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivDenseVectorRowIntDouble extends GeneratedBinaryOp[DenseVectorRow[Int],Double,OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddDenseVectorRowLongInt extends GeneratedBinaryOp[DenseVectorRow[Long],Int,OpAdd,DenseVectorRow[Long]](OpAdd)
  implicit object OpSubDenseVectorRowLongInt extends GeneratedBinaryOp[DenseVectorRow[Long],Int,OpSub,DenseVectorRow[Long]](OpSub)
  implicit object OpMulDenseVectorRowLongInt extends GeneratedBinaryOp[DenseVectorRow[Long],Int,OpMul,DenseVectorRow[Long]](OpMul)
  implicit object OpDivDenseVectorRowLongInt extends GeneratedBinaryOp[DenseVectorRow[Long],Int,OpDiv,DenseVectorRow[Long]](OpDiv)
  implicit object OpAddDenseVectorRowLongLong extends GeneratedBinaryOp[DenseVectorRow[Long],Long,OpAdd,DenseVectorRow[Long]](OpAdd)
  implicit object OpSubDenseVectorRowLongLong extends GeneratedBinaryOp[DenseVectorRow[Long],Long,OpSub,DenseVectorRow[Long]](OpSub)
  implicit object OpMulDenseVectorRowLongLong extends GeneratedBinaryOp[DenseVectorRow[Long],Long,OpMul,DenseVectorRow[Long]](OpMul)
  implicit object OpDivDenseVectorRowLongLong extends GeneratedBinaryOp[DenseVectorRow[Long],Long,OpDiv,DenseVectorRow[Long]](OpDiv)
  implicit object OpAddDenseVectorRowLongFloat extends GeneratedBinaryOp[DenseVectorRow[Long],Float,OpAdd,DenseVectorRow[Float]](OpAdd)
  implicit object OpSubDenseVectorRowLongFloat extends GeneratedBinaryOp[DenseVectorRow[Long],Float,OpSub,DenseVectorRow[Float]](OpSub)
  implicit object OpMulDenseVectorRowLongFloat extends GeneratedBinaryOp[DenseVectorRow[Long],Float,OpMul,DenseVectorRow[Float]](OpMul)
  implicit object OpDivDenseVectorRowLongFloat extends GeneratedBinaryOp[DenseVectorRow[Long],Float,OpDiv,DenseVectorRow[Float]](OpDiv)
  implicit object OpAddDenseVectorRowLongDouble extends GeneratedBinaryOp[DenseVectorRow[Long],Double,OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubDenseVectorRowLongDouble extends GeneratedBinaryOp[DenseVectorRow[Long],Double,OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulDenseVectorRowLongDouble extends GeneratedBinaryOp[DenseVectorRow[Long],Double,OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivDenseVectorRowLongDouble extends GeneratedBinaryOp[DenseVectorRow[Long],Double,OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddDenseVectorRowFloatInt extends GeneratedBinaryOp[DenseVectorRow[Float],Int,OpAdd,DenseVectorRow[Float]](OpAdd)
  implicit object OpSubDenseVectorRowFloatInt extends GeneratedBinaryOp[DenseVectorRow[Float],Int,OpSub,DenseVectorRow[Float]](OpSub)
  implicit object OpMulDenseVectorRowFloatInt extends GeneratedBinaryOp[DenseVectorRow[Float],Int,OpMul,DenseVectorRow[Float]](OpMul)
  implicit object OpDivDenseVectorRowFloatInt extends GeneratedBinaryOp[DenseVectorRow[Float],Int,OpDiv,DenseVectorRow[Float]](OpDiv)
  implicit object OpAddDenseVectorRowFloatLong extends GeneratedBinaryOp[DenseVectorRow[Float],Long,OpAdd,DenseVectorRow[Float]](OpAdd)
  implicit object OpSubDenseVectorRowFloatLong extends GeneratedBinaryOp[DenseVectorRow[Float],Long,OpSub,DenseVectorRow[Float]](OpSub)
  implicit object OpMulDenseVectorRowFloatLong extends GeneratedBinaryOp[DenseVectorRow[Float],Long,OpMul,DenseVectorRow[Float]](OpMul)
  implicit object OpDivDenseVectorRowFloatLong extends GeneratedBinaryOp[DenseVectorRow[Float],Long,OpDiv,DenseVectorRow[Float]](OpDiv)
  implicit object OpAddDenseVectorRowFloatFloat extends GeneratedBinaryOp[DenseVectorRow[Float],Float,OpAdd,DenseVectorRow[Float]](OpAdd)
  implicit object OpSubDenseVectorRowFloatFloat extends GeneratedBinaryOp[DenseVectorRow[Float],Float,OpSub,DenseVectorRow[Float]](OpSub)
  implicit object OpMulDenseVectorRowFloatFloat extends GeneratedBinaryOp[DenseVectorRow[Float],Float,OpMul,DenseVectorRow[Float]](OpMul)
  implicit object OpDivDenseVectorRowFloatFloat extends GeneratedBinaryOp[DenseVectorRow[Float],Float,OpDiv,DenseVectorRow[Float]](OpDiv)
  implicit object OpAddDenseVectorRowFloatDouble extends GeneratedBinaryOp[DenseVectorRow[Float],Double,OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubDenseVectorRowFloatDouble extends GeneratedBinaryOp[DenseVectorRow[Float],Double,OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulDenseVectorRowFloatDouble extends GeneratedBinaryOp[DenseVectorRow[Float],Double,OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivDenseVectorRowFloatDouble extends GeneratedBinaryOp[DenseVectorRow[Float],Double,OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddDenseVectorRowDoubleInt extends GeneratedBinaryOp[DenseVectorRow[Double],Int,OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubDenseVectorRowDoubleInt extends GeneratedBinaryOp[DenseVectorRow[Double],Int,OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulDenseVectorRowDoubleInt extends GeneratedBinaryOp[DenseVectorRow[Double],Int,OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivDenseVectorRowDoubleInt extends GeneratedBinaryOp[DenseVectorRow[Double],Int,OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddDenseVectorRowDoubleLong extends GeneratedBinaryOp[DenseVectorRow[Double],Long,OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubDenseVectorRowDoubleLong extends GeneratedBinaryOp[DenseVectorRow[Double],Long,OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulDenseVectorRowDoubleLong extends GeneratedBinaryOp[DenseVectorRow[Double],Long,OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivDenseVectorRowDoubleLong extends GeneratedBinaryOp[DenseVectorRow[Double],Long,OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddDenseVectorRowDoubleFloat extends GeneratedBinaryOp[DenseVectorRow[Double],Float,OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubDenseVectorRowDoubleFloat extends GeneratedBinaryOp[DenseVectorRow[Double],Float,OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulDenseVectorRowDoubleFloat extends GeneratedBinaryOp[DenseVectorRow[Double],Float,OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivDenseVectorRowDoubleFloat extends GeneratedBinaryOp[DenseVectorRow[Double],Float,OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddDenseVectorRowDoubleDouble extends GeneratedBinaryOp[DenseVectorRow[Double],Double,OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubDenseVectorRowDoubleDouble extends GeneratedBinaryOp[DenseVectorRow[Double],Double,OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulDenseVectorRowDoubleDouble extends GeneratedBinaryOp[DenseVectorRow[Double],Double,OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivDenseVectorRowDoubleDouble extends GeneratedBinaryOp[DenseVectorRow[Double],Double,OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddDenseVectorColIntInt extends GeneratedBinaryOp[DenseVectorCol[Int],Int,OpAdd,DenseVectorCol[Int]](OpAdd)
  implicit object OpSubDenseVectorColIntInt extends GeneratedBinaryOp[DenseVectorCol[Int],Int,OpSub,DenseVectorCol[Int]](OpSub)
  implicit object OpMulDenseVectorColIntInt extends GeneratedBinaryOp[DenseVectorCol[Int],Int,OpMul,DenseVectorCol[Int]](OpMul)
  implicit object OpDivDenseVectorColIntInt extends GeneratedBinaryOp[DenseVectorCol[Int],Int,OpDiv,DenseVectorCol[Int]](OpDiv)
  implicit object OpAddDenseVectorColIntLong extends GeneratedBinaryOp[DenseVectorCol[Int],Long,OpAdd,DenseVectorCol[Long]](OpAdd)
  implicit object OpSubDenseVectorColIntLong extends GeneratedBinaryOp[DenseVectorCol[Int],Long,OpSub,DenseVectorCol[Long]](OpSub)
  implicit object OpMulDenseVectorColIntLong extends GeneratedBinaryOp[DenseVectorCol[Int],Long,OpMul,DenseVectorCol[Long]](OpMul)
  implicit object OpDivDenseVectorColIntLong extends GeneratedBinaryOp[DenseVectorCol[Int],Long,OpDiv,DenseVectorCol[Long]](OpDiv)
  implicit object OpAddDenseVectorColIntFloat extends GeneratedBinaryOp[DenseVectorCol[Int],Float,OpAdd,DenseVectorCol[Float]](OpAdd)
  implicit object OpSubDenseVectorColIntFloat extends GeneratedBinaryOp[DenseVectorCol[Int],Float,OpSub,DenseVectorCol[Float]](OpSub)
  implicit object OpMulDenseVectorColIntFloat extends GeneratedBinaryOp[DenseVectorCol[Int],Float,OpMul,DenseVectorCol[Float]](OpMul)
  implicit object OpDivDenseVectorColIntFloat extends GeneratedBinaryOp[DenseVectorCol[Int],Float,OpDiv,DenseVectorCol[Float]](OpDiv)
  implicit object OpAddDenseVectorColIntDouble extends GeneratedBinaryOp[DenseVectorCol[Int],Double,OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubDenseVectorColIntDouble extends GeneratedBinaryOp[DenseVectorCol[Int],Double,OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulDenseVectorColIntDouble extends GeneratedBinaryOp[DenseVectorCol[Int],Double,OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivDenseVectorColIntDouble extends GeneratedBinaryOp[DenseVectorCol[Int],Double,OpDiv,DenseVectorCol[Double]](OpDiv)
  implicit object OpAddDenseVectorColLongInt extends GeneratedBinaryOp[DenseVectorCol[Long],Int,OpAdd,DenseVectorCol[Long]](OpAdd)
  implicit object OpSubDenseVectorColLongInt extends GeneratedBinaryOp[DenseVectorCol[Long],Int,OpSub,DenseVectorCol[Long]](OpSub)
  implicit object OpMulDenseVectorColLongInt extends GeneratedBinaryOp[DenseVectorCol[Long],Int,OpMul,DenseVectorCol[Long]](OpMul)
  implicit object OpDivDenseVectorColLongInt extends GeneratedBinaryOp[DenseVectorCol[Long],Int,OpDiv,DenseVectorCol[Long]](OpDiv)
  implicit object OpAddDenseVectorColLongLong extends GeneratedBinaryOp[DenseVectorCol[Long],Long,OpAdd,DenseVectorCol[Long]](OpAdd)
  implicit object OpSubDenseVectorColLongLong extends GeneratedBinaryOp[DenseVectorCol[Long],Long,OpSub,DenseVectorCol[Long]](OpSub)
  implicit object OpMulDenseVectorColLongLong extends GeneratedBinaryOp[DenseVectorCol[Long],Long,OpMul,DenseVectorCol[Long]](OpMul)
  implicit object OpDivDenseVectorColLongLong extends GeneratedBinaryOp[DenseVectorCol[Long],Long,OpDiv,DenseVectorCol[Long]](OpDiv)
  implicit object OpAddDenseVectorColLongFloat extends GeneratedBinaryOp[DenseVectorCol[Long],Float,OpAdd,DenseVectorCol[Float]](OpAdd)
  implicit object OpSubDenseVectorColLongFloat extends GeneratedBinaryOp[DenseVectorCol[Long],Float,OpSub,DenseVectorCol[Float]](OpSub)
  implicit object OpMulDenseVectorColLongFloat extends GeneratedBinaryOp[DenseVectorCol[Long],Float,OpMul,DenseVectorCol[Float]](OpMul)
  implicit object OpDivDenseVectorColLongFloat extends GeneratedBinaryOp[DenseVectorCol[Long],Float,OpDiv,DenseVectorCol[Float]](OpDiv)
  implicit object OpAddDenseVectorColLongDouble extends GeneratedBinaryOp[DenseVectorCol[Long],Double,OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubDenseVectorColLongDouble extends GeneratedBinaryOp[DenseVectorCol[Long],Double,OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulDenseVectorColLongDouble extends GeneratedBinaryOp[DenseVectorCol[Long],Double,OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivDenseVectorColLongDouble extends GeneratedBinaryOp[DenseVectorCol[Long],Double,OpDiv,DenseVectorCol[Double]](OpDiv)
  implicit object OpAddDenseVectorColFloatInt extends GeneratedBinaryOp[DenseVectorCol[Float],Int,OpAdd,DenseVectorCol[Float]](OpAdd)
  implicit object OpSubDenseVectorColFloatInt extends GeneratedBinaryOp[DenseVectorCol[Float],Int,OpSub,DenseVectorCol[Float]](OpSub)
  implicit object OpMulDenseVectorColFloatInt extends GeneratedBinaryOp[DenseVectorCol[Float],Int,OpMul,DenseVectorCol[Float]](OpMul)
  implicit object OpDivDenseVectorColFloatInt extends GeneratedBinaryOp[DenseVectorCol[Float],Int,OpDiv,DenseVectorCol[Float]](OpDiv)
  implicit object OpAddDenseVectorColFloatLong extends GeneratedBinaryOp[DenseVectorCol[Float],Long,OpAdd,DenseVectorCol[Float]](OpAdd)
  implicit object OpSubDenseVectorColFloatLong extends GeneratedBinaryOp[DenseVectorCol[Float],Long,OpSub,DenseVectorCol[Float]](OpSub)
  implicit object OpMulDenseVectorColFloatLong extends GeneratedBinaryOp[DenseVectorCol[Float],Long,OpMul,DenseVectorCol[Float]](OpMul)
  implicit object OpDivDenseVectorColFloatLong extends GeneratedBinaryOp[DenseVectorCol[Float],Long,OpDiv,DenseVectorCol[Float]](OpDiv)
  implicit object OpAddDenseVectorColFloatFloat extends GeneratedBinaryOp[DenseVectorCol[Float],Float,OpAdd,DenseVectorCol[Float]](OpAdd)
  implicit object OpSubDenseVectorColFloatFloat extends GeneratedBinaryOp[DenseVectorCol[Float],Float,OpSub,DenseVectorCol[Float]](OpSub)
  implicit object OpMulDenseVectorColFloatFloat extends GeneratedBinaryOp[DenseVectorCol[Float],Float,OpMul,DenseVectorCol[Float]](OpMul)
  implicit object OpDivDenseVectorColFloatFloat extends GeneratedBinaryOp[DenseVectorCol[Float],Float,OpDiv,DenseVectorCol[Float]](OpDiv)
  implicit object OpAddDenseVectorColFloatDouble extends GeneratedBinaryOp[DenseVectorCol[Float],Double,OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubDenseVectorColFloatDouble extends GeneratedBinaryOp[DenseVectorCol[Float],Double,OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulDenseVectorColFloatDouble extends GeneratedBinaryOp[DenseVectorCol[Float],Double,OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivDenseVectorColFloatDouble extends GeneratedBinaryOp[DenseVectorCol[Float],Double,OpDiv,DenseVectorCol[Double]](OpDiv)
  implicit object OpAddDenseVectorColDoubleInt extends GeneratedBinaryOp[DenseVectorCol[Double],Int,OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubDenseVectorColDoubleInt extends GeneratedBinaryOp[DenseVectorCol[Double],Int,OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulDenseVectorColDoubleInt extends GeneratedBinaryOp[DenseVectorCol[Double],Int,OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivDenseVectorColDoubleInt extends GeneratedBinaryOp[DenseVectorCol[Double],Int,OpDiv,DenseVectorCol[Double]](OpDiv)
  implicit object OpAddDenseVectorColDoubleLong extends GeneratedBinaryOp[DenseVectorCol[Double],Long,OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubDenseVectorColDoubleLong extends GeneratedBinaryOp[DenseVectorCol[Double],Long,OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulDenseVectorColDoubleLong extends GeneratedBinaryOp[DenseVectorCol[Double],Long,OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivDenseVectorColDoubleLong extends GeneratedBinaryOp[DenseVectorCol[Double],Long,OpDiv,DenseVectorCol[Double]](OpDiv)
  implicit object OpAddDenseVectorColDoubleFloat extends GeneratedBinaryOp[DenseVectorCol[Double],Float,OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubDenseVectorColDoubleFloat extends GeneratedBinaryOp[DenseVectorCol[Double],Float,OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulDenseVectorColDoubleFloat extends GeneratedBinaryOp[DenseVectorCol[Double],Float,OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivDenseVectorColDoubleFloat extends GeneratedBinaryOp[DenseVectorCol[Double],Float,OpDiv,DenseVectorCol[Double]](OpDiv)
  implicit object OpAddDenseVectorColDoubleDouble extends GeneratedBinaryOp[DenseVectorCol[Double],Double,OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubDenseVectorColDoubleDouble extends GeneratedBinaryOp[DenseVectorCol[Double],Double,OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulDenseVectorColDoubleDouble extends GeneratedBinaryOp[DenseVectorCol[Double],Double,OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivDenseVectorColDoubleDouble extends GeneratedBinaryOp[DenseVectorCol[Double],Double,OpDiv,DenseVectorCol[Double]](OpDiv)
  implicit object OpAddIntArrayInt extends GeneratedBinaryOp[Int,Array[Int],OpAdd,Array[Int]](OpAdd)
  implicit object OpSubIntArrayInt extends GeneratedBinaryOp[Int,Array[Int],OpSub,Array[Int]](OpSub)
  implicit object OpMulIntArrayInt extends GeneratedBinaryOp[Int,Array[Int],OpMul,Array[Int]](OpMul)
  implicit object OpDivIntArrayInt extends GeneratedBinaryOp[Int,Array[Int],OpDiv,Array[Int]](OpDiv)
  implicit object OpAddIntArrayLong extends GeneratedBinaryOp[Int,Array[Long],OpAdd,Array[Long]](OpAdd)
  implicit object OpSubIntArrayLong extends GeneratedBinaryOp[Int,Array[Long],OpSub,Array[Long]](OpSub)
  implicit object OpMulIntArrayLong extends GeneratedBinaryOp[Int,Array[Long],OpMul,Array[Long]](OpMul)
  implicit object OpDivIntArrayLong extends GeneratedBinaryOp[Int,Array[Long],OpDiv,Array[Long]](OpDiv)
  implicit object OpAddIntArrayFloat extends GeneratedBinaryOp[Int,Array[Float],OpAdd,Array[Float]](OpAdd)
  implicit object OpSubIntArrayFloat extends GeneratedBinaryOp[Int,Array[Float],OpSub,Array[Float]](OpSub)
  implicit object OpMulIntArrayFloat extends GeneratedBinaryOp[Int,Array[Float],OpMul,Array[Float]](OpMul)
  implicit object OpDivIntArrayFloat extends GeneratedBinaryOp[Int,Array[Float],OpDiv,Array[Float]](OpDiv)
  implicit object OpAddIntArrayDouble extends GeneratedBinaryOp[Int,Array[Double],OpAdd,Array[Double]](OpAdd)
  implicit object OpSubIntArrayDouble extends GeneratedBinaryOp[Int,Array[Double],OpSub,Array[Double]](OpSub)
  implicit object OpMulIntArrayDouble extends GeneratedBinaryOp[Int,Array[Double],OpMul,Array[Double]](OpMul)
  implicit object OpDivIntArrayDouble extends GeneratedBinaryOp[Int,Array[Double],OpDiv,Array[Double]](OpDiv)
  implicit object OpAddLongArrayInt extends GeneratedBinaryOp[Long,Array[Int],OpAdd,Array[Long]](OpAdd)
  implicit object OpSubLongArrayInt extends GeneratedBinaryOp[Long,Array[Int],OpSub,Array[Long]](OpSub)
  implicit object OpMulLongArrayInt extends GeneratedBinaryOp[Long,Array[Int],OpMul,Array[Long]](OpMul)
  implicit object OpDivLongArrayInt extends GeneratedBinaryOp[Long,Array[Int],OpDiv,Array[Long]](OpDiv)
  implicit object OpAddLongArrayLong extends GeneratedBinaryOp[Long,Array[Long],OpAdd,Array[Long]](OpAdd)
  implicit object OpSubLongArrayLong extends GeneratedBinaryOp[Long,Array[Long],OpSub,Array[Long]](OpSub)
  implicit object OpMulLongArrayLong extends GeneratedBinaryOp[Long,Array[Long],OpMul,Array[Long]](OpMul)
  implicit object OpDivLongArrayLong extends GeneratedBinaryOp[Long,Array[Long],OpDiv,Array[Long]](OpDiv)
  implicit object OpAddLongArrayFloat extends GeneratedBinaryOp[Long,Array[Float],OpAdd,Array[Float]](OpAdd)
  implicit object OpSubLongArrayFloat extends GeneratedBinaryOp[Long,Array[Float],OpSub,Array[Float]](OpSub)
  implicit object OpMulLongArrayFloat extends GeneratedBinaryOp[Long,Array[Float],OpMul,Array[Float]](OpMul)
  implicit object OpDivLongArrayFloat extends GeneratedBinaryOp[Long,Array[Float],OpDiv,Array[Float]](OpDiv)
  implicit object OpAddLongArrayDouble extends GeneratedBinaryOp[Long,Array[Double],OpAdd,Array[Double]](OpAdd)
  implicit object OpSubLongArrayDouble extends GeneratedBinaryOp[Long,Array[Double],OpSub,Array[Double]](OpSub)
  implicit object OpMulLongArrayDouble extends GeneratedBinaryOp[Long,Array[Double],OpMul,Array[Double]](OpMul)
  implicit object OpDivLongArrayDouble extends GeneratedBinaryOp[Long,Array[Double],OpDiv,Array[Double]](OpDiv)
  implicit object OpAddFloatArrayInt extends GeneratedBinaryOp[Float,Array[Int],OpAdd,Array[Float]](OpAdd)
  implicit object OpSubFloatArrayInt extends GeneratedBinaryOp[Float,Array[Int],OpSub,Array[Float]](OpSub)
  implicit object OpMulFloatArrayInt extends GeneratedBinaryOp[Float,Array[Int],OpMul,Array[Float]](OpMul)
  implicit object OpDivFloatArrayInt extends GeneratedBinaryOp[Float,Array[Int],OpDiv,Array[Float]](OpDiv)
  implicit object OpAddFloatArrayLong extends GeneratedBinaryOp[Float,Array[Long],OpAdd,Array[Float]](OpAdd)
  implicit object OpSubFloatArrayLong extends GeneratedBinaryOp[Float,Array[Long],OpSub,Array[Float]](OpSub)
  implicit object OpMulFloatArrayLong extends GeneratedBinaryOp[Float,Array[Long],OpMul,Array[Float]](OpMul)
  implicit object OpDivFloatArrayLong extends GeneratedBinaryOp[Float,Array[Long],OpDiv,Array[Float]](OpDiv)
  implicit object OpAddFloatArrayFloat extends GeneratedBinaryOp[Float,Array[Float],OpAdd,Array[Float]](OpAdd)
  implicit object OpSubFloatArrayFloat extends GeneratedBinaryOp[Float,Array[Float],OpSub,Array[Float]](OpSub)
  implicit object OpMulFloatArrayFloat extends GeneratedBinaryOp[Float,Array[Float],OpMul,Array[Float]](OpMul)
  implicit object OpDivFloatArrayFloat extends GeneratedBinaryOp[Float,Array[Float],OpDiv,Array[Float]](OpDiv)
  implicit object OpAddFloatArrayDouble extends GeneratedBinaryOp[Float,Array[Double],OpAdd,Array[Double]](OpAdd)
  implicit object OpSubFloatArrayDouble extends GeneratedBinaryOp[Float,Array[Double],OpSub,Array[Double]](OpSub)
  implicit object OpMulFloatArrayDouble extends GeneratedBinaryOp[Float,Array[Double],OpMul,Array[Double]](OpMul)
  implicit object OpDivFloatArrayDouble extends GeneratedBinaryOp[Float,Array[Double],OpDiv,Array[Double]](OpDiv)
  implicit object OpAddDoubleArrayInt extends GeneratedBinaryOp[Double,Array[Int],OpAdd,Array[Double]](OpAdd)
  implicit object OpSubDoubleArrayInt extends GeneratedBinaryOp[Double,Array[Int],OpSub,Array[Double]](OpSub)
  implicit object OpMulDoubleArrayInt extends GeneratedBinaryOp[Double,Array[Int],OpMul,Array[Double]](OpMul)
  implicit object OpDivDoubleArrayInt extends GeneratedBinaryOp[Double,Array[Int],OpDiv,Array[Double]](OpDiv)
  implicit object OpAddDoubleArrayLong extends GeneratedBinaryOp[Double,Array[Long],OpAdd,Array[Double]](OpAdd)
  implicit object OpSubDoubleArrayLong extends GeneratedBinaryOp[Double,Array[Long],OpSub,Array[Double]](OpSub)
  implicit object OpMulDoubleArrayLong extends GeneratedBinaryOp[Double,Array[Long],OpMul,Array[Double]](OpMul)
  implicit object OpDivDoubleArrayLong extends GeneratedBinaryOp[Double,Array[Long],OpDiv,Array[Double]](OpDiv)
  implicit object OpAddDoubleArrayFloat extends GeneratedBinaryOp[Double,Array[Float],OpAdd,Array[Double]](OpAdd)
  implicit object OpSubDoubleArrayFloat extends GeneratedBinaryOp[Double,Array[Float],OpSub,Array[Double]](OpSub)
  implicit object OpMulDoubleArrayFloat extends GeneratedBinaryOp[Double,Array[Float],OpMul,Array[Double]](OpMul)
  implicit object OpDivDoubleArrayFloat extends GeneratedBinaryOp[Double,Array[Float],OpDiv,Array[Double]](OpDiv)
  implicit object OpAddDoubleArrayDouble extends GeneratedBinaryOp[Double,Array[Double],OpAdd,Array[Double]](OpAdd)
  implicit object OpSubDoubleArrayDouble extends GeneratedBinaryOp[Double,Array[Double],OpSub,Array[Double]](OpSub)
  implicit object OpMulDoubleArrayDouble extends GeneratedBinaryOp[Double,Array[Double],OpMul,Array[Double]](OpMul)
  implicit object OpDivDoubleArrayDouble extends GeneratedBinaryOp[Double,Array[Double],OpDiv,Array[Double]](OpDiv)
  implicit object OpAddIntDenseVectorRowInt extends GeneratedBinaryOp[Int,DenseVectorRow[Int],OpAdd,DenseVectorRow[Int]](OpAdd)
  implicit object OpSubIntDenseVectorRowInt extends GeneratedBinaryOp[Int,DenseVectorRow[Int],OpSub,DenseVectorRow[Int]](OpSub)
  implicit object OpMulIntDenseVectorRowInt extends GeneratedBinaryOp[Int,DenseVectorRow[Int],OpMul,DenseVectorRow[Int]](OpMul)
  implicit object OpDivIntDenseVectorRowInt extends GeneratedBinaryOp[Int,DenseVectorRow[Int],OpDiv,DenseVectorRow[Int]](OpDiv)
  implicit object OpAddIntDenseVectorRowLong extends GeneratedBinaryOp[Int,DenseVectorRow[Long],OpAdd,DenseVectorRow[Long]](OpAdd)
  implicit object OpSubIntDenseVectorRowLong extends GeneratedBinaryOp[Int,DenseVectorRow[Long],OpSub,DenseVectorRow[Long]](OpSub)
  implicit object OpMulIntDenseVectorRowLong extends GeneratedBinaryOp[Int,DenseVectorRow[Long],OpMul,DenseVectorRow[Long]](OpMul)
  implicit object OpDivIntDenseVectorRowLong extends GeneratedBinaryOp[Int,DenseVectorRow[Long],OpDiv,DenseVectorRow[Long]](OpDiv)
  implicit object OpAddIntDenseVectorRowFloat extends GeneratedBinaryOp[Int,DenseVectorRow[Float],OpAdd,DenseVectorRow[Float]](OpAdd)
  implicit object OpSubIntDenseVectorRowFloat extends GeneratedBinaryOp[Int,DenseVectorRow[Float],OpSub,DenseVectorRow[Float]](OpSub)
  implicit object OpMulIntDenseVectorRowFloat extends GeneratedBinaryOp[Int,DenseVectorRow[Float],OpMul,DenseVectorRow[Float]](OpMul)
  implicit object OpDivIntDenseVectorRowFloat extends GeneratedBinaryOp[Int,DenseVectorRow[Float],OpDiv,DenseVectorRow[Float]](OpDiv)
  implicit object OpAddIntDenseVectorRowDouble extends GeneratedBinaryOp[Int,DenseVectorRow[Double],OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubIntDenseVectorRowDouble extends GeneratedBinaryOp[Int,DenseVectorRow[Double],OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulIntDenseVectorRowDouble extends GeneratedBinaryOp[Int,DenseVectorRow[Double],OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivIntDenseVectorRowDouble extends GeneratedBinaryOp[Int,DenseVectorRow[Double],OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddLongDenseVectorRowInt extends GeneratedBinaryOp[Long,DenseVectorRow[Int],OpAdd,DenseVectorRow[Long]](OpAdd)
  implicit object OpSubLongDenseVectorRowInt extends GeneratedBinaryOp[Long,DenseVectorRow[Int],OpSub,DenseVectorRow[Long]](OpSub)
  implicit object OpMulLongDenseVectorRowInt extends GeneratedBinaryOp[Long,DenseVectorRow[Int],OpMul,DenseVectorRow[Long]](OpMul)
  implicit object OpDivLongDenseVectorRowInt extends GeneratedBinaryOp[Long,DenseVectorRow[Int],OpDiv,DenseVectorRow[Long]](OpDiv)
  implicit object OpAddLongDenseVectorRowLong extends GeneratedBinaryOp[Long,DenseVectorRow[Long],OpAdd,DenseVectorRow[Long]](OpAdd)
  implicit object OpSubLongDenseVectorRowLong extends GeneratedBinaryOp[Long,DenseVectorRow[Long],OpSub,DenseVectorRow[Long]](OpSub)
  implicit object OpMulLongDenseVectorRowLong extends GeneratedBinaryOp[Long,DenseVectorRow[Long],OpMul,DenseVectorRow[Long]](OpMul)
  implicit object OpDivLongDenseVectorRowLong extends GeneratedBinaryOp[Long,DenseVectorRow[Long],OpDiv,DenseVectorRow[Long]](OpDiv)
  implicit object OpAddLongDenseVectorRowFloat extends GeneratedBinaryOp[Long,DenseVectorRow[Float],OpAdd,DenseVectorRow[Float]](OpAdd)
  implicit object OpSubLongDenseVectorRowFloat extends GeneratedBinaryOp[Long,DenseVectorRow[Float],OpSub,DenseVectorRow[Float]](OpSub)
  implicit object OpMulLongDenseVectorRowFloat extends GeneratedBinaryOp[Long,DenseVectorRow[Float],OpMul,DenseVectorRow[Float]](OpMul)
  implicit object OpDivLongDenseVectorRowFloat extends GeneratedBinaryOp[Long,DenseVectorRow[Float],OpDiv,DenseVectorRow[Float]](OpDiv)
  implicit object OpAddLongDenseVectorRowDouble extends GeneratedBinaryOp[Long,DenseVectorRow[Double],OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubLongDenseVectorRowDouble extends GeneratedBinaryOp[Long,DenseVectorRow[Double],OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulLongDenseVectorRowDouble extends GeneratedBinaryOp[Long,DenseVectorRow[Double],OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivLongDenseVectorRowDouble extends GeneratedBinaryOp[Long,DenseVectorRow[Double],OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddFloatDenseVectorRowInt extends GeneratedBinaryOp[Float,DenseVectorRow[Int],OpAdd,DenseVectorRow[Float]](OpAdd)
  implicit object OpSubFloatDenseVectorRowInt extends GeneratedBinaryOp[Float,DenseVectorRow[Int],OpSub,DenseVectorRow[Float]](OpSub)
  implicit object OpMulFloatDenseVectorRowInt extends GeneratedBinaryOp[Float,DenseVectorRow[Int],OpMul,DenseVectorRow[Float]](OpMul)
  implicit object OpDivFloatDenseVectorRowInt extends GeneratedBinaryOp[Float,DenseVectorRow[Int],OpDiv,DenseVectorRow[Float]](OpDiv)
  implicit object OpAddFloatDenseVectorRowLong extends GeneratedBinaryOp[Float,DenseVectorRow[Long],OpAdd,DenseVectorRow[Float]](OpAdd)
  implicit object OpSubFloatDenseVectorRowLong extends GeneratedBinaryOp[Float,DenseVectorRow[Long],OpSub,DenseVectorRow[Float]](OpSub)
  implicit object OpMulFloatDenseVectorRowLong extends GeneratedBinaryOp[Float,DenseVectorRow[Long],OpMul,DenseVectorRow[Float]](OpMul)
  implicit object OpDivFloatDenseVectorRowLong extends GeneratedBinaryOp[Float,DenseVectorRow[Long],OpDiv,DenseVectorRow[Float]](OpDiv)
  implicit object OpAddFloatDenseVectorRowFloat extends GeneratedBinaryOp[Float,DenseVectorRow[Float],OpAdd,DenseVectorRow[Float]](OpAdd)
  implicit object OpSubFloatDenseVectorRowFloat extends GeneratedBinaryOp[Float,DenseVectorRow[Float],OpSub,DenseVectorRow[Float]](OpSub)
  implicit object OpMulFloatDenseVectorRowFloat extends GeneratedBinaryOp[Float,DenseVectorRow[Float],OpMul,DenseVectorRow[Float]](OpMul)
  implicit object OpDivFloatDenseVectorRowFloat extends GeneratedBinaryOp[Float,DenseVectorRow[Float],OpDiv,DenseVectorRow[Float]](OpDiv)
  implicit object OpAddFloatDenseVectorRowDouble extends GeneratedBinaryOp[Float,DenseVectorRow[Double],OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubFloatDenseVectorRowDouble extends GeneratedBinaryOp[Float,DenseVectorRow[Double],OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulFloatDenseVectorRowDouble extends GeneratedBinaryOp[Float,DenseVectorRow[Double],OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivFloatDenseVectorRowDouble extends GeneratedBinaryOp[Float,DenseVectorRow[Double],OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddDoubleDenseVectorRowInt extends GeneratedBinaryOp[Double,DenseVectorRow[Int],OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubDoubleDenseVectorRowInt extends GeneratedBinaryOp[Double,DenseVectorRow[Int],OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulDoubleDenseVectorRowInt extends GeneratedBinaryOp[Double,DenseVectorRow[Int],OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivDoubleDenseVectorRowInt extends GeneratedBinaryOp[Double,DenseVectorRow[Int],OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddDoubleDenseVectorRowLong extends GeneratedBinaryOp[Double,DenseVectorRow[Long],OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubDoubleDenseVectorRowLong extends GeneratedBinaryOp[Double,DenseVectorRow[Long],OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulDoubleDenseVectorRowLong extends GeneratedBinaryOp[Double,DenseVectorRow[Long],OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivDoubleDenseVectorRowLong extends GeneratedBinaryOp[Double,DenseVectorRow[Long],OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddDoubleDenseVectorRowFloat extends GeneratedBinaryOp[Double,DenseVectorRow[Float],OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubDoubleDenseVectorRowFloat extends GeneratedBinaryOp[Double,DenseVectorRow[Float],OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulDoubleDenseVectorRowFloat extends GeneratedBinaryOp[Double,DenseVectorRow[Float],OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivDoubleDenseVectorRowFloat extends GeneratedBinaryOp[Double,DenseVectorRow[Float],OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddDoubleDenseVectorRowDouble extends GeneratedBinaryOp[Double,DenseVectorRow[Double],OpAdd,DenseVectorRow[Double]](OpAdd)
  implicit object OpSubDoubleDenseVectorRowDouble extends GeneratedBinaryOp[Double,DenseVectorRow[Double],OpSub,DenseVectorRow[Double]](OpSub)
  implicit object OpMulDoubleDenseVectorRowDouble extends GeneratedBinaryOp[Double,DenseVectorRow[Double],OpMul,DenseVectorRow[Double]](OpMul)
  implicit object OpDivDoubleDenseVectorRowDouble extends GeneratedBinaryOp[Double,DenseVectorRow[Double],OpDiv,DenseVectorRow[Double]](OpDiv)
  implicit object OpAddIntDenseVectorColInt extends GeneratedBinaryOp[Int,DenseVectorCol[Int],OpAdd,DenseVectorCol[Int]](OpAdd)
  implicit object OpSubIntDenseVectorColInt extends GeneratedBinaryOp[Int,DenseVectorCol[Int],OpSub,DenseVectorCol[Int]](OpSub)
  implicit object OpMulIntDenseVectorColInt extends GeneratedBinaryOp[Int,DenseVectorCol[Int],OpMul,DenseVectorCol[Int]](OpMul)
  implicit object OpDivIntDenseVectorColInt extends GeneratedBinaryOp[Int,DenseVectorCol[Int],OpDiv,DenseVectorCol[Int]](OpDiv)
  implicit object OpAddIntDenseVectorColLong extends GeneratedBinaryOp[Int,DenseVectorCol[Long],OpAdd,DenseVectorCol[Long]](OpAdd)
  implicit object OpSubIntDenseVectorColLong extends GeneratedBinaryOp[Int,DenseVectorCol[Long],OpSub,DenseVectorCol[Long]](OpSub)
  implicit object OpMulIntDenseVectorColLong extends GeneratedBinaryOp[Int,DenseVectorCol[Long],OpMul,DenseVectorCol[Long]](OpMul)
  implicit object OpDivIntDenseVectorColLong extends GeneratedBinaryOp[Int,DenseVectorCol[Long],OpDiv,DenseVectorCol[Long]](OpDiv)
  implicit object OpAddIntDenseVectorColFloat extends GeneratedBinaryOp[Int,DenseVectorCol[Float],OpAdd,DenseVectorCol[Float]](OpAdd)
  implicit object OpSubIntDenseVectorColFloat extends GeneratedBinaryOp[Int,DenseVectorCol[Float],OpSub,DenseVectorCol[Float]](OpSub)
  implicit object OpMulIntDenseVectorColFloat extends GeneratedBinaryOp[Int,DenseVectorCol[Float],OpMul,DenseVectorCol[Float]](OpMul)
  implicit object OpDivIntDenseVectorColFloat extends GeneratedBinaryOp[Int,DenseVectorCol[Float],OpDiv,DenseVectorCol[Float]](OpDiv)
  implicit object OpAddIntDenseVectorColDouble extends GeneratedBinaryOp[Int,DenseVectorCol[Double],OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubIntDenseVectorColDouble extends GeneratedBinaryOp[Int,DenseVectorCol[Double],OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulIntDenseVectorColDouble extends GeneratedBinaryOp[Int,DenseVectorCol[Double],OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivIntDenseVectorColDouble extends GeneratedBinaryOp[Int,DenseVectorCol[Double],OpDiv,DenseVectorCol[Double]](OpDiv)
  implicit object OpAddLongDenseVectorColInt extends GeneratedBinaryOp[Long,DenseVectorCol[Int],OpAdd,DenseVectorCol[Long]](OpAdd)
  implicit object OpSubLongDenseVectorColInt extends GeneratedBinaryOp[Long,DenseVectorCol[Int],OpSub,DenseVectorCol[Long]](OpSub)
  implicit object OpMulLongDenseVectorColInt extends GeneratedBinaryOp[Long,DenseVectorCol[Int],OpMul,DenseVectorCol[Long]](OpMul)
  implicit object OpDivLongDenseVectorColInt extends GeneratedBinaryOp[Long,DenseVectorCol[Int],OpDiv,DenseVectorCol[Long]](OpDiv)
  implicit object OpAddLongDenseVectorColLong extends GeneratedBinaryOp[Long,DenseVectorCol[Long],OpAdd,DenseVectorCol[Long]](OpAdd)
  implicit object OpSubLongDenseVectorColLong extends GeneratedBinaryOp[Long,DenseVectorCol[Long],OpSub,DenseVectorCol[Long]](OpSub)
  implicit object OpMulLongDenseVectorColLong extends GeneratedBinaryOp[Long,DenseVectorCol[Long],OpMul,DenseVectorCol[Long]](OpMul)
  implicit object OpDivLongDenseVectorColLong extends GeneratedBinaryOp[Long,DenseVectorCol[Long],OpDiv,DenseVectorCol[Long]](OpDiv)
  implicit object OpAddLongDenseVectorColFloat extends GeneratedBinaryOp[Long,DenseVectorCol[Float],OpAdd,DenseVectorCol[Float]](OpAdd)
  implicit object OpSubLongDenseVectorColFloat extends GeneratedBinaryOp[Long,DenseVectorCol[Float],OpSub,DenseVectorCol[Float]](OpSub)
  implicit object OpMulLongDenseVectorColFloat extends GeneratedBinaryOp[Long,DenseVectorCol[Float],OpMul,DenseVectorCol[Float]](OpMul)
  implicit object OpDivLongDenseVectorColFloat extends GeneratedBinaryOp[Long,DenseVectorCol[Float],OpDiv,DenseVectorCol[Float]](OpDiv)
  implicit object OpAddLongDenseVectorColDouble extends GeneratedBinaryOp[Long,DenseVectorCol[Double],OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubLongDenseVectorColDouble extends GeneratedBinaryOp[Long,DenseVectorCol[Double],OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulLongDenseVectorColDouble extends GeneratedBinaryOp[Long,DenseVectorCol[Double],OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivLongDenseVectorColDouble extends GeneratedBinaryOp[Long,DenseVectorCol[Double],OpDiv,DenseVectorCol[Double]](OpDiv)
  implicit object OpAddFloatDenseVectorColInt extends GeneratedBinaryOp[Float,DenseVectorCol[Int],OpAdd,DenseVectorCol[Float]](OpAdd)
  implicit object OpSubFloatDenseVectorColInt extends GeneratedBinaryOp[Float,DenseVectorCol[Int],OpSub,DenseVectorCol[Float]](OpSub)
  implicit object OpMulFloatDenseVectorColInt extends GeneratedBinaryOp[Float,DenseVectorCol[Int],OpMul,DenseVectorCol[Float]](OpMul)
  implicit object OpDivFloatDenseVectorColInt extends GeneratedBinaryOp[Float,DenseVectorCol[Int],OpDiv,DenseVectorCol[Float]](OpDiv)
  implicit object OpAddFloatDenseVectorColLong extends GeneratedBinaryOp[Float,DenseVectorCol[Long],OpAdd,DenseVectorCol[Float]](OpAdd)
  implicit object OpSubFloatDenseVectorColLong extends GeneratedBinaryOp[Float,DenseVectorCol[Long],OpSub,DenseVectorCol[Float]](OpSub)
  implicit object OpMulFloatDenseVectorColLong extends GeneratedBinaryOp[Float,DenseVectorCol[Long],OpMul,DenseVectorCol[Float]](OpMul)
  implicit object OpDivFloatDenseVectorColLong extends GeneratedBinaryOp[Float,DenseVectorCol[Long],OpDiv,DenseVectorCol[Float]](OpDiv)
  implicit object OpAddFloatDenseVectorColFloat extends GeneratedBinaryOp[Float,DenseVectorCol[Float],OpAdd,DenseVectorCol[Float]](OpAdd)
  implicit object OpSubFloatDenseVectorColFloat extends GeneratedBinaryOp[Float,DenseVectorCol[Float],OpSub,DenseVectorCol[Float]](OpSub)
  implicit object OpMulFloatDenseVectorColFloat extends GeneratedBinaryOp[Float,DenseVectorCol[Float],OpMul,DenseVectorCol[Float]](OpMul)
  implicit object OpDivFloatDenseVectorColFloat extends GeneratedBinaryOp[Float,DenseVectorCol[Float],OpDiv,DenseVectorCol[Float]](OpDiv)
  implicit object OpAddFloatDenseVectorColDouble extends GeneratedBinaryOp[Float,DenseVectorCol[Double],OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubFloatDenseVectorColDouble extends GeneratedBinaryOp[Float,DenseVectorCol[Double],OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulFloatDenseVectorColDouble extends GeneratedBinaryOp[Float,DenseVectorCol[Double],OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivFloatDenseVectorColDouble extends GeneratedBinaryOp[Float,DenseVectorCol[Double],OpDiv,DenseVectorCol[Double]](OpDiv)
  implicit object OpAddDoubleDenseVectorColInt extends GeneratedBinaryOp[Double,DenseVectorCol[Int],OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubDoubleDenseVectorColInt extends GeneratedBinaryOp[Double,DenseVectorCol[Int],OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulDoubleDenseVectorColInt extends GeneratedBinaryOp[Double,DenseVectorCol[Int],OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivDoubleDenseVectorColInt extends GeneratedBinaryOp[Double,DenseVectorCol[Int],OpDiv,DenseVectorCol[Double]](OpDiv)
  implicit object OpAddDoubleDenseVectorColLong extends GeneratedBinaryOp[Double,DenseVectorCol[Long],OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubDoubleDenseVectorColLong extends GeneratedBinaryOp[Double,DenseVectorCol[Long],OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulDoubleDenseVectorColLong extends GeneratedBinaryOp[Double,DenseVectorCol[Long],OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivDoubleDenseVectorColLong extends GeneratedBinaryOp[Double,DenseVectorCol[Long],OpDiv,DenseVectorCol[Double]](OpDiv)
  implicit object OpAddDoubleDenseVectorColFloat extends GeneratedBinaryOp[Double,DenseVectorCol[Float],OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubDoubleDenseVectorColFloat extends GeneratedBinaryOp[Double,DenseVectorCol[Float],OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulDoubleDenseVectorColFloat extends GeneratedBinaryOp[Double,DenseVectorCol[Float],OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivDoubleDenseVectorColFloat extends GeneratedBinaryOp[Double,DenseVectorCol[Float],OpDiv,DenseVectorCol[Double]](OpDiv)
  implicit object OpAddDoubleDenseVectorColDouble extends GeneratedBinaryOp[Double,DenseVectorCol[Double],OpAdd,DenseVectorCol[Double]](OpAdd)
  implicit object OpSubDoubleDenseVectorColDouble extends GeneratedBinaryOp[Double,DenseVectorCol[Double],OpSub,DenseVectorCol[Double]](OpSub)
  implicit object OpMulDoubleDenseVectorColDouble extends GeneratedBinaryOp[Double,DenseVectorCol[Double],OpMul,DenseVectorCol[Double]](OpMul)
  implicit object OpDivDoubleDenseVectorColDouble extends GeneratedBinaryOp[Double,DenseVectorCol[Double],OpDiv,DenseVectorCol[Double]](OpDiv)
}


/*
 * Distributed as part of Scalala, a linear algebra library.
 * 
 * Copyright (C) 2008- Daniel Ramage
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA 
 */
package scalala.tensor.operators;

import scalala.collection.{MergeableSet, IntSpanSet, ProductSet, DomainException};
                             
import scalala.tensor.{Vector,Matrix};

import TensorShapes._;

/** Type aliases supporting Matrix operations. */
object MatrixTypes {
  type MatrixOp[M<:Matrix] =
    TensorOp[(Int,Int),Matrix,M,Shape2[Int,Int]];

  type MatrixTranspose[M<:Matrix,T<:Matrix] =
    Tensor2Transpose[Int,Int,Matrix,M,Matrix,T];
}

import MatrixTypes._;
import VectorTypes._;

/** Implicits supporting Matrix operations. */
trait MatrixOps {
}

/** Singleton instance of MatrixOps trait. */
object MatrixOps extends MatrixOps;

/**
 * Matrix methods. Because Matrix isn't parameterized, there is no
 * clean way to inherit from Tensor2Op*, so there is some code duplication
 * here.
 */
class RichMatrixOp[M<:Matrix](base : MatrixOp[M])
extends RichTensorOp[(Int,Int),Matrix,M,Shape2[Int,Int]](base) {
  
  def t = new MatrixTranspose[M,Matrix](base);
  
  /** Matrix-matrix multiplication */
  def *[M2<:Matrix] (op : MatrixOp[M2])
    (implicit tpB: TensorProductBuilder[M,M2,M]) =
    MatrixMultMatrix(base,op);
  
  /** Matrix-vector multiplication */
  def *[V2<:Vector] (op : ColVectorOp[V2])
    (implicit tpB: TensorProductBuilder[M,V2,Vector]) =
    MatrixMultColVector[M,V2,Vector](base, op);
}

/**
 * Matrix-matrix multiplication.
 */
case class MatrixMultMatrix[M1<:Matrix,M2<:Matrix]
(a : MatrixOp[M1], b : MatrixOp[M2])
(implicit tpB: TensorProductBuilder[M1,M2,M1])
extends MatrixOp[M1] {
  if (a.domain.asInstanceOf[ProductSet[Int,Int]]._2 != b.domain.asInstanceOf[ProductSet[Int,Int]]._1)
    throw new DomainException;
  
  override def domain =
    ProductSet[Int,Int](a.domain.asInstanceOf[ProductSet[Int,Int]]._1, b.domain.asInstanceOf[ProductSet[Int,Int]]._2);
  
  override def value = {
    val innerDomain = a.domain.asInstanceOf[ProductSet[Int,Int]]._2;
    val av = a.value;
    val bv = b.value;
    val rv = tpB.create(av,bv);
    for (i <- domain._1; j <- domain._2) {
      rv(i,j) = av.getRow(i) dot bv.getCol(j);
    }
    rv;
  }
}

/**
 * Matrix-vector multiplication.
 */
case class MatrixMultColVector[M<:Matrix,V<:Vector,V2<:Vector]
(a : MatrixOp[M], b : ColVectorOp[V])
(implicit tpB: TensorProductBuilder[M,V,V2])
extends RowVectorOp[V2] {
  if (a.domain.asInstanceOf[ProductSet[Int,Int]]._2 != b.domain)
    throw new DomainException;
    
  override def domain = a.domain.asInstanceOf[ProductSet[Int,Int]]._1;
  
  override def value = {
    val mv = a.value;
    val vv = b.value;
    val rv = tpB.create(mv,vv);
    for (i <- domain) {
      rv(i) = mv.getRow(i) dot vv;
    }
    rv;
  }
  
}

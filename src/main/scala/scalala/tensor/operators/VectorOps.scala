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
package scalala;
package tensor;
package operators;

import collection.{MergeableSet, IntSpanSet, ProductSet, DomainException};
import Tensor1Types._;
import Tensor2Types._;
import TensorShapes._;

object VectorTypes {
  type ColVectorOp[V<:Vector] =
    ColTensor1Op[V];
  
  type RowVectorOp[V<:Vector] =
    RowTensor1Op[V];
}

import VectorTypes._;
import MatrixTypes._;

trait VectorOps extends TensorOps {
  implicit val vectorColArith = new Tensor1Arith[Int,Vector,Tensor1[Int],Shape1Col];
  implicit val vectorRowArith = new Tensor1Arith[Int,Vector,Tensor1[Int],Shape1Row];

  implicit val vectorPBuilder : TensorProductBuilder[Vector,Vector,Matrix,Shape1Col,Shape1Row,Shape2] = {
    new TensorProductBuilder[Vector,Vector,Matrix,Shape1Col,Shape1Row,Shape2] {
      def create(t: Vector,t2: Vector) = t.matrixLike(t.size,t2.size);
      def makeProduct(t: ColTensor1Op[Vector], t2: RowTensor1Op[Vector]) = {
        Tensor1OuterMultTensor1[Int,Int,Vector,Vector,Matrix](t,t2);
      }
    }
  }

  implicit val vectorMPBuilder : TensorProductBuilder[Vector,Matrix,Vector,Shape1Row,Shape2,Shape1Row] = {
    new TensorProductBuilder[Vector,Matrix,Vector,Shape1Row,Shape2,Shape1Row] {
      def create(t: Vector,t2: Matrix) = t.vectorLike(t2.cols);
      def makeProduct(t: RowTensor1Op[Vector], t2: Tensor2Op[Matrix]) = {
        RowTensor1MultTensor2[Int,Int,Vector,Vector,Matrix](t,t2);
      }
    }
  }

}

object VectorOps extends VectorOps;

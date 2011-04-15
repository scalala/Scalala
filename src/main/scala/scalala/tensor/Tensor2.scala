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
package tensor;

import scalar.Scalar;

import domain._;

import scalala.generic.collection._;
import scalala.operators._;

import generic.TensorTriplesMonadic;


/**
 * Implementation trait for tensors indexed by two keys, such as Matrices.
 *
 * TODO: Make up mind about whether implementing classes should provide
 * which one (or both) of the pairs interface and triples interface.
 *
 * @author dramage
 */
trait Tensor2Like
[@specialized(Int) K1, @specialized(Int) K2,
 @specialized(Int,Long,Float,Double,Boolean) V,
 +D1<:Domain1[K1],
 +D2<:Domain1[K2],
 +D<:Domain2[K1,K2],
 +T<:Domain2[K2,K1],
 +This<:Tensor2[K1,K2,V]]
extends TensorLike[(K1,K2),V,D,This]
with operators.MatrixOps[This] { self =>
  def checkKey(k1 : K1, k2 : K2) : Unit = {
    if (!domain._1.contains(k1) || !domain._2.contains(k2)) {
      throw new DomainException((k1,k2)+" not in domain");
    }
  }

  /* final */ override def checkKey(pos : (K1,K2)) : Unit =
    checkKey(pos._1, pos._2);

  /** Gets the value indexed by (i,j). */
  def apply(i : K1, j : K2) : V;

  /** Fixed alias for apply(i,j). */
  /* final */ override def apply(pos : (K1,K2)) : V =
    apply(pos._1, pos._2);

  /** Select all columns within a given row. */
  def apply[TT>:This,That](i : K1, j : SelectAll)(implicit bf : CanSliceRow[TT,K1,That]) : That =
    bf.apply(repr, i);

  /** Select all rows within a given column. */
  def apply[TT>:This,That](i : SelectAll, j : K2)(implicit bf : CanSliceCol[TT,K2,That]) : That =
    bf.apply(repr, j);

  /** Select a sub-matrix of the requested rows and columns. */
  def apply[TT>:This,That](i : Seq[K1], j : Seq[K2])(implicit bf : CanSliceMatrix[TT,K1,K2,That]) : That =
    bf.apply(repr, i, j);

  /** Select all columns for a specified set of rows. */
  def apply[TT>:This,That](i : Seq[K1], j : SelectAll)(implicit bf : CanSliceMatrix[TT,K1,K2,That]) : That =
    bf.apply(repr, i, domain._2.toIndexedSeq);

  /** Select all rows for a specified set of columns. */
  def apply[TT>:This,That](i : SelectAll, j : Seq[K2])(implicit bf : CanSliceMatrix[TT,K1,K2,That]) : That =
    bf.apply(repr, domain._1.toIndexedSeq, j);

  /** Select specified columns within a row. */
  def apply[TT>:This,That1,That2](i : K1, j : IndexedSeq[K2])
  (implicit s1 : CanSliceRow[TT,K1,That1], s2 : CanSliceVector[That1,K2,That2]) : That2 =
    s2.apply(s1.apply(repr, i), j);

  /** Select specified rows within a column. */
  def apply[TT>:This,That1,That2](i : IndexedSeq[K1], j : K2)
  (implicit s1 : CanSliceCol[TT,K2,That1], s2 : CanSliceVector[That1,K1,That2]) : That2 =
    s2.apply(s1.apply(repr, j), i);
  
  /** Transpose of the matrix. */
  def t : Tensor2[K2,K1,V] =
    new Tensor2Transpose.Impl[K2,K1,V,This](repr);

  //
  // triples accessors
  //

  def triples : TensorTriplesMonadic[K1,K2,V,This] =
    new TensorTriplesMonadic[K1,K2,V,This] { override def repr = self.repr; }

  def foreachTriple[U](fn : (K1,K2,V)=>U) : Unit =
    foreachPair((k,v) => fn(k._1, k._2, v));

  def foreachNonZeroTriple[U](fn : (K1,K2,V)=>U) : Unit =
    foreachNonZeroPair((k,v) => fn(k._1, k._2, v));
  
  def mapTriples[TT>:This,RV,That](fn : (K1,K2,V)=>RV)
  (implicit bf : CanMapKeyValuePairs[TT, (K1,K2), V, RV, That]) : That =
    mapPairs[TT,RV,That]((k,v) => fn(k._1,k._2,v))(bf);

  def mapNonZeroTriples[TT>:This,RV,That](fn : (K1,K2,V)=>RV)
  (implicit bf : CanMapKeyValuePairs[TT, (K1,K2), V, RV, That]) : That =
    mapNonZeroPairs[TT,RV,That]((k,v) => fn(k._1,k._2,v))(bf);

  def triplesIterator : Iterator[(K1,K2,V)] =
    pairsIterator.map(pair => (pair._1._1,pair._1._2,pair._2));

  def triplesIteratorNonZero : Iterator[(K1,K2,V)] =
    pairsIteratorNonZero.map(pair => (pair._1._1,pair._1._2,pair._2));

  //
  // equality
  //

  override protected def canEqual(other : Any) : Boolean = other match {
    case that : Tensor2[_,_,_] => true;
    case _ => false;
  }
}

/**
 * Tensors indexed by two keys, such as matrices.
 *
 * @author dramage
 */
trait Tensor2
[@specialized(Int) K1, @specialized(Int) K2,
 @specialized(Int,Long,Float,Double,Boolean) V]
extends Tensor[(K1,K2),V]
with Tensor2Like[K1,K2,V,Domain1[K1],Domain1[K2],Domain2[K1,K2],Domain2[K2,K1],Tensor2[K1,K2,V]]

object Tensor2 {
  /** Constructs a tensor for the given domain. */
  def apply[K1,K2,V:Scalar](domain : Domain2[K1,K2]) =
    mutable.Tensor2.apply(domain);

  implicit def canSliceRow[K1,K2,V:Scalar] : CanSliceRow[Tensor2[K1,K2,V],K1,Tensor1Row[K2,V]]
  = new CanSliceRow[Tensor2[K1,K2,V],K1,Tensor1Row[K2,V]] {
    override def apply(from : Tensor2[K1,K2,V], row : K1) =
      new RowSliceImpl[K1,K2,V,Tensor2[K1,K2,V]](from,row);
  }

  implicit def canSliceCol[K1,K2,V:Scalar] : CanSliceCol[Tensor2[K1,K2,V],K2,Tensor1Col[K1,V]]
  = new CanSliceCol[Tensor2[K1,K2,V],K2,Tensor1Col[K1,V]] {
    override def apply(from : Tensor2[K1,K2,V], col : K2) =
      new ColSliceImpl[K1,K2,V,Tensor2[K1,K2,V]](from, col);
  }

  implicit def canSliceMatrix[K1,K2,V:Scalar]
  : CanSliceMatrix[Tensor2[K1,K2,V],K1,K2,Matrix[V]]
  = new CanSliceMatrix[Tensor2[K1,K2,V],K1,K2,Matrix[V]] {
    override def apply(from : Tensor2[K1,K2,V], keys1 : Seq[K1], keys2 : Seq[K2]) =
      new MatrixSliceImpl[K1,K2,V,Tensor2[K1,K2,V]](from, keys1, keys2);
  }

  implicit def canMulTensor2ByTensor1Col[
    @specialized(Int) K1, @specialized(Int) K2,
    @specialized(Int,Double) V1, K, AR, ADomainRow, ADomainCol, ADomain,
    @specialized(Int,Double) V2, V, BD,
    @specialized(Int,Double) RV, That]
  (implicit
   viewA : K=>Tensor2[K1,K2,V1],
   sliceA : CanSliceRow[K,K1,AR],
   domainA : CanGetDomain2[K,ADomainRow,ADomainCol,ADomain],
   viewB : V=>Tensor1Col[K2,V2],
   mul : BinaryOp[AR,V,OpMulRowVectorBy,RV],
   bf : CanBuildTensorFrom[V,ADomainRow,K1,RV,That],
   scalar : Scalar[RV])
  : BinaryOp[K, V, OpMulMatrixBy, That] =
  new BinaryOp[K, V, OpMulMatrixBy, That] {
    override def opType = OpMulMatrixBy;
    override def apply(a : K, b : V) = {
      val builder = bf(b, domainA._1(a));
      for (i <- a.domain._1) {
        builder(i) = mul(sliceA(a,i), b);
      }
      builder.result;
    }
  }

  implicit def canMulMatrixByMatrix[
   @specialized(Int) K1, @specialized(Int) K2, @specialized(Int) K3,
   @specialized(Int,Double) V1, A, ARow, ADomainRow, InnerDomain, ADomain,
   @specialized(Int,Double) V2, B, BCol, BDomainCol, BDomain,
   @specialized(Int,Double) RV, RDomain, That]
  (implicit
    viewA : A=>Tensor2[K1,K2,V1],
    sliceA : CanSliceRow[A,K1,ARow],
    domainA : CanGetDomain2[A,ADomainRow,InnerDomain,ADomain],
    viewB : B=>Tensor2[K2,K3,V2],
    sliceB : CanSliceCol[B,K3,BCol],
    domainB : CanGetDomain2[B,InnerDomain,BDomainCol,BDomain],
    mul : BinaryOp[ARow,BCol,OpMulRowVectorBy,RV],
    domainR : CanBuildDomain2[ADomainRow,BDomainCol,RDomain],
    bf : CanBuildTensorForBinaryOp[A,B,RDomain,(K1,K3),RV,OpMulMatrixBy,That],
    scalar : Scalar[RV])
  : BinaryOp[A, B, OpMulMatrixBy, That] =
  new BinaryOp[A, B, OpMulMatrixBy, That] {
    override def opType = OpMulMatrixBy;
    override def apply(a : A, b : B) = {
      val domain = domainR(domainA._1(a), domainB._2(b));
      val builder = bf(a, b, domain);
      for (i <- a.domain._1; j <- b.domain._2) {
        builder((i,j)) = mul(sliceA(a,i), sliceB(b,j));
      }
      builder.result;
    }
  }

  trait RowSliceLike[K1,K2,V,+Coll<:Tensor2[K1,K2,V],+This<:RowSlice[K1,K2,V,Coll]]
  extends Tensor1SliceLike[(K1,K2),IterableDomain[(K1,K2)],K2,Domain1[K2],V,Coll,This] with Tensor1RowLike[K2,V,Domain1[K2],This] {
    def row : K1;
    override def domain = underlying.domain._2;
    override def size = domain.size;
    override def lookup(key : K2) = (row,key);
  }

  trait RowSlice[K1,K2,V,+Coll<:Tensor2[K1,K2,V]]
  extends Tensor1Slice[(K1,K2),K2,V,Coll] with Tensor1Row[K2,V] with RowSliceLike[K1,K2,V,Coll,RowSlice[K1,K2,V,Coll]];

  class RowSliceImpl[K1,K2,V,+Coll<:Tensor2[K1,K2,V]]
  (override val underlying : Coll, override val row : K1)
  (implicit override val scalar : Scalar[V])
  extends RowSlice[K1,K2,V,Coll];

  trait ColSliceLike[K1,K2,V,+Coll<:Tensor2[K1,K2,V],+This<:ColSlice[K1,K2,V,Coll]]
  extends Tensor1SliceLike[(K1,K2),IterableDomain[(K1,K2)],K1,Domain1[K1],V,Coll,This] with Tensor1ColLike[K1,V,Domain1[K1],This] {
    def col : K2;
    override val domain = underlying.domain._1;
    override def size = domain.size;
    override def lookup(key : K1) = (key,col);
  }

  trait ColSlice[K1,K2,V,+Coll<:Tensor2[K1,K2,V]]
  extends Tensor1Slice[(K1,K2),K1,V,Coll] with Tensor1Col[K1,V] with ColSliceLike[K1,K2,V,Coll,ColSlice[K1,K2,V,Coll]];

  class ColSliceImpl[K1,K2,V,+Coll<:Tensor2[K1,K2,V]]
  (override val underlying : Coll, override val col : K2)
  (implicit override val scalar : Scalar[V])
  extends ColSlice[K1,K2,V,Coll];

  trait MatrixSliceLike
  [@specialized(Int) K1, @specialized(Int) K2,
   @specialized(Int,Long,Float,Double,Boolean) V,
   +D1<:Domain1[K1],
   +D2<:Domain1[K2],
   +D<:Domain2[K1,K2],
   +T<:Domain2[K2,K1],
   +Coll<:Tensor2[K1,K2,V],
   +This<:MatrixSlice[K1,K2,V,Coll]]
  extends TensorSliceLike[(K1,K2),D,(Int,Int),TableDomain,V,Coll,This]
  with MatrixLike[V,This] {

    def lookup1(i : Int) : K1;
    def lookup2(j : Int) : K2;

    /* final */ override def lookup(tup : (Int,Int)) =
      (lookup1(tup._1), lookup2(tup._2));

    override def apply(i : Int, j : Int) : V =
      underlying.apply(lookup1(i), lookup2(j));
  }

  trait MatrixSlice
  [@specialized(Int,Long) K1, @specialized(Int,Long) K2,
   @specialized(Int,Long,Float,Double,Boolean) V,
   +Coll<:Tensor2[K1,K2,V]]
  extends TensorSlice[(K1,K2),(Int,Int),V,Coll]
  with Matrix[V]
  with MatrixSliceLike[K1,K2,V,Domain1[K1],Domain1[K2],Domain2[K1,K2],Domain2[K2,K1],Coll,MatrixSlice[K1,K2,V,Coll]];

  class MatrixSliceImpl[K1, K2, V, +Coll<:Tensor2[K1,K2,V]]
  (override val underlying : Coll, val keys1 : Seq[K1], val keys2 : Seq[K2])
  (implicit override val scalar : Scalar[V])
  extends MatrixSlice[K1, K2, V, Coll] {
    override def numRows = keys1.size;
    override def numCols = keys2.size;
  
    override def lookup1(i : Int) = keys1(i);
    override def lookup2(j : Int) = keys2(j);

    override val domain = TableDomain(keys1.length, keys2.length);
  }
}


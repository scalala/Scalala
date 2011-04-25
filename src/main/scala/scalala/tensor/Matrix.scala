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

import domain.{DomainException,IndexDomain,TableDomain}

import scalala.scalar.Scalar;
import scalala.generic.collection._;
import scalala.operators._;

/**
 * Implementation trait for a matrix.
 *
 * @author dramage
 */
trait MatrixLike[@specialized(Int,Long,Float,Double) V, +This<:Matrix[V]]
extends Tensor2Like[Int,Int,V,IndexDomain,IndexDomain,TableDomain,TableDomain,This] {
self =>

  /** Number of rows in this table. */
  def numRows : Int;

  /** Number of columsn in this table. */
  def numCols : Int;

  override def size = numRows * numCols;

  override def domain = TableDomain(numRows, numCols);

  override def checkKey(row : Int, col : Int) {
    if (row < 0 || row >= numRows || col < 0 || col >= numCols)
      throw new DomainException("Index "+(row,col)+" out of range.  Size is "+numRows+"x"+numCols);
  }

  override def foreachKey[U](fn : ((Int,Int))=>U) =
    for (i <- 0 until numRows; j <- 0 until numCols) fn((i,j));

  /** Returns true if this matrix has the same number of rows as columns. */
  def isSquare : Boolean =
    numRows == numCols;
  
  /** Returns true if this matrix is symmetric. */
  def isSymmetric : Boolean = {
    if (!isSquare) return false;
    
    var i = 0;
    while (i < numRows) {
      var j = i + 1;
      while (j < numCols) {
        if (this(i,j) != this(j,i)) {
          return false;
        }
        j += 1;
      }
      i += 1;
    }
    
    return true;
  }

  /** Returns the sum of the diagonal elements of this matrix. */
  def trace(implicit add : BinaryOp[V,V,OpAdd,V]) : V= {
    var rv = this(0,0);
    var i = 1;
    var n = math.min(numRows, numCols);
    while (i < n) {
      rv = add(rv, this(i,i));
      i += 1;
    }
    rv;
  }

  def toString(maxLines : Int, maxWidth : Int) : String = {
    val showRows = if (numRows > maxLines) maxLines - 1 else numRows;
    def colWidth(col : Int) =
      (0 until showRows).map(row => mkValueString(this(row,col)).length+2).max;

    val colWidths = new scala.collection.mutable.ArrayBuffer[Int];
    var col = 0;
    while (col < numCols && colWidths.sum < maxWidth) {
      colWidths += colWidth(col);
      col += 1;
    }
    // make space for "... (7 more)"
    while (colWidths.sum + (numCols - colWidths.size).toString.length + 11 > maxWidth) {
      colWidths.remove(colWidths.length - 1);
    }

    val newline = System.getProperty("line.separator");

    var rv = new scala.StringBuilder;
    for (row <- 0 until showRows; col <- 0 until colWidths.length) {
      val cell = mkValueString(this(row,col));
      rv.append(cell);
      rv.append(" " * (colWidths(col) - cell.length));
      if (col == colWidths.length - 1) {
        if (col < numCols - 1) {
          rv.append("...");
          if (row == 0) {
            rv.append(" (");
            rv.append(numCols - colWidths.size);
            rv.append(" more)");
          }
        }
        if (row + 1 < showRows) {
          rv.append(newline);
        }
      }
    }
    
    if (numRows > showRows) {
      rv.append(newline);
      rv.append("... (");
      rv.append(numRows - showRows);
      rv.append(" more)");
    }

    rv.toString;
  }

  override def toString : String =
    toString(maxLines = 11, maxWidth = 72);

  override protected def canEqual(other : Any) : Boolean = other match {
    case that : Matrix[_] => true;
    case _ => false;
  }
  
  override def t : Matrix[V] =
    new MatrixTranspose.Impl[V,Matrix[V]](repr);
}

trait Matrix[@specialized(Int,Long,Float,Double) V]
extends Tensor2[Int,Int,V]
with MatrixLike[V,Matrix[V]];

object Matrix {
  implicit def canSliceRow[V:Scalar] : CanSliceRow[Matrix[V],Int,VectorRow[V]]
  = new CanSliceRow[Matrix[V],Int,VectorRow[V]] {
    override def apply(from : Matrix[V], row : Int) =
      new RowSliceImpl[V,Matrix[V]](from,row);
  }

  implicit def canSliceCol[V:Scalar] : CanSliceCol[Matrix[V],Int,VectorCol[V]]
  = new CanSliceCol[Matrix[V],Int,VectorCol[V]] {
    override def apply(from : Matrix[V], col : Int) =
      new ColSliceImpl[V,Matrix[V]](from, col);
  }

  implicit def canSliceMatrix[V:Scalar] : CanSliceMatrix[Matrix[V],Int,Int,Matrix[V]]
  = new CanSliceMatrix[Matrix[V],Int,Int,Matrix[V]] {
    override def apply(from : Matrix[V], keys1 : Seq[Int], keys2 : Seq[Int]) =
      new MatrixSliceImpl[V,Matrix[V]](from, keys1, keys2);
  }

  trait RowSliceLike[V,+Coll<:Matrix[V],+This<:RowSlice[V,Coll]]
  extends VectorSliceLike[(Int,Int),TableDomain,V,Coll,This] with VectorRowLike[V,This] {
    def row : Int;
    override val domain = underlying.domain._2;
    override def length = underlying.numCols;
    override def lookup(key : Int) = (row,key);
  }

  trait RowSlice[V,+Coll<:Matrix[V]]
  extends VectorSlice[(Int,Int),V,Coll] with VectorRow[V] with RowSliceLike[V,Coll,RowSlice[V,Coll]];

  class RowSliceImpl[V,+Coll<:Matrix[V]]
  (override val underlying : Coll, override val row : Int)
  (implicit override val scalar : Scalar[V])
  extends RowSlice[V,Coll];

  trait ColSliceLike[V,+Coll<:Matrix[V],+This<:ColSlice[V,Coll]]
  extends VectorSliceLike[(Int,Int),TableDomain,V,Coll,This] with VectorColLike[V,This] {
    def col : Int;
    override def domain = underlying.domain._1;
    override def length = underlying.numRows;
    override def lookup(key : Int) = (key,col);
  }

  trait ColSlice[V,+Coll<:Matrix[V]]
  extends VectorSlice[(Int,Int),V,Coll] with VectorCol[V] with ColSliceLike[V,Coll,ColSlice[V,Coll]];

  class ColSliceImpl[V,+Coll<:Matrix[V]]
  (override val underlying : Coll, override val col : Int)
  (implicit override val scalar : Scalar[V])
  extends ColSlice[V,Coll];

  trait MatrixSliceLike[@specialized(Int,Long,Float,Double,Boolean) V,
   +Coll<:Matrix[V], +This<:MatrixSlice[V,Coll]]
  extends TensorSliceLike[(Int,Int),TableDomain,(Int,Int),TableDomain,V,Coll,This]
  with MatrixLike[V,This] {

    def lookup1(i : Int) : Int;
    def lookup2(j : Int) : Int;

    /* final */ override def lookup(tup : (Int,Int)) =
      (lookup1(tup._1), lookup2(tup._2));

    override def apply(i : Int, j : Int) : V =
      underlying.apply(lookup1(i), lookup2(j));
  }

  trait MatrixSlice[@specialized(Int,Long,Float,Double,Boolean) V,
   +Coll<:Matrix[V]]
  extends TensorSlice[(Int,Int),(Int,Int),V,Coll]
  with Matrix[V] with MatrixSliceLike[V,Coll,MatrixSlice[V,Coll]];

  class MatrixSliceImpl[V, +Coll<:Matrix[V]]
  (override val underlying : Coll, val keys1 : Seq[Int], val keys2 : Seq[Int])
  (implicit override val scalar : Scalar[V])
  extends MatrixSlice[V, Coll] {
    override def numRows = keys1.size;
    override def numCols = keys2.size;
    override def lookup1(i : Int) = keys1(i);
    override def lookup2(j : Int) = keys2(j);

    override val domain = TableDomain(keys1.length, keys2.length);
  }

  implicit def canMulMatrixByMatrix[
   @specialized(Int,Double) V1, A, ARow, ADomainRow, InnerDomain, ADomain,
   @specialized(Int,Double) V2, B, BCol, BDomainCol, BDomain,
   @specialized(Int,Double) RV, RDomain, That]
  (implicit
    viewA : A=>Matrix[V1],
    sliceA : CanSliceRow[A,Int,ARow],
    viewB : B=>Matrix[V2],
    sliceB : CanSliceCol[B,Int,BCol],
    mul : BinaryOp[ARow,BCol,OpMulRowVectorBy,RV],
    bf : CanBuildTensorFrom[A,TableDomain,(Int,Int),RV,That],
    scalar : Scalar[RV])
  : BinaryOp[A, B, OpMulMatrixBy, That] =
  new BinaryOp[A, B, OpMulMatrixBy, That] {
    override def opType = OpMulMatrixBy;
    override def apply(a : A, b : B) = {
      val domain = new TableDomain(a.numRows,b.numCols)
      val builder = bf(a, domain);
      for (i <- a.domain._1; j <- b.domain._2) {
        builder(i->j) = mul(sliceA(a,i), sliceB(b,j));
      }
      builder.result;
    }
  }

//  implicit def canAppendMatrixColumns[V]
//  : CanAppendColumns[Bound[V],Matrix[V],Matrix[V]]
//  = new CanAppendColumns[Bound[V],Matrix[V],Matrix[V]] {
//    override def apply(a : Bound[V], b : Matrix[V]) = {
//      require(a.numRows == b.numRows, "Arguments must have same number of rows");
//      implicit val sv = a.scalar;
//      val builder = a.newBuilder[(Int,Int),V](TableDomain(a.numRows, a.numCols+b.numCols));
//      a.foreachNonZero((i,j,v) => builder((i,j)) = v);
//      b.foreachNonZero((i,j,v) => builder((i,j+a.numCols)) = v);
//      builder.result.asInstanceOf[Matrix[V]];
//    }
//  }
//
//  implicit def canAppendVectorColumn[V]
//  : CanAppendColumns[Bound[V],VectorCol[V],Matrix[V]]
//  = new CanAppendColumns[Bound[V],VectorCol[V],Matrix[V]] {
//    override def apply(a : Bound[V], b : VectorCol[V]) = {
//      require(a.numRows == b.size, "Arguments must have same number of rows");
//      implicit val sv = a.scalar;
//      val builder = a.newBuilder[(Int,Int),V](TableDomain(a.numRows, a.numCols+1));
//      a.foreachNonZero((i,j,v) => builder((i,j)) = v);
//      b.foreachNonZero((i,v) => builder((i,a.numCols)) = v);
//      builder.result.asInstanceOf[Matrix[V]];
//    }
//  }
}


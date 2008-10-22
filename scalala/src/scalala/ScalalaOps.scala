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
package scalala

import ScalalaMTJ._
import ScalalaValues._

/** Operations that act on Scalala values */
object ScalalaOps {
  
  //
  // Implicit conversions
  //
  
  implicit def iScalarOp(s : Int)(implicit pool : EvaluationPool)    : MatrixOp[One,One] = ScalarIdentity(s);
  implicit def iScalarOp(s : Float)(implicit pool : EvaluationPool)  : MatrixOp[One,One] = ScalarIdentity(s);
  implicit def iScalarOp(s : Long)(implicit pool : EvaluationPool)   : MatrixOp[One,One] = ScalarIdentity(s);
  implicit def iScalarOp(s : Double)(implicit pool : EvaluationPool) : MatrixOp[One,One] = ScalarIdentity(s);
  implicit def iScalarOp(s : Short)(implicit pool : EvaluationPool)  : MatrixOp[One,One] = ScalarIdentity(s);
  implicit def iScalarOp(s : Byte)(implicit pool : EvaluationPool)   : MatrixOp[One,One] = ScalarIdentity(s);
  
  implicit def iMatrixOp(m : Matrix)(implicit pool : EvaluationPool) : MatrixOp[Many,Many] = MatrixIdentity(m);
  implicit def iVectorOp(v : Vector)(implicit pool : EvaluationPool) : MatrixOp[Many,One] = VectorIdentity(v);
  
  implicit def iMatrix(op : MatrixOp[Many,Many]) : Matrix = op.asMatrix;
  implicit def iColVector[O<:MatrixOp[One,Many]](op : O) : Vector = op.asVector;
  implicit def iRowVector[O<:MatrixOp[Many,One]](op : O) : Vector = op.asVector;
  implicit def iScalar(op : MatrixOp[One,One]) : Double = op.asScalar;
  
  //
  // For composition of MatrixOps
  //
  
  private def requireSameDimensions[ROW<:CC,COL<:CC](a : MatrixOp[ROW,COL], b : MatrixOp[ROW,COL]) {
    if (a.rows != b.rows || a.cols != b.cols) {
      throw new IllegalArgumentException("Matrix dimensions must agree");
    }
  }
  
  private def requireInnerDimensions[ROW<:CC,INNER<:CC,COL<:CC](a : MatrixOp[ROW,INNER], b : MatrixOp[INNER,COL]) {
    if (a.cols != b.rows) {
      throw new IllegalArgumentException("Inner matrix dimensions must agree: "
                                         +(a.rows,a.cols)+" "+(b.rows,b.cols));
    }
  }
  
  /** Pool of instantiated objects to be modified */
  class EvaluationPool {
    var matrixs = new scala.collection.jcl.LinkedList[Matrix]()
    val vectors = new scala.collection.jcl.LinkedList[Vector]()
    
    /** Returns a temporary vector of the given length from the pool */
    def request(veclen : Int) : Vector = {
      val candidates = vectors.filter(_.size == veclen)
      if (candidates.length > 0) {
        val vector = candidates(0)
        vectors -= vector
        return vector
      } else {
        return DenseVector(veclen)
      }
    }
    
    /** Releases the given vector as re-usable in the evaluation pool */
    def release(v : Vector) : Unit = {
      vectors += v
    }
    
    /** Returns a temporary matrix of the given size from the pool */
    def request(rows : Int, cols : Int) : Matrix = {
      val candidates = matrixs.filter(((m:Matrix) => m.rows == rows && m.cols == cols))
      if (candidates.length > 0) {
        val matrix = candidates(0)
        matrixs -= matrix
        return matrix
      } else {
        return DenseMatrix(rows,cols)
      }
    }
    
    /** Releases the given matrix as re-usable in the evaluation pool */
    def release(m :  Matrix) : Unit = {
      matrixs += m
    }
  }
  
  //
  // Cardinality of a matrix dimension for MatrixOp
  //
  
  sealed abstract case class Cardinality()
  sealed case class One() extends Cardinality()
  sealed case class Many() extends Cardinality()
  
  type CC = Cardinality

  //
  // Matrix operations
  //
  
  abstract class MatrixOp[ROW<:CC,COL<:CC](implicit pool : EvaluationPool) {
    //
    // Standard properties
    //
      
    def rows : Int
    def cols : Int
    def size = (rows,cols)
    
    def isVector = { Math.min(rows,cols) == 1 }
    def isScalar = { rows == 1 && cols == 1 }
    
    /** By default, all operations are intermediate */
    def intermediate = true
    
    //
    // Evaluators
    //
    
    /** Computes the value of the given operation as a matrix */
    def asMatrix : Matrix
    
    /**
     * Computes the value of the given operation as a scalar.  Only defined
     * for MatrixOp[One,Many] or MatrixOp[Many,One]
     */
    def asVector : Vector = {
      if (!isVector) {
        throw new IllegalArgumentException("Not a vector");
      }
      
      val matrix = this.asMatrix;
      val vector = DenseVector(Math.max(rows,cols));
      for (entry <- matrix) {
        vector.set(Math.max(entry.row,entry.col),entry.get);
      }
      return vector;
    }
    
    /**
     * Computes the value of the given operation as a matrix.  Only defined
     * for MatrixOp[One,One].
     */
    def asScalar : Double = {
      assert(isScalar)
      return asMatrix.get(0,0)
    }
    
    
    //
    // unary matrix operations
    //
    
    def tr : MatrixOp[COL,ROW] =
      MatrixTranspose(this);
    
    def unary_! : MatrixOp[ROW,COL] =
      MatrixMap(this,{x:Double => if (x == 0.0) 1.0 else 0.0 });
    
    def unary_~ : MatrixOp[ROW,COL] =
      !this;

    //
    // matrix-matrix operations
    //
    
    def  +        (b : MatrixOp[ROW,COL]) =
      MatrixPlusMatrix(this, b);
    
    def  -        (b : MatrixOp[ROW,COL]) =
      MatrixPlusMatrixMultScalar(this, b, ScalarIdentity(-1.0));
    
    def  *[S<:CC] (b : MatrixOp[COL,S])   =
      MatrixMultMatrix(this, b);
    
    def :*        (b : MatrixOp[ROW,COL]) =
      MatrixJoinInnerMap(this,b,{(row:Int,col:Int,ax:Double,bx:Double) => ax*bx});
    
    def :/        (b : MatrixOp[ROW,COL]) =
      MatrixJoinOuterMap(this,b,{(row:Int,col:Int,ax:Double,bx:Double) => ax/bx});
    
    def :^        (b : MatrixOp[ROW,COL]) =
      MatrixJoinOuterMap(this,b,{(row:Int,col:Int,ax:Double,bx:Double) => Math.pow(ax,bx)});
    
    def <         (b : MatrixOp[ROW,COL]) =
      MatrixJoinOuterMap(this,b,{(row:Int,col:Int,ax:Double,bx:Double) => if (ax < bx)  1.0 else 0.0 });
    
    def <=        (b : MatrixOp[ROW,COL]) =
      MatrixJoinOuterMap(this,b,{(row:Int,col:Int,ax:Double,bx:Double) => if (ax <= bx) 1.0 else 0.0 });
    
    def >         (b : MatrixOp[ROW,COL]) =
      MatrixJoinOuterMap(this,b,{(row:Int,col:Int,ax:Double,bx:Double) => if (ax > bx)  1.0 else 0.0 });
    
    def >=        (b : MatrixOp[ROW,COL]) =
      MatrixJoinOuterMap(this,b,{(row:Int,col:Int,ax:Double,bx:Double) => if (ax >= bx) 1.0 else 0.0 });
    
    def &&        (b : MatrixOp[ROW,COL]) =
      MatrixJoinOuterMap(this,b,{(row:Int,col:Int,ax:Double,bx:Double) => if (ax != 0.0 && bx != 0.0) 1.0 else 0.0});
    
    def ||        (b : MatrixOp[ROW,COL]) =
      MatrixJoinOuterMap(this,b,{(row:Int,col:Int,ax:Double,bx:Double) => if (ax != 0.0 || bx != 0.0) 1.0 else 0.0});
    
    //
    // matrix-matrix aliases, equivalent to matlab expressions
    //
    
    def :+        (b : MatrixOp[ROW,COL]) = this.+(b);
    
    def :-        (b : MatrixOp[ROW,COL]) = this.-(b);
    
    def :<        (b : MatrixOp[ROW,COL]) = this.<(b);
    
    def :<=       (b : MatrixOp[ROW,COL]) = this.<=(b);
    
    def :>        (b : MatrixOp[ROW,COL]) = this.>(b);
    
    def :>=       (b : MatrixOp[ROW,COL]) = this.>=(b);
    
    def :&&       (b : MatrixOp[ROW,COL]) = this.&&(b);
    
    def :||       (b : MatrixOp[ROW,COL]) = this.||(b);
    
    //
    // matrix-scalar operations
    //
    
    def  * (s : MatrixOp[One,One]) =
      MatrixMultScalar(this, s, {ss:Double => ss});
    
    def  / (s : MatrixOp[One,One]) =
      MatrixMultScalar(this, s, {ss:Double => 1.0 / ss});
    
    def  + (s : MatrixOp[One,One]) =
      MatrixPlusScalar(this, s, {ss:Double => ss});
    
    def  - (s : MatrixOp[One,One]) =
      MatrixPlusScalar(this, s, {ss:Double => -ss});
    
    def  < (s : MatrixOp[One,One]) = {
      val cmp = s.asScalar;
      MatrixMap(this,{x:Double => if (x < cmp)  1.0 else 0.0});
    }
    
    def <= (s : MatrixOp[One,One]) = {
      val cmp = s.asScalar;
      MatrixMap(this,{x:Double => if (x <= cmp) 1.0 else 0.0});
    }
    
    def  > (s : MatrixOp[One,One]) = {
      val cmp = s.asScalar;
      MatrixMap(this,{x:Double => if (x > cmp)  1.0 else 0.0});
    }
    
    def >= (s : MatrixOp[One,One]) = {
      val cmp = s.asScalar;
      MatrixMap(this,{x:Double => if (x >= cmp) 1.0 else 0.0});
    }
    
    def :^ (s : MatrixOp[One,One]) = {
      val exp = s.asScalar;
      MatrixNZMap(this,{x:Double => Math.pow(x,exp)});
    }
    
    //
    // matrix-scalar aliases
    //
    
    def :* (s : MatrixOp[One,One]) = this.*(s);
    
    def :/ (s : MatrixOp[One,One]) = this./(s);
  }
  
  
  //
  // basic unary opertaions
  //
  
  case class ScalarIdentity(s : Double)(implicit pool : EvaluationPool) extends MatrixOp[One,One] {
    override def rows = 1
    override def cols = 1
    override def intermediate = false
    
    override def asScalar = s
    override def asMatrix = ScalarMatrix(s,1,1)
  } 
  
  case class VectorIdentity(v: Vector)(implicit pool : EvaluationPool) extends MatrixOp[Many,One] {
    override def rows = v.size
    override def cols = 1
    override def intermediate = false
    
    override def asVector = v
    override def asMatrix = ColMatrix(v)
    
    override def toString : String = "VectorIdentity("+System.identityHashCode(v)+")"
  }
    
  case class MatrixIdentity(m: Matrix)(implicit pool : EvaluationPool) extends MatrixOp[Many,Many] {
    override def rows = m.rows
    override def cols = m.cols
    override def intermediate = false
    
    override def asMatrix : Matrix = m
    
    override def toString : String = "MatrixIdentity("+System.identityHashCode(m)+")"
  }
  
  case class MatrixTranspose[ROW<:CC,COL<:CC](a: MatrixOp[ROW,COL])(implicit pool : EvaluationPool) extends MatrixOp[COL,ROW] {
    override def rows = a.cols
    override def cols = a.rows
    
    override def asMatrix : Matrix = TransposeMatrix(a.asMatrix)
  }
  
  //
  // basic pairwise matrix operations
  //
    
  case class MatrixPlusMatrix[ROW<:CC,COL<:CC](a: MatrixOp[ROW,COL], b: MatrixOp[ROW,COL])(implicit pool : EvaluationPool) extends MatrixOp[ROW,COL] {
    requireSameDimensions(a,b)
    override def rows = a.rows
    override def cols = a.cols
    
    override def asMatrix : Matrix = {
      val ma = MTJMatrix(a.asMatrix)
      val mb = MTJMatrix(b.asMatrix)
      if (a.intermediate && b.intermediate) {
        val rv = ma.add(mb)
        pool.release(mb)
        return rv
      } else if (a.intermediate) {
        return ma.add(mb)
      } else if (b.intermediate) {
        return mb.add(ma)
      } else {
        val rv = MTJMatrix(pool.request(rows,cols))
        rv.set(ma)
        rv.add(mb)
        return rv
      }
    }
  }
  
  case class MatrixPlusMatrixMultScalar[ROW<:CC,COL<:CC](a : MatrixOp[ROW,COL], b : MatrixOp[ROW,COL], s : MatrixOp[One,One])(implicit pool : EvaluationPool) extends MatrixOp[ROW,COL] {
    requireSameDimensions(a,b)
    override def rows = a.rows
    override def cols = a.cols
    
    override def asMatrix : Matrix = {
      val ma = MTJMatrix(a.asMatrix)
      val mb = MTJMatrix(b.asMatrix)
      val ss = s.asScalar
      
      if (a.intermediate && b.intermediate) {
        val rv = ma.add(ss,mb)
        pool.release(mb)
        return rv
      } else if (a.intermediate) {
        return ma.add(ss,mb)
      } else if (b.intermediate) {
        return mb.add(1.0 / ss,ma).scale(ss)
      } else {
        val rv = MTJMatrix(pool.request(rows,cols))
        rv.set(ma)
        rv.add(ss, mb)
        return rv
      }
    }
  }
  
  case class MatrixMultMatrix[ROW<:CC,INNER<:CC,COL<:CC](a: MatrixOp[ROW,INNER], b: MatrixOp[INNER,COL])(implicit pool : EvaluationPool) extends MatrixOp[ROW,COL] {
    requireInnerDimensions(a,b)
    override def rows = a.rows
    override def cols = b.cols
    
    override def asMatrix : Matrix = {
      val ma = MTJMatrix(a.asMatrix)
      val mb = MTJMatrix(b.asMatrix)
      val target = MTJMatrix(pool.request(rows, cols))
      
      ma.mult(mb,target)
      
      if (a.intermediate) { pool.release(ma) }
      if (b.intermediate) { pool.release(mb) }
      
      return target
    }
  }
  
  case class MatrixMultScalar[ROW<:CC,COL<:CC](a: MatrixOp[ROW,COL], s: MatrixOp[One,One], f:(Double=>Double))(implicit pool : EvaluationPool) extends MatrixOp[ROW,COL] {
    override def rows = a.rows
    override def cols = a.cols
    
    override def asMatrix : Matrix = {
      val ma = MTJMatrix(a.asMatrix)
      val ds = f(s.asScalar)
      
      if (a.intermediate) {
        ma.scale(ds)
        return ma
      } else {
        val target = MTJMatrix(pool.request(rows,cols))
        target.set(ds, ma)
        return target
      }
    }
  }
  
  case class MatrixPlusScalar[ROW<:CC,COL<:CC](a: MatrixOp[ROW,COL], s: MatrixOp[One,One], f:(Double=>Double))(implicit pool : EvaluationPool) extends MatrixOp[ROW,COL] {
    override def rows = a.rows
    override def cols = a.cols
    
    override def asMatrix : Matrix = {
      val ma = MTJMatrix(a.asMatrix)
      val ds = f(s.asScalar)
      
      if (a.intermediate) {
        for (i <- 0 until rows; j <- 0 until cols) {
          ma.add(i,j,ds)
        }
        return ma
      } else {
        val target = MTJMatrix(pool.request(rows,cols))
        for (i <- 0 until rows; j <- 0 until cols) {
          target.set(i,j,target.get(i,j)+ds)
        }
        return target
      }
    }
  }
  
  /** Maps all non-zero elements in the matrix according to the function */
  case class MatrixNZMap[ROW<:CC,COL<:CC](a : MatrixOp[ROW,COL], f:(Double=>Double))(implicit pool : EvaluationPool) extends MatrixOp[ROW,COL] {
    override def rows = a.rows
    override def cols = a.cols
    
    override def asMatrix : Matrix = {
      val ma = a.asMatrix
      if (a.intermediate) {
        for (entry <- ma) {
          entry.set(f(entry.get))
        }
        return ma
      } else {
        val rv = pool.request(rows,cols)
        for (entry <- rv) {
          entry.set(0)
        }
        for (entry <- ma) {
          rv.set(entry.row,entry.col,f(entry.get))
        }
        return rv
      }
    }
  }
  
  /** Maps all elements in the matrix according to the function */
  case class MatrixMap[ROW<:CC,COL<:CC](a : MatrixOp[ROW,COL], f:(Double=>Double))(implicit pool : EvaluationPool) extends MatrixOp[ROW,COL] {
    override def rows = a.rows
    override def cols = a.cols
    
    override def asMatrix : Matrix = {
      val ma = a.asMatrix
      if (a.intermediate) {
        for (i <- 0 until rows) {
          for (j <- 0 until cols) {
            ma.set(i,j,f(ma.get(i,j)))
          }
        }
        return ma
      } else {
        val rv = pool.request(rows,cols)
        for (entry <- rv) {
          entry.set(0)
        }
        for (i <- 0 until rows; j <- 0 until cols) {
          rv.set(i,j,f(ma.get(i,j)))
        }
        return rv
      }
    }
  }
  
  type MatrixJoinFunction = ((Int,Int,Double,Double) => Double)
  
  /** Map of inner join on two matrices (both non-zero) */
  case class MatrixJoinInnerMap[ROW<:CC,COL<:CC](a : MatrixOp[ROW,COL], b : MatrixOp[ROW,COL], f:MatrixJoinFunction)(implicit pool : EvaluationPool) extends MatrixOp[ROW,COL] {
    requireSameDimensions(a,b)
    
    override def rows = a.rows
    override def cols = a.cols
    
    override def asMatrix : Matrix = {
      /** Compares two entries based on their indeces (row then col) */
      def compare(aEntry : MatrixEntry, bEntry : MatrixEntry) : Int = {
        def cmp(a:Int,b:Int) = if (a < b) -1 else if (a > b) 1 else 0
        if (aEntry.row != bEntry.row) {
          cmp(aEntry.col, bEntry.col)
        } else {
          cmp(aEntry.row, bEntry.row)
        }
      }
      
      type E = MatrixEntry
      
      /** Wraps an iterator to make sure its entries are ordered */
      def ordered(iterator : Iterator[E]) : Iterator[E] = {
        new Iterator[E] {
          var lastRow : Int = -1
          var lastCol : Int = -1
          
          override def hasNext = iterator.hasNext
          override def next : E = {
            val entry = iterator.next
            assert(lastRow < entry.row || lastCol < entry.col,
                   "Unerlying entry iterator out of order: "+(lastRow,lastCol) + " !< " +(entry.row,entry.col))
            lastRow = entry.row
            lastCol = entry.col
            return entry
          }
        }
      }
      
      /** Returns an iterator over entries in both matrices */
      def inner : Iterator[(E,E)] = {
        return new Iterator[(E,E)] {
          val iterA = ordered(a.asMatrix.elements)
          val iterB = ordered(b.asMatrix.elements)
        
          def nextPair() : (E,E) = {
            var nextA : E = null
            var nextB : E = null
        
            while (iterA.hasNext && iterB.hasNext) {
              // increment iterators that are null
              if (nextA == null) { nextA = iterA.next }
              if (nextB == null) { nextB = iterB.next }
              val cmp = compare(nextA, nextB)
              if (cmp < 0) {
                // iterA too small, increment it next time around
                nextA = null
              } else if (cmp > 0) {
                // iterB too small, increment it next time around
                nextB = null
              } else {
                // just right - return this pair
                return (nextA,nextB)
              }
            }
            return null
          }
        
          var ready : Boolean = false
          var pending : (E,E) = null
          
          override def hasNext : Boolean = {
            if (!ready) {
              pending = nextPair()
              ready = true
            }
            return pending != null
          }
          override def next : (E,E) = {
            if (!ready && !hasNext) {
              throw new IllegalAccessError("Called next without hasNext")
            }
            ready = false
            return pending
          }
        }
      }
      
      val ma = a.asMatrix
      val mb = b.asMatrix
      val rv = pool.request(rows,cols)
      
      for (entry <- rv) {
        entry.set(0)
      }
      for (pair <- inner) {
        val row = pair._1.row
        val col = pair._1.col
        assert(pair._2.row == row && pair._2.col == col)
        rv.set(row,col,f(row,col,pair._1.get,pair._2.get))
      }
      
      if (a.intermediate) { pool.release(ma) }
      if (b.intermediate) { pool.release(mb) }
      return rv
    }
  }
  
  /** Map of outer join on two matrices (all elements, 0's and non 0's) */
  case class MatrixJoinOuterMap[ROW<:CC,COL<:CC](a : MatrixOp[ROW,COL], b : MatrixOp[ROW,COL], f:MatrixJoinFunction)(implicit pool : EvaluationPool) extends MatrixOp[ROW,COL] {
    requireSameDimensions(a,b)
    
    override def rows = a.rows
    override def cols = a.cols
    
    override def asMatrix : Matrix = {
      val ma = a.asMatrix
      val mb = b.asMatrix
      val rv = pool.request(rows,cols)
      
      for (entry <- rv) {
        entry.set(0)
      }
      for (row <- 0 until rows; col <- 0 until cols) {
        rv.set(row,col,f(row,col,ma.get(row,col),mb.get(row,col)))
      }
      
      if (a.intermediate) { pool.release(ma) }
      if (b.intermediate) { pool.release(mb) }
      return rv
    }
  }
  
  /** Some unit testing */
  def test() {
    import Scalala._
    import ScalalaValues._
    
    val n = 5
    val m = 5;
    
    val x = rand(n,m)
    val y = rand(m,n)
    
    def asserteq[E](a : E, b : E) {
      assert(a.toString == b.toString, "\nExpected "+b+";\nReceived "+a)
    }
    
    print("ScalalaOps: Symbolic tests ... ")
    
    asserteq(x+y,   MatrixPlusMatrix(MatrixIdentity(x),MatrixIdentity(y)))
    asserteq(y*2,   MatrixMultScalar(MatrixIdentity(y),ScalarIdentity(2.0),{x:Double=>x}))
    asserteq(x+y*2, MatrixPlusMatrix(MatrixIdentity(x),MatrixMultScalar(MatrixIdentity(y),ScalarIdentity(2.0),{x:Double=>x})))
    
    println("OK")
    
    print("ScalalaOps: Numeric tests ... ")
    
    val a  : Matrix = Array[Array[Int]](Array(7,0),Array(9,-4),Array(11,1),Array(1,1))
    val b  : Matrix = Array[Array[Int]](Array(1,3,-1),Array(2,3,3))
    val ex : Matrix = Array[Array[Int]](Array(7,21,-7),Array(1,15,-21),Array(13,36,-8),Array(3,6,2))
    asserteq((a  * b).asMatrix.toString, ex.toString)
    asserteq((a :* a).asMatrix.toString, (a :^ 2).asMatrix.toString)
    
    println("OK")
  }
  
  /** Main method runs some unit tests */
  def main(argv : Array[String]) {
    test()
  }
  
}

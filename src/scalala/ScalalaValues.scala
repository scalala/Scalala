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

//
// Value types for linear algebra, including matrices, vectors, scalars.
//

/**
 * A TensorValue is multi-dimensional array value.  If E is Int, then
 * the tensor is naturally considered a Vector.  If it is an (Int,Int),
 * then a Matrix.
 */
sealed trait Tensor[I,E<:TensorEntry[I]] extends PartialFunction[I,Double] {
  //
  // abstract methods: size, get, set, copy, elements, isDefinedAt, copy
  //
  
  /** Size of the tensor. */
  def size : I
  
  /** Gets the value at the given index. */
  def get(index : I) : Double
  
  /** Sets the value at the given index. */
  def set(index : I, value : Double) : Unit
  
  /** Returns a deep-copy clone of this data structure. */
  def copy : this.type
  
  /**
   * Iterates all non-zero elements in this Tensor in the order
   * stored in the underlying implementation.
   * 
   * Zero-valued elements may be returned at the descretion of the
   * underlying implementation.  The returned elements should be
   * considered temporary and may be mutated between calls to next.
   */
  def elements : Iterator[E];
  
  /** Canonical, default ordering for ordered. */
  protected def canonical : I;
    
  /**
   * Iterates all non-zero elements in this Tensor in the order
   * as specified by the relative values in each dimension of 
   * the given order, low values being major.  So for a matrix m,
   * m.ordered((1,2)) returns an iterator that iterates rows and
   * then columns within each row, whereas m.ordered((2,1)) iterates
   * columns and then rows within each column. 
   * 
   * Zero-valued elements may be returned at the descretion of the
   * underlying implementation.  The returned elements should be
   * considered temporary and may be mutated between calls to next.
   */
  def ordered(order : I) : Iterator[E];
  
  //
  // common convenience methods
  //
  
  /** Gets the value at the given index */
  @inline override def apply(index : I) = get(index);
  
  /** Selects for the given elements, returning them as a vector. */
  @inline def apply(elements : Iterator[I]) : Iterator[Double] =
    elements.map(get);
  
  /** Selects for the given elements, returning them as a vector. */
  @inline def apply(seq : Seq[I]) : Seq[Double] =
    seq.map(get);

  /** Updates the value at the given index by calling set(index,value). */
  @inline def update(index : I, value : Double) : Unit =
    set(index, value);
  
  /**
   * The returned iterator generates all indexes where the given predicate
   * is true.
   */
  def find(f : (Double => Boolean)) : Iterator[I] =
    for (e <- Tensor.this.elements if f(e.get)) yield e.index; 
  
  /** Apply a function f to all elements of this tensor. */
  def foreach(f : (E => Unit)) : Unit = {
    for (element <- elements) {
      f(element);
    }
  }
  
//  /** Apply a function f to all elements of this tensor. */
//  def foreach(f : (Double => Unit)) : Unit = {
//    for (element <- elements) { f(element.get); }
//  }

  
//  /** Tensor projection. */
//  def map(f : (Double => Double)) = new Projection(f);
//  
//  class Projection(f : (Double => Double)) extends Tensor[I,E] {
//    override def get(i : I) = f(Tensor.this.get(i));
//  }
  
  //
  // equality
  //
  
  override def equals(other : Any) : Boolean = {
    if (other.asInstanceOf[AnyRef] eq this) {
      return true;
    } else if (!other.isInstanceOf[Tensor[_,_]]) {
      return false;
    } else {
      val mo = other.asInstanceOf[Tensor[I,E]];
      if (this.size != mo.size) {
        return false;
      }
      for (pair <- (this.ordered(canonical) zip mo.ordered(canonical))) {
        if (pair._1.index != pair._2.index || pair._1.get != pair._2.get) {
          return false;
        }
      }
    }
    return true;
  }
  
  override def hashCode() : Int =
    throw new UnsupportedOperationException("Mutable: should not be used as hash key");
  
  //
  // views
  //
  
  /** To get contents as a map */
  def asMap : scala.collection.Map[I,Double] = {
    return new scala.collection.Map[I,Double] {
      override def size : Int = {
        Tensor.this.size match {
          case (i:Int)             => i;
          case (i:Int,j:Int)       => i*j;
          case (i:Int,j:Int,k:Int) => i*j*k;
          case _       => throw new ScalalaValueException("Unexpected tensor size");
        }
      }
      
      override def isDefinedAt(key : I) = Tensor.this.isDefinedAt(key);
      override def get(key : I) = Some(Tensor.this.get(key));
      
      override def elements = new Iterator[(I,Double)] {
        val iter = Tensor.this.elements;
        override def hasNext = iter.hasNext;
        
        override def next = {
          val e = iter.next
          (e.index,e.get)
        }
      }
    }
  }
  
  override def toString : String =
    elements.take(20).mkString("\n");
  
  //
  // operations
  //
  
  def *= (s : Double) {
    foreach((entry:E) => entry.set(entry.get * s));
  }
  
  def /= (s : Double) {
    foreach((entry:E) => entry.set(entry.get / s));
  }
  
  def += (s : Double) {
    foreach((entry:E) => entry.set(entry.get + s));
  }
  
  def -= (s : Double) {
    foreach((entry:E) => entry.set(entry.get - s));
  }
  
  private def ensure (t : Tensor[I,E]) {
    if (this.size != t.size) {
      throw new IllegalArgumentException("Incompatible domain sizes");
    }
  }
  
  def :=  (t : Tensor[I,E]) {
    ensure(t);
    foreach((entry:E) => entry.set(t(entry.index)));
  } 
  
  def :*= (t : Tensor[I,E]) {
    ensure(t);
    foreach((entry:E) => entry.set(entry.get * t(entry.index)));
  }
  
  def :/= (t : Tensor[I,E]) {
    ensure(t);
    foreach((entry:E) => entry.set(entry.get / t(entry.index)));
  }
  
  def += (t : Tensor[I,E]) {
    ensure(t);
    foreach((entry:E) => entry.set(entry.get + t(entry.index)));
  }
  
  def -= (t : Tensor[I,E]) {
    ensure(t);
    foreach((entry:E) => entry.set(entry.get - t(entry.index)));
  }
}

/**
 * A (mutating) entry for iterating the non-zero elements of the tensor
 */
sealed trait TensorEntry[I] {
  def index : I
  def get : Double
  def set(value : Double) : Unit
  
  override def toString : String =
    index + " " + get;
}

/** Scalars are un-indexed Tensors. */
trait Scalar extends Tensor[Unit,ScalarEntry] {}
trait ScalarEntry extends TensorEntry[Unit] {}

object Vector {
  /** Creates a vector by wrapping a double array.  Shallow copy. */
  def apply(values : Array[Double]) : Vector =
    Scalala.DenseVector(values);
  
  /** Creates a vector from a sequence of doubles.  Deep copy. */
  def apply(values : Double*) : Vector =
    Scalala.DenseVector(values.toArray);
  
  /** Creates a vector from a sequence of ints.  Deep copy. */
  def apply(values : Array[Int]) : Vector = {
    val v = Scalala.DenseVector(values.length);
    for (i <- 0 until values.length) {
      v(i) = values(i);
    }
    v;
  }
  
  /**
   * A projection mapping all entries in this Vector according
   * to the given function.  This implementation is safe to use with
   * sparse vectors.  In particular, if f(0) is 0 then only entries
   * returned by the underlying .elements are iterated in the projection's
   * .elements.  If not, all 0 entries will be returned as a single
   * mutating VectorEntry that contains the value of f(0), with its
   * index field mutated between calls to next.
   */
  class Projection(vector : Vector, f:(Double=>Double)) extends Vector {
    val at0 = f(0.0);
    
    override def size = vector.size;
    
    override def get(i : Int) : Double = {
      if (isDefinedAt(i)) {
        return f(vector.get(i));
      } else {
        return at0;
      }
    }
    
    override def set(i : Int, value : Double) =
      throw new UnsupportedOperationException();
    
    override def copy = vector.copy.map(f).asInstanceOf[this.type];
    
    override def elements : Iterator[VectorEntry] = {
      // this iterator maps all underlying elements but may miss 0's
      val mappedIgnoringZeros = 
        new Iterator[VectorEntry]() {
          var _entry : VectorEntry = null;
          val entry = new VectorEntry {
            override def index = _entry.index;
            override def get = f(_entry.get);
            override def set(x:Double) =
              throw new UnsupportedOperationException();
          }
      
          val iter = vector.elements;
        
          override def hasNext = iter.hasNext;
          override def next = {
            _entry = iter.next;
            entry;
          }
        };
      
      if (at0 == 0.0) {
        // we can use the underlying iterator because f(0)=0.
        mappedIgnoringZeros;	
      } else {
        // we need to iterate all elements because f(0) != 0
        new Iterator[VectorEntry]() {
          // which position are we
          var _index = -1;
          
          // for when we're at an underlying entry
          var _entry : VectorEntry = null;
          
          // for when we are at an entry skipped in the underlying vector
          val mappedZeroEntry : VectorEntry = new VectorEntry {
            override def index = _index;
            override def get = at0;
            override def set(x:Double) =
              throw new UnsupportedOperationException();
          }
          
          override def hasNext = _index+1 < size;
          
          override def next = {
            // update the underlying mappedIgnoringZeros if it is time to
            if ((_entry == null || _entry.index <= _index) && mappedIgnoringZeros.hasNext) {
              _entry = mappedIgnoringZeros.next;
            }
            
            // update index pointer and return the underlying entry or the zero entry
            _index += 1;
            if (_index != _entry.index) mappedZeroEntry else _entry;
          }
        }
      }
    }
  }
}

/** Vectors are Tensors indexed by a single Int. */
trait Vector extends Tensor[(Int),VectorEntry] {
  @inline override def isDefinedAt(index : Int) =
    index < size && index >= 0
  
  /**
   * A projection mapping all entries in this Vector according
   * to the given function.  This implementation is safe to use with
   * sparse vectors.  In particular, if f(0) is 0 then only entries
   * returned by the underlying .elements are iterated in the projection's
   * .elements.  If not, all 0 entries will be returned as a single
   * mutating VectorEntry that contains the value of f(0), with its
   * index field mutated between calls to next.
   */
  def map(f:(Double=>Double)) = new Vector.Projection(this,f);
  
  def canonical = 1;
  def ordered(key : Int) = elements;
  
  /** Default iterator */
  override def elements : Iterator[VectorEntry] = {
    for (i <- (0 until size).elements)
      yield new VectorEntry {
        override def index = i;
        override def get = Vector.this.get(i);
        override def set(d : Double) = Vector.this.set(i,d);
      }
  }
  
  def toArray = {
    val a = new Array[Double](size);
    for (entry <- elements) {
      a(entry.index) = entry.get;
    }
    a;
  }
}

/** An entry in a Vector */
trait VectorEntry extends TensorEntry[(Int)] {}

/** A Matrix is indexed by a pair of Ints. */
trait Matrix extends Tensor[(Int,Int),MatrixEntry] {
  //
  // pure abstract methods
  //
  
  /** Number of rows in the matrix. */
  def rows : Int
  
  /** Number of columns in the matrix. */
  def cols : Int
  
  /** Returns the value at the given row, col index. */
  def get(row : Int, col : Int) : Double

  /** Sets the given element to the given value. */
  def set(row : Int, col : Int, value : Double) : Unit;
  
  //
  // accessors
  //
  
  @inline def apply(row : Int, col : Int) = get(row, col)
  
  @inline def update(row : Int, col : Int, value : Double) =
    set(row, col, value);
  
  /** For matrix((_,1)) and matrix((0,_)) */
  def apply(select : (Int=>(Int,Int))) : Matrix = {
    import Scalala._;
    // find whether row or column
    select(-1) match {
    case (-1,-1)  => throw new IllegalArgumentException("Index of out range")
    case (-1,col) => {ColMatrix(DenseVector((0 until rows).map(row => get(row,col))))}
    case (row,-1) => {RowMatrix(DenseVector((0 until cols).map(col => get(row,col))))}
    case _        => throw new IllegalArgumentException("Invalid index selector")
    }
  }
  
  @inline protected def check(row : Int, col : Int) = {
    if (!isDefinedAt((row,col))) {
      throw new IndexOutOfBoundsException();
    }
  }
  
  //
  // Tensor[(Int,Int)] implementation
  //
  
  @inline override def size = (rows,cols);
  
  @inline override def get(index : (Int,Int)) =
    get(index._1, index._2);
  
  @inline override def set(index : (Int,Int), value : Double) =
    set(index._1, index._2, value);
  
  @inline override def isDefinedAt(index : (Int,Int)) =
    index._1 < rows && index._2 < cols && index._1 >= 0 && index._2 >= 0
  
  //
  // iterators and views -- to be overridden by subclasses for speed
  //
  
  override def elements = ordered(canonical); 
    
  override def canonical = (1,2);
  override def ordered(order : (Int,Int)) : Iterator[MatrixEntry] = {
    val indexes : Iterator[(Int,Int)] = (order match {
      case (1,2) => for (row <- 0 until rows; col <- 0 until cols) yield (row,col);
      case (2,1) => for (col <- 0 until cols; row <- 0 until rows) yield (row,col);
      case _     => throw new IllegalArgumentException("Expected order to be one of (1,2) or (2,1))");
    }).elements
    
    var _index : (Int,Int) = null;
    
    val entry = new MatrixEntry {
      override def index = _index;
      override def row = _index._1;
      override def col = _index._2;
      override def get = Matrix.this.get(_index._1,_index._2);
      override def set(value:Double) = Matrix.this.set(index._1,_index._2,value);
    }
    
    return new Iterator[MatrixEntry] {
      override def hasNext = indexes.hasNext;
      override def next = {
        _index = indexes.next;
        entry;
      }
    }
  }
  
  override def toString : String = {
    if (cols <= 10) {
      val r = for (i <- 0 until rows)
        yield ((0 until cols).map(j => String.format("%8.4f",double2Double(get(i,j)))).mkString(" "));
      r.mkString("\n")
    } else {
      super.toString
    }
  }
  
  /** Returns a vector view of the given row. */
  def row(i : Int) = new Vector {
    override def size = cols;
    override def get(j : Int) = Matrix.this.get(i, j);
    override def set(j : Int, value : Double) = Matrix.this.set(i, j, value);
    override def copy = Vector(this.toArray).asInstanceOf[this.type];
  };
  
  /** Returns a vector view of the given column. */
  def col(j : Int) = new Vector {
    override def size = rows;
    override def get(i : Int) = Matrix.this.get(i, j);
    override def set(i : Int, value : Double) = Matrix.this.set(i, j, value);
    override def copy = Vector(this.toArray).asInstanceOf[this.type];
  }
}

/** An entry in a matrix */
trait MatrixEntry extends TensorEntry[(Int,Int)]{
  def row : Int
  def col : Int
  override def index : (Int,Int) = (row,col)
}

/** An exception thrown for an invalid value */
class ScalalaValueException(message : String) extends RuntimeException(message);

trait ScalalaValuesTest extends library.Library with library.Implicits {
  import ScalalaTest._;
  
  def matrix_ordered_test() {
    val m : Matrix = Array[Array[Int]](Array(1,2,3),Array(4,5,6),Array(7,8,9));
    assertEquals(m.ordered((1,2)).map(_.get).toList, List(1,2,3,4,5,6,7,8,9));
    assertEquals(m.ordered((2,1)).map(_.get).toList, List(1,4,7,2,5,8,3,6,9));
  }
  
  def matrix_find_test() {
    val m : Matrix = Array[Array[Int]](Array(1,2,3),Array(4,5,6),Array(7,8,9));
    assertEquals(m.find(_%2==0).toList, List((1,0),(0,1),(2,1),(1,2)));
    assertEquals(m(m.find(_%2==0)).toList, List(4,2,8,6));
  }
  
  def vector_find_test() {
    val v = Vector(2,1,8,6);
    assertEquals(v.find(_%2==0).toList, List(0,2,3));
    assertEquals(v(v.find(_%2==0)).toList, List(2,8,6));
  }
  
  def matrix_apply_test() {
    val m : Matrix = Array[Array[Double]](Array(1,2,3),Array(4,5,6));
    assertEquals(m((_,0)),ColMatrix(List(1,4)));
    assertEquals(m((_,1)),ColMatrix(List(2,5)));
    assertEquals(m((_,2)),ColMatrix(List(3,6)));
    assertEquals(m((0,_)),RowMatrix(List(1,2,3)));
    assertEquals(m((1,_)),RowMatrix(List(4,5,6)));
  }
}


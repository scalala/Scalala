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
package scalala.tensor;

import scalala.collection.{DomainException, PartialMap, MutablePartialMap}
import scalala.collection.{MergeableSet, IntSpanSet, ProductSet};

import scalala.tensor.operators._;
import TensorShapes._;

/**
 * Implicit type promotions from arrays and sequences.
 * 
 * @author dramage
 */
trait TensorImplicits {
  import dense.DenseVector;
  
  implicit def iSeqToDoublePartialMap[T](seq : Seq[T])(implicit convert : T=>Double) = {
    new PartialMap[Int,Double] {
      override def default = 0.0;
      override lazy val domain : MergeableSet[Int] = IntSpanSet(0, seq.length);
      override def activeDomain = domain;
      override def apply(i : Int) = convert(seq(i));
    };
  }
  
  implicit def iArrayToVector(x : Array[Double]) =
    new dense.DenseVector(x);
  
  implicit def iArrayToPartialMap(x : Array[Float]) = {
    new PartialMap[Int,Double] {
      override def default = 0;
      override lazy val domain : MergeableSet[Int] = IntSpanSet(0, x.length);
      override def activeDomain = domain;
      override def apply(i : Int) = x(i);
    }
  }
  
  implicit def iArrayToPartialMap(x : Array[Int]) = {
    new PartialMap[Int,Double] {
      override def default = 0;
      override lazy val domain : MergeableSet[Int] = IntSpanSet(0, x.length);
      override def activeDomain = domain;
      override def apply(i : Int) = x(i);
    }
  }
  
  implicit def iArrayToPartialMap(x : Array[Long]) = {
    new PartialMap[Int,Double] {
      override def default = 0;
      override lazy val domain : MergeableSet[Int] = IntSpanSet(0, x.length);
      override def activeDomain = domain;
      override def apply(i : Int) = x(i);
    }
  }
  
  implicit def iArrayToPartialMap(x : Array[Short]) = {
    new PartialMap[Int,Double] {
      override def default = 0;
      override lazy val domain : MergeableSet[Int] = IntSpanSet(0, x.length);
      override def activeDomain = domain;
      override def apply(i : Int) = x(i);
    }
  }
  
  implicit def iArrayToPartialMap(x : Array[Byte]) = {
    new PartialMap[Int,Double] {
      override def default = 0;
      override lazy val domain : MergeableSet[Int] = IntSpanSet(0, x.length);
      override def activeDomain = domain;
      override def apply(i : Int) = x(i);
    }
  }
}

object TensorImplicits extends TensorImplicits { }

/**
 * A double-valued mutable partial map with support for assignment operators.
 * See {@link Tensor1} and {@link Tensor2} for tensors defined over a domain of 1 key
 * (e.g. {@link Vector}) and 2 key (e.g. {@link Matrix}), respectively.
 * 
 * @author dramage
 */
trait Tensor[I] extends MutablePartialMap[I,Double] with TensorSelfOp[I,Tensor[I],AnyShape] {

  private var _default = 0.0;
  
  /** Returns the default value for this tensor. */
  override def default = _default;
  
  /** Sets the default value for this tensor. */
  override def default_=(update : Double) = _default = update;
  
  /** Set all values to zero. */
  def zero() : Unit = {
    default = 0;
    this(activeKeys) = 0;
  }
  
  /** Requires that this domain and that domain are equal.*/
  protected def ensure (that : PartialMap[I,Double]) {
    if (this.domain != that.domain) {
      throw new DomainException("Incompatible domain sizes");
    }
  }
  
  //
  // Scalar updates.
  //
  
  /** Multiplies each element by the given scale factor. */
  def *= (s : Double) = {
    if (s == 1) {
      // do nothing
    } else {
      this.default = this.default * s;
      for (i <- activeDomain) { this(i) *= s; }
    }
  }
  
  /** Divides each element by the given scale factor. */
  def /= (s : Double) = {
    if (s == 1) {
      // do nothing
    } else {
      this.default = this.default / s;
      this(activeDomain) = ((x:Double) => x / s);
    }
  }
  
  /** Increments element by the given scalar. */
  def += (s : Double) = {
    this.default = this.default + s;
    this(activeDomain) = ((x:Double) => x + s);
  }
  
  /** Decrements each element by the given scalar. */
  def -= (s : Double) = {
    this.default = this.default - s;
    this(activeDomain) = ((x:Double) => x - s);
  }
  
  /** Raises each element to the the given power. */
  def :^= (s : Double) = {
    this.default = Math.pow(this.default, s);
    this(activeDomain) = ((x:Double) => Math.pow(x,s));
  }
  
  /** Each element becomes itself modulo the given scalar. */
  def %= (s : Double) = {
    this.default %= s;
    this(activeDomain) = ((x:Double) => x % s);
  }
  
  //
  // Assignments and updates from another PartialMap.
  //
  
  /** Assigns each element in this map to the corresponding value in the other map. */
  def :=  (t : PartialMap[I,Double]) {
    ensure(t);
    this.default = t.default;
    this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => t(i));
  }
  
  /** Multiplies each value in this map by the corresponding value in the other map. */
  def :*= (t : PartialMap[I,Double]) {
    ensure(t);
    // if our default is 0 then we can skip active iterator in the other domain
    val domain = {
      if (this.default == 0.0) (this.activeDomain)
      else                     (this.activeDomain ++ t.activeDomain)
    }
    this.default = this.default * t.default;
    this(domain) = ((i : I, x : Double) => x*t(i));
  }

  /** Divides each value in this map by the corresponding value in the other map. */
  def :/= (t : PartialMap[I,Double]) {
    ensure(t);
    this.default = this.default / t.default;
    this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x/t(i));
  }
  
  /** Increments each value in this map by the corresponding value in the other map. */
  def :+= (t : PartialMap[I,Double]) {
    ensure(t);
    if (t.default == 0) {
      for (i <- t.activeDomain) {
        this(i) += t(i);
      }
    } else {
      this.default += t.default;
      this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x+t(i));
    }
  }
  
  /** Decrements each value in this map by the corresponding value in the other map. */
  def :-= (t : PartialMap[I,Double]) {
    ensure(t);
    if (t.default == 0) {
      for (i <- t.activeDomain) {
        this(i) -= t(i);
      }
    } else {
      this.default -= t.default;
      this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x-t(i));
    }
  }

  /** Raises each value in this map by the corresponding value in the other map. */
  def :^= (t : PartialMap[I,Double]) {
    ensure(t);
    this.default = Math.pow(this.default, t.default);
    this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => Math.pow(x,t(i)));
  }
  
  /** Modulos each value in this map by the corresponding value in the other map. */
  def :%= (t : PartialMap[I,Double]) {
    ensure(t);
    this.default = this.default % t.default;
    this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x%t(i));
  }
  
  /** += with another PartialMap is a fixed alias for :+= */
  final def += (t : PartialMap[I,Double]) = this.:+=(t);
  
  /** -= with another PartialMap is a fixed alias for :-= */
  final def -= (t : PartialMap[I,Double]) = this.:-=(t);
  
  //
  // Assignments and updates from an op
  //

//  /** Assigns each element in this map to the corresponding value as returned by the given operation. */
//  def := [V<:Base] (op : TensorOp[I,Base,V,Shape]) : Unit = {
//    op match {
//      case op : TensorOp[_,_,_,_] => this := op.value;
//    }
//  }
  
  /** Returns the partial map representing this tensor op skipping negations. */
  private def getPartialMap[V<:Tensor[I],S<:TensorShape] (op : TensorOp[V,S]) : PartialMap[I,Double] = {
    op match {
      case TensorNegation(n) => n.value.map( (x:Double) => -x);
      case _ => op.value.asInstanceOf[PartialMap[I,Double]];
    }
  }
  
  /** Increments each element in this map by the corresponding value as returned by the given operation. */
  def :+= [V<:Tensor[I]] (op : TensorOp[V,_]) : Unit = {
    op match {
      case TensorMultScalar(m, s) if (s != 0) => {
        val t = getPartialMap(m);
        if (t.default == 0) {
          for (i <- t.activeDomain) {
            this(i) += t(i) * s;
          }
        } else {
          this.default += t.default * s;
          this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x + (t(i) * s));
        }
      }
      case ScalarDivTensor(s, m) => {
        val t = getPartialMap(m);
        this.default += (s / t.default);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x + (s / t(i)));
      }
      case TensorPlusScalar(m, s) => {
        val t = getPartialMap(m);
        this.default += (t.default + s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x + (t(i) + s));
      }
      case _ => this :+= op.value.asInstanceOf[PartialMap[I,Double]];
    }
  }
  
  /** += is a fixed alias for :+=  */
  final def += [V<:Tensor[I]]  (op : TensorOp[V,_]) = this.:+=(op);

  /** Make Scala happy about inheritance */
  final def :+= [V<:Tensor[I]]  (op : V with TensorOp[V,_]) { this.:+=(op:PartialMap[I,Double]); }

  /** Make Scala happy about inheritance */
  final def += [V<:Tensor[I]]  (op : V with TensorOp[V,_]) { this.:+=(op:PartialMap[I,Double]); }

  /** Decrements each element in this map by the corresponding value as returned by the given operation. */
  def :-= [V<:Tensor[I]] (op : TensorOp[V,_]) : Unit = {
    op match {
      case TensorMultScalar(m, s) if (s != 0) => {
        val t = getPartialMap(m);
        if (t.default == 0) {
          for (i <- t.activeDomain) {
            this(i) -= t(i) * s;
          }
        } else {
          this.default -= t.default * s;
          this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x - (t(i) * s));
        }
      }
      case ScalarDivTensor(s, m) => {
        val t = getPartialMap(m);
        this.default -= (s / t.default);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x - (s / t(i)));
      }
      case TensorPlusScalar(m, s) => {
        val t = getPartialMap(m);
        this.default -= (t.default + s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x - (t(i) + s));
      }
      case _ => this :-= op.value.asInstanceOf[PartialMap[I,Double]];
    }
  }
  
  /** -= is a fixed alias for :-= */
  final def -= [V<:Tensor[I]] (op : TensorOp[V,_]) = this.:-=(op);


  /** Make Scala happy about inheritance */
  final def :-= [V<:Tensor[I]]  (op : V with TensorOp[V,_]) { this.:-=(op:PartialMap[I,Double]); }

  /** Make Scala happy about inheritance */
  final def -= [V<:Tensor[I]]  (op : V with TensorOp[V,_]) { this.:-=(op:PartialMap[I,Double]); }

  
  /** Multiplies each element in this map by the corresponding value as returned by the given operation. */
  def :*= [V<:Tensor[I],S<:TensorShapes.PublicShape] (op : TensorOp[V,S]) : Unit = {
    op match {
      case TensorMultScalar(m, s) => {
        val t = getPartialMap(m);
        this.default *= (t.default * s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x * (t(i) * s));
      }
      case ScalarDivTensor(s, m) => {
        val t = getPartialMap(m);
        this.default *= (s / t.default);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x * (s / t(i)));
      }
      case TensorPlusScalar(m, s) => {
        val t = getPartialMap(m);
        this.default *= (t.default + s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x * (t(i) + s));
      }
      case _ => this :*= op.value;
    }
  }

  /** Make Scala happy about inheritance */
  final def :*= [V<:Tensor[I]]  (op : V with TensorOp[V,_]) { this.:*=(op:PartialMap[I,Double]); }

  /** Divides each element in this map by the corresponding value as returned by the given operation. */
  def :/= [V<:Tensor[I],S<:TensorShapes.PublicShape] (op : TensorOp[V,S]) : Unit = {
    op match {
      case TensorMultScalar(m, s) => {
        val t = getPartialMap(m);
        this.default /= (t.default * s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x / (t(i) * s));
      }
      case ScalarDivTensor(s, m) => {
        val t = getPartialMap(m);
        this.default /= (s / t.default);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x / (s / t(i)));
      }
      case TensorPlusScalar(m, s) => {
        val t = getPartialMap(m);
        this.default /= (t.default + s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x / (t(i) + s));
      }
      case _ => this :/= op.value;
    }
  }

  /** Make Scala happy about inheritance */
  final def :/= [V<:Tensor[I]]  (op : V with TensorOp[V,_]) { this.:/=(op:PartialMap[I,Double]); }

  /** Raises each element in this map to the power (in the corresponding value) in the value returned by the given operation. */
  def :^= [V<:Tensor[I],S<:PublicShape] (op : TensorOp[V,S]) : Unit =
    this :^= op.value.asInstanceOf[PartialMap[I,Double]];
  
  /** Make Scala happy about inheritance */
  final def :^= [V<:Tensor[I]]  (op : V with TensorOp[V,_]) { this.:^=(op:PartialMap[I,Double]); }

  /** Approximate equality at a given tolerance level. */
  def =~= (tolerance : Double)(that : Tensor[I]) : Boolean = {
    (this eq that) ||
    (
     (that canEqual this) &&
     (this.domain == that.domain) &&
     { val joint = (this join that)((a,b) => Math.abs(a - b) < tolerance);
       ((joint.default == true || joint.activeDomain.size == joint.domain.size)
       && !joint.activeValues.contains(false));
     }
    );
  }

  /** Approximate equality using the default Tensor.TOLERANCE value. */
  def =~= (that : Tensor[I]) : Boolean =
    this.=~=(Tensor.TOLERANCE)(that);
  
  /** Returns !(this.=~=(tolerance)(that)) */
  def !~= (tolerance : Double)(that : Tensor[I]) : Boolean =
    ! this.=~=(tolerance)(that);
  
  /** Returns !(this.=~=(that)) */
  def !~= (that : Tensor[I]) : Boolean =
    ! this.=~=(that);
}

object Tensor {
  class CreateException(msg : String) extends RuntimeException(msg);
  
  val TOLERANCE = 1e-8;
}

/** A one-axis tensor is defined on single iterator from a domain. */
trait Tensor1[I] extends Tensor[I] with TensorSelfOp[I,Tensor1[I],Shape1Col] {

  /** Returns the inner product of this tensor with another. */
  def dot(that : Tensor1[I]) : Double = {
    ensure(that);
    
    if (this.default == 0.0 || that.default == 0.0) {
      var sum = 0.0;
      for (k <- this.activeDomain ** that.activeDomain) {
        sum += (this(k) * that(k));
      }
      sum;
    } else {
      var sum = 0.0;
      for (value <- this.join(that)(_ * _).valuesIterator) {
        sum += value;
      }
      sum;
    }
  }


  def +=(t: Tensor1[I]) { super.+=(t:PartialMap[(I),Double]) }
  def -=(t: Tensor1[I]) { super.-=(t:PartialMap[(I),Double]) }
  def :+=(t: Tensor1[I]) { super.:+=(t:PartialMap[(I),Double]) }
  def :-=(t: Tensor1[I]) { super.:-=(t:PartialMap[(I),Double]) }
  def :*=(t: Tensor1[I]) { super.:*=(t:PartialMap[(I),Double]) }
  def :/=(t: Tensor1[I]) { super.:/=(t:PartialMap[(I),Double]) }
  def :^=(t: Tensor1[I]) { super.:^=(t:PartialMap[(I),Double]) }

}

object Tensor1 {
  import scalala.tensor.operators._;
}

/** A two-axes tensor is defined on pairs of iterator from two domains. */
trait Tensor2[I1,I2] extends Tensor[(I1,I2)] with TensorSelfOp[(I1,I2),Tensor2[I1,I2],Shape2] {
  /** Gets the value indexed by (i,j). */
  def apply(i : I1, j : I2) : Double;
  
  /** Updates the value indexed by (i,j). */
  def update(i : I1, j : I2, value : Double) : Unit;
  
  /** Returns the domain of this tensor, typed as a Domain2. */
  override def domain : ProductSet[I1,I2];
  
  /** Fixed alias for apply(i,j). */
  @inline final override def apply(pos : (I1,I2)) = apply(pos._1, pos._2);
  
  /** Fixed alias for update(i,j,value). */
  @inline final override def update(pos : (I1,I2), value : Double) : Unit = update(pos._1, pos._2, value);
  
  /** Returns a transposed view of this matrix. */
  def transpose : Tensor2[I2,I1] = new {
    val inner = Tensor2.this;
  } with Tensor2.Transpose[I1,I2] {
    def like : Tensor2[I2,I1] = inner.like.transpose;
  }
  
  /** Selects a row from this Tensor2 as a Tensor1. */
  def getRow(row : I1) : Tensor1[I2]
  
  /** Selects a column from this Tensor2 as a Tensor1. */
  def getCol(col : I2) : Tensor1[I1]
  
  /** Gets the active domain in the given row. */
  def activeDomainInRow(row : I1) : MergeableSet[I2] = {
    new MergeableSet[I2] {
      override def iterator = Tensor2.this.activeDomain.iterator.filter(_._1 == row).map(_._2);
      override def contains(col : I2) = Tensor2.this.activeDomain.contains((row,col));
    };
  }
  
  /** Gets the active domain in the given column. */
  def activeDomainInCol(col : I2) : MergeableSet[I1] = {
    new MergeableSet[I1] {
      override def iterator = Tensor2.this.activeDomain.iterator.filter(_._2 == col).map(_._1);
      override def contains(row : I1) = Tensor2.this.activeDomain.contains((row,col));
    };
  }

  def +=(t: Tensor2[I1,I2]) { super.+=(t:PartialMap[(I1,I2),Double]) }
  def -=(t: Tensor2[I1,I2]) { super.-=(t:PartialMap[(I1,I2),Double]) }
  def :+=(t: Tensor2[I1,I2]) { super.:+=(t:PartialMap[(I1,I2),Double]) }
  def :-=(t: Tensor2[I1,I2]) { super.:-=(t:PartialMap[(I1,I2),Double]) }
  def :*=(t: Tensor2[I1,I2]) { super.:*=(t:PartialMap[(I1,I2),Double]) }
  def :/=(t: Tensor2[I1,I2]) { super.:/=(t:PartialMap[(I1,I2),Double]) }
  def :^=(t: Tensor2[I1,I2]) { super.:^=(t:PartialMap[(I1,I2),Double]) }

}

/**
 * Companion object to Tensor2.
 * 
 * @author dramage
 */
object Tensor2 {
  trait Projection[I1,I2] extends Tensor2[I1,I2] {
    val inner : Tensor2[I1,I2];
    
    override def domain =
      inner.domain;
    
    override def apply(row : I1, col : I2) =
      inner.apply(row,col);
    
    override def update(row : I1, col : I2, value : Double) =
      inner.update(row,col,value);
    
    override def activeDomain : MergeableSet[(I1,I2)] =
      inner.activeDomain;
    
    override def transpose =
      inner.transpose
    
    override def getRow(row : I1) =
      inner.getRow(row);
    
    override def getCol(col : I2) =
      inner.getCol(col);
    
    override def activeDomainInRow(row : I1) =
      inner.activeDomainInRow(row);
    
    override def activeDomainInCol(col : I2) =
      inner.activeDomainInCol(col);
  }
  
  trait Transpose[I1,I2] extends Tensor2[I2,I1] {
    val inner : Tensor2[I1,I2];
    
    override def domain =
      inner.domain.transpose;
    
    override def apply(row : I2, col : I1) =
      inner.apply(col, row);
      
    override def update(row : I2, col : I1, value : Double) =
      inner.update(col, row, value);
    
    override def copy =
      inner.copy.asInstanceOf[Tensor2[I1,I2]].transpose;
    
    override def activeDomain : MergeableSet[(I2,I1)] = {
      new MergeableSet[(I2,I1)] {
        override def size = inner.activeDomain.size;
        override def iterator = inner.activeDomain.iterator.map(tup => (tup._2,tup._1));
        override def contains(i : (I2,I1)) = inner.activeDomain.contains((i._2,i._1));
      };
    }
    
    override def transpose = inner;
    
    override def getRow(col : I2) =
      inner.getCol(col);
    
    override def getCol(row : I1) =
      inner.getRow(row);
    
    override def activeDomainInRow(col : I2) =
      inner.activeDomainInCol(col);
    
    override def activeDomainInCol(row : I1) =
      inner.activeDomainInRow(row);
  }
}

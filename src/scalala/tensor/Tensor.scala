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

import scalala.collection.domain.{Domain, Domain1, Domain2, DomainException};
import scalala.collection.{PartialMap, MutablePartialMap, MergeableSet, IntSpanSet};
import scalala.tensor.operators._;

/**
 * Implicit type promotions from arrays.
 * @author dramage
 */
trait TensorImplicits {
  import dense.DenseVector;
  import scalala.collection.domain.IntSpanDomain;
  import spans.IntSpans._;
  
  implicit def iSeqToDoublePartialMap[T<:AnyVal](seq : Seq[T])(implicit manifest : scala.reflect.Manifest[T]) : PartialMap[Int,Double] = {
    val convert : (T=>Double) =
        (if      (manifest.erasure == classOf[Byte])    ((x:Byte)    => x.asInstanceOf[Double]);
         else if (manifest.erasure == classOf[Short])   ((x:Short)   => x.asInstanceOf[Double]);
         else if (manifest.erasure == classOf[Int])     ((x:Int)     => x.asInstanceOf[Double]);
         else if (manifest.erasure == classOf[Long])    ((x:Long)    => x.asInstanceOf[Double]);
         else if (manifest.erasure == classOf[Float])   ((x:Float)   => x.asInstanceOf[Double]);
         else if (manifest.erasure == classOf[Double])  ((x:Double)  => x.asInstanceOf[Double]);
         else if (manifest.erasure == classOf[Boolean]) ((x:Boolean) => x.asInstanceOf[Double]);
         else if (manifest.erasure == classOf[Char])    ((x:Char)    => x.asInstanceOf[Double]);
         else throw new IllegalArgumentException("Unexpected value type")
       ).asInstanceOf[T=>Double];
    
    return new PartialMap[Int,Double] {
      override def default = 0.0;
      override def domain : Domain[Int] = IntSpanDomain(0, seq.length);
      override def activeDomain : MergeableSet[Int] = IntSpanSet(0,seq.length);
      override def apply(i : Int) = convert(seq(i));
    };
  }
  
  implicit def iArrayToVector(x : Array[Double]) =
    new dense.DenseVector(x);

  implicit def iArrayToPartialMap(x : Array[Float]) = {
    new PartialMap[Int,Double] {
      override def default = 0;
      override def domain = IntSpanDomain(0, x.size);
      override def activeDomain : MergeableSet[Int] = IntSpanSet(0, x.length);
      override def apply(i : Int) = x(i);
    }
  }
  
  implicit def iArrayToPartialMap(x : Array[Int]) = {
    new PartialMap[Int,Double] {
      override def default = 0;
      override def domain = IntSpanDomain(0, x.size);
      override def activeDomain : MergeableSet[Int] = IntSpanSet(0, x.length);
      override def apply(i : Int) = x(i);
    }
  }
  
  implicit def iArrayToPartialMap(x : Array[Long]) = {
    new PartialMap[Int,Double] {
      override def default = 0;
      override def domain = IntSpanDomain(0, x.size);
      override def activeDomain : MergeableSet[Int] = IntSpanSet(0, x.length);
      override def apply(i : Int) = x(i);
    }
  }
  
  implicit def iArrayToPartialMap(x : Array[Short]) = {
    new PartialMap[Int,Double] {
      override def default = 0;
      override def domain = IntSpanDomain(0, x.size);
      override def activeDomain : MergeableSet[Int] = IntSpanSet(0, x.length);
      override def apply(i : Int) = x(i);
    }
  }
  
  implicit def iArrayToPartialMap(x : Array[Byte]) = {
    new PartialMap[Int,Double] {
      override def default = 0;
      override def domain = IntSpanDomain(0, x.size);
      override def activeDomain : MergeableSet[Int] = IntSpanSet(0, x.length);
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
trait Tensor[I] extends MutablePartialMap[I,Double] {
  /** Returns a new map of the same type for the given domain. */
  def create[J](domain : Domain[J]) : Tensor[J];
  
  /** Returns a deep copy of this data structure. */
  def copy : Tensor[I] = {
    val rv = create(domain);
    rv := this;
    rv;
  }
  
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
      this(activeDomain) = ((x:Double) => x * s);
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
  def ^= (s : Double) = {
    this.default = Math.pow(this.default, s);
    this(activeDomain) = ((x:Double) => Math.pow(x,s));
  }
  
  /** Each element becomes itself modulo the given scala. */
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
    // if our default is 0 then we can skip active elements in the other domain
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
    this.default = this.default + t.default;
    this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x+t(i));
  }
  
  /** Decrements each value in this map by the corresponding value in the other map. */
  def :-= (t : PartialMap[I,Double]) {
    ensure(t);
    this.default = this.default - t.default;
    this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x-t(i));
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
  // Assignments and updates from a NumericPartialMapOp
  //
    
  /** Assigns each element in this map to the corresponding value as returned by the given operation. */
  def :=[T<:Tensor[I]]  (op : TensorOp[I,T]) : Unit = {
    op match {
      case op : TensorOp[_,_] => this := op.value;
    }
  }
  
  /** Increments each element in this map by the corresponding value as returned by the given operation. */
  def :+=[T<:Tensor[I]] (op : TensorOp[I,T]) : Unit = {
    op match {
      case TensorMultScalar(tOp, s) => {
        val t = tOp.value;
        this.default += (t.default * s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x + (t(i) * s));
      }
      case TensorPlusScalar(tOp, s) => {
        val t = tOp.value;
        this.default += (t.default + s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x + (t(i) + s));
      }
      case op : TensorOp[_,_] => this :+= op.value;
    }
  }
  
  /** Decrements each element in this map by the corresponding value as returned by the given operation. */
  def :-=[T<:Tensor[I]] (op : TensorOp[I,T]) : Unit = {
    op match {
      case TensorMultScalar(tOp, s) => {
        val t = tOp.value;
        this.default -= (t.default * s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x - (t(i) * s));
      }
      case TensorPlusScalar(tOp, s) => {
        val t = tOp.value;
        this.default -= (t.default + s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x - (t(i) + s));
      }
      case op : TensorOp[_,_] => this :-= op.value;
    }
  }
  
  /** Multiplies each element in this map by the corresponding value as returned by the given operation. */
  def :*=[T<:Tensor[I]] (op : TensorOp[I,T]) : Unit = {
    op match {
      case TensorMultScalar(tOp, s) => {
        val t = tOp.value;
        this.default *= (t.default * s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x * (t(i) * s));
      }
      case TensorPlusScalar(tOp, s) => {
        val t = tOp.value;
        this.default *= (t.default + s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x * (t(i) + s));
      }
      case op : TensorOp[_,_] => this :*= op.value;
    }
  }
  
  /** Divides each element in this map by the corresponding value as returned by the given operation. */
  def :/=[T<:Tensor[I]] (op : TensorOp[I,T]) : Unit = {
    op match {
      case TensorMultScalar(tOp, s) => {
        val t = tOp.value;
        this.default /= (t.default * s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x / (t(i) * s));
      }
      case TensorPlusScalar(tOp, s) => {
        val t = tOp.value;
        this.default /= (t.default + s);
        this(this.activeDomain ++ t.activeDomain) = ((i : I, x : Double) => x / (t(i) + s));
      }
      case op : TensorOp[_,_] => this :/= op.value;
    }
  }
  
  /** += with a TensorOp is a fixed alias for :+= */
  final def +=[T<:Tensor[I]] (op : TensorOp[I,T]) : Unit = this.:+=(op);
  
  /** -= with a TensorOp is a fixed alias for :-= */
  final def -=[T<:Tensor[I]] (op : TensorOp[I,T]) : Unit = this.:-=(op);
  
  
  /*
  def := (op : VectorOp) : Unit = {
    op match {
      case VectorMultScalar(v, s) => {
        msg("custom := "+op);
        if (s == 0) { zero(); } else { this := v; this *= s.value; }
      }
      
      case VectorIdentity(v) if v eq this => {
        msg("custom := "+op);
        // do nothing: self assignment
      }
      
      case op : VectorOp => {
        msg("default := "+op);
        this := op.value;
      }
    }
  }
  
  def += (op : VectorOp) : Unit = {
    op match {
      case VectorMultScalar(v,s) => {
        msg("custom += "+op);
        if (s.value != 0) { this += op.value; }
      }
      
      case VectorIdentity(v) if v eq this => {
        msg("custom += "+op);
        // adding self: double
        this *= 2;
      }
      
      case op : VectorOp => {
        msg("default += "+op);
        this += op.value;
      }
    }
  }
  
  def -= (op : VectorOp) : Unit = {
    op match {
      case VectorMultScalar(v,s) => {
        msg("custom -= "+op);
        if (s != 0) { this += op.value; }
      }
      
      case VectorIdentity(v) if v eq this => {
        msg("custom -= "+op);
        // subtracting self: zero
        this.zero();
      }
      
      case op : VectorOp => {
        msg("default -= "+op);
        this -= op.value;
      }
    }
  }
  
  def :*= (op : VectorOp) : Unit = {
    op match {
      case op : VectorOp => {
        msg("default *= "+op);
        this :*= op.value;
      }
    }
  }
  
  def :/= (op : VectorOp) : Unit = {
    op match {
      case op : VectorOp => {
        msg("default /= "+op);
        this :/= op.value;
      }
    }
  }
  */
  
}

object Tensor {
  class CreateException(msg : String) extends RuntimeException(msg);
}

/** A one-axis tensor is defined on single elements from a domain. */
trait Tensor1[I] extends Tensor[I] {
  /** Fixed alias for domain1. */
  final override def domain = domain1;
  
  /** Returns the domain of this tensor, typed as a Domain1. */
  def domain1 : Domain1[I];
  
  /** Returns the inner product of this tensor with another. */
  def dot(that : Tensor1[I]) : Double = {
    if (that.domain != this.domain)
      throw new DomainException("Domains do not match");
    
    if (this.default == 0.0 || that.default == 0.0) {
      var sum = 0.0;
      for (k <- this.activeDomain ** that.activeDomain) {
        sum += (this(k) * that(k));
      }
      return sum;
    } else {
      var sum = 0.0;
      for (value <- this.join(that)(_ * _).values) {
        sum += value;
      }
      return sum;
    }
  }
}

/** A two-axes tensor is defined on pairs of elements from two domains. */
trait Tensor2[I1,I2] extends Tensor[(I1,I2)] {
  /** Gets the value indexed by (i,j). */
  def apply(i : I1, j : I2) : Double;
  
  /** Updates the value indexed by (i,j). */
  def update(i : I1, j : I2, value : Double) : Unit;
  
  /** Returns the domain of this tensor, typed as a Domain2. */
  def domain2 : Domain2[I1,I2];
  
  /** Fixed alias for domain2. */
  @inline final override def domain = domain2;
  
  /** Fixed alias for apply(i,j). */
  @inline final override def apply(pos : (I1,I2)) = apply(pos._1, pos._2);
  
  /** Fixed alias for update(i,j,value). */
  @inline final override def update(pos : (I1,I2), value : Double) : Unit = update(pos._1, pos._2, value);
  
  def transpose : Tensor2[I2,I1] = {
    val inner = this;
    new Tensor2[I2,I1] {
      override def domain2 =
        inner.domain2.transpose;
    
      override def apply(row : I2, col : I1) =
        inner.apply(col, row);
      
      override def update(row : I2, col : I1, value : Double) =
        inner.update(col, row, value);
    
      override def copy =
        inner.copy.asInstanceOf[Tensor2[I1,I2]].transpose;
    
      override def create[J](domain : Domain[J]) =
        inner.create(domain);
      
      override def activeDomain : MergeableSet[(I2,I1)] = {
        new MergeableSet[(I2,I1)] {
          override def size = Tensor2.this.activeDomain.size;
          override def elements = Tensor2.this.activeDomain.elements.map(tup => (tup._2,tup._1));
          override def contains(i : (I2,I1)) = Tensor2.this.activeDomain.contains((i._2,i._1));
        };
      }
      
      override def getRow(row : I2) = inner.getCol(row);
      
      override def getCol(col : I1) = inner.getRow(col);
    }
  }
  
  /** Selects a row from this Tensor2 as a Tensor1. */
  def getRow(row : I1) : Tensor1[I2]
  
  /** Selects a column from this Tensor2 as a Tensor1. */
  def getCol(col : I2) : Tensor1[I1]
  
  /** Gets the active domain in the given row. */
  def activeDomainInRow(row : I1) : MergeableSet[I2] = {
    new MergeableSet[I2] {
      override def elements = Tensor2.this.activeDomain.elements.filter(_._1 == row).map(_._2);
      override def contains(col : I2) = Tensor2.this.activeDomain.contains((row,col));
    };
  }
  
  /** Gets the active domain in the given column. */
  def activeDomainInCol(col : I2) : MergeableSet[I1] = {
    new MergeableSet[I1] {
      override def elements = Tensor2.this.activeDomain.elements.filter(_._2 == col).map(_._1);
      override def contains(row : I1) = Tensor2.this.activeDomain.contains((row,col));
    };
  }
}

/**
 * Companion object to Tensor2.
 * 
 * @author dramage
 */
object Tensor2 {
  abstract class Projection[I1,I2](protected val inner : Tensor2[I1,I2]) extends Tensor2[I1,I2] {
    override def domain2 =
      inner.domain2;
    
    override def apply(row : I1, col : I2) =
      inner.apply(row,col);
    
    override def update(row : I1, col : I2, value : Double) =
      inner.update(row,col,value);
    
    override def create[J](domain : Domain[J]) : Tensor[J] =
      inner.create(domain);
    
    override def activeDomain : MergeableSet[(I1,I2)] =
      inner.activeDomain;
    
    override def getRow(row : I1) = inner.getRow(row);
    
    override def getCol(col : I2) = inner.getCol(col);
    
    override def activeDomainInRow(row : I1) = inner.activeDomainInRow(row);
    
    override def activeDomainInCol(col : I2) = inner.activeDomainInCol(col);
  }
  
  /** The given Tensor1 as a single column in a Tensor2. */
  /*
  class Column[I1,I2](tensor : Tensor1[I1], newDomain : Domain1[I2], newValue : I2) extends Tensor2[I1,I2] {
    override def domain2 =
      Domain2(tensor.domain1, newDomain);
    
    override def apply(row : I1, col : I2) = {
      if (col != newValue) throw new IndexOutOfBoundsException;
      tensor.apply(row);
    }
    
    override def update(row : I1, col : I2, value : Double) = {
      if (col != newValue) throw new IndexOutOfBoundsException;
      tensor.update(row,value);
    }
    
    override def copy =
      new Column(tensor.copy, newDomain, newValue).asInstanceOf[this.type];
    
    override def create[J](domain : Domain[J]) : Tensor[J] =
      tensor.create(domain);
    
    override def activeDomain : Set[(I1,I2)] =
      tensor.activeDomain.map(v => (v,newValue));
  }
  */
}

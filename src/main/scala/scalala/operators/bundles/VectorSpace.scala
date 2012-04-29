package scalala
package operators
package bundles

import scalar.Scalar
import generic.math.CanNorm
import generic.collection.{CanZipMapValues, CanCopy, CanMapValues, CanCreateZerosLike}

/**
 * A VectorSpace has vectors S with vector addition and multiplication
 * by scalars, along with a privileged zero vector.
 * We add in implicits for operations that fall out of them.
 * @author dlwh
 */
trait VectorSpace[S,V] {
  implicit def field : Scalar[S]
  implicit def numericOps(t: V):NumericOps[V];
  implicit def hasValuesMonadic(t: V):HasValuesMonadic[V,S]
  implicit def mapValues: CanMapValues[V,S,S,V]
  implicit def zipMapValues: CanZipMapValues[V,S,S,V]

  implicit val zeros: CanCreateZerosLike[V, V];
  def ones(v: V) = zeros(v) + field.one;

//  implicit def negV : UnaryOp[V,OpNeg,V];

  implicit def addSV : BinaryOp[S,V,OpAdd,V];
  implicit def addVS : BinaryOp[V,S,OpAdd,V];
  implicit def addVV : BinaryOp[V,V,OpAdd,V];

  implicit def subSV : BinaryOp[S,V,OpSub,V];
  implicit def subVS : BinaryOp[V,S,OpSub,V];
  implicit def subVV : BinaryOp[V,V,OpSub,V];

  implicit def mulSV : BinaryOp[S,V,OpMul,V];
  implicit def mulVS : BinaryOp[V,S,OpMul,V];
  implicit def mulVV : BinaryOp[V,V,OpMul,V];

  implicit def divSV : BinaryOp[S,V,OpDiv,V];
  implicit def divVS : BinaryOp[V,S,OpDiv,V];
  implicit def divVV : BinaryOp[V,V,OpDiv,V];
}

/**
 * Adds in mutating operations
 * @author dlwh
 */
trait MutableVectorSpace[S,V] extends VectorSpace[S,V] {

  override implicit def numericOps(t: V):MutableNumericOps[V]
  implicit val copy: CanCopy[V];

  implicit def addIntoVS : BinaryUpdateOp[V,S,OpAdd];
  implicit def addIntoVV : BinaryUpdateOp[V,V,OpAdd];

  implicit def subIntoVS : BinaryUpdateOp[V,S,OpSub];
  implicit def subIntoVV : BinaryUpdateOp[V,V,OpSub];

  implicit def divIntoVS : BinaryUpdateOp[V,S,OpDiv];
  implicit def divIntoVV : BinaryUpdateOp[V,V,OpDiv];

  implicit def mulIntoVS : BinaryUpdateOp[V,S,OpMul];
  implicit def mulIntoVV : BinaryUpdateOp[V,V,OpMul];
}



object VectorSpace {
  implicit def make[V,S](implicit _field : Scalar[S],
                         _numericOps: V=>NumericOps[V],
                         _hasValuesMonadic: V=>HasValuesMonadic[V,S],
                         _mapValues: CanMapValues[V,S,S,V],
                         _zipMapValues: CanZipMapValues[V,S,S,V],
                         _zeros: CanCreateZerosLike[V, V],
//                         _negV : UnaryOp[V,OpNeg,V],
                         _addSV : BinaryOp[S,V,OpAdd,V],
                         _addVS : BinaryOp[V,S,OpAdd,V],
                         _addVV : BinaryOp[V,V,OpAdd,V],

                         _subSV : BinaryOp[S,V,OpSub,V],
                         _subVS : BinaryOp[V,S,OpSub,V],
                         _subVV : BinaryOp[V,V,OpSub,V],

                         _mulSV : BinaryOp[S,V,OpMul,V],
                         _mulVS : BinaryOp[V,S,OpMul,V],
                         _mulVV : BinaryOp[V,V,OpMul,V],

                         _divSV : BinaryOp[S,V,OpDiv,V],
                         _divVS : BinaryOp[V,S,OpDiv,V],
                         _divVV : BinaryOp[V,V,OpDiv,V]):VectorSpace[S,V] = {
    new VectorSpace[S,V] {
      def field : Scalar[S] = _field;
      def numericOps(t: V):NumericOps[V] = _numericOps(t)
      def hasValuesMonadic(t: V):HasValuesMonadic[V,S] = _hasValuesMonadic(t)
      def mapValues: CanMapValues[V,S,S,V] = _mapValues;
      def zipMapValues: CanZipMapValues[V,S,S,V] = _zipMapValues;

      implicit val zeros: CanCreateZerosLike[V, V] = _zeros

//      implicit def negV : UnaryOp[V,OpNeg,V] = _negV;

      implicit def addSV : BinaryOp[S,V,OpAdd,V] = _addSV;
      implicit def addVS : BinaryOp[V,S,OpAdd,V] = _addVS;
      implicit def addVV : BinaryOp[V,V,OpAdd,V] = _addVV;

      implicit def subSV : BinaryOp[S,V,OpSub,V] = _subSV;
      implicit def subVS : BinaryOp[V,S,OpSub,V] = _subVS;
      implicit def subVV : BinaryOp[V,V,OpSub,V] = _subVV;

      implicit def mulSV : BinaryOp[S,V,OpMul,V] = _mulSV;
      implicit def mulVS : BinaryOp[V,S,OpMul,V] = _mulVS;
      implicit def mulVV : BinaryOp[V,V,OpMul,V] = _mulVV;

      implicit def divSV : BinaryOp[S,V,OpDiv,V] = _divSV;
      implicit def divVS : BinaryOp[V,S,OpDiv,V] = _divVS;
      implicit def divVV : BinaryOp[V,V,OpDiv,V] = _divVV;
    }
  }

}

object MutableVectorSpace {
  implicit def make[V,S](implicit _field : Scalar[S],
                         _numericOps: V=>MutableNumericOps[V],

                         _hasValuesMonadic: V=>HasValuesMonadic[V,S],
                         _mapValues: CanMapValues[V,S,S,V],
                         _zipMapValues: CanZipMapValues[V,S,S,V],
                         _zeros: CanCreateZerosLike[V, V],
                         _copy: CanCopy[V],
//                         _negV : UnaryOp[V,OpNeg,V],
                         _addSV : BinaryOp[S,V,OpAdd,V],
                         _addVS : BinaryOp[V,S,OpAdd,V],
                         _addVV : BinaryOp[V,V,OpAdd,V],

                         _subSV : BinaryOp[S,V,OpSub,V],
                         _subVS : BinaryOp[V,S,OpSub,V],
                         _subVV : BinaryOp[V,V,OpSub,V],

                         _mulSV : BinaryOp[S,V,OpMul,V],
                         _mulVS : BinaryOp[V,S,OpMul,V],
                         _mulVV : BinaryOp[V,V,OpMul,V],

                         _divSV : BinaryOp[S,V,OpDiv,V],
                         _divVS : BinaryOp[V,S,OpDiv,V],
                         _divVV : BinaryOp[V,V,OpDiv,V],

                         _addIntoVS : BinaryUpdateOp[V,S,OpAdd],
                         _addIntoVV : BinaryUpdateOp[V,V,OpAdd],

                         _mulIntoVS : BinaryUpdateOp[V,S,OpMul],
                         _mulIntoVV : BinaryUpdateOp[V,V,OpMul],

                         _subIntoVS : BinaryUpdateOp[V,S,OpSub],
                         _subIntoVV : BinaryUpdateOp[V,V,OpSub],

                         _divIntoVS : BinaryUpdateOp[V,S,OpDiv],
                         _divIntoVV : BinaryUpdateOp[V,V,OpDiv]):MutableVectorSpace[S,V] = {
    new MutableVectorSpace[S,V] {
      def field : Scalar[S] = _field;
      def numericOps(t: V) = _numericOps(t)
      def hasValuesMonadic(t: V):HasValuesMonadic[V,S] = _hasValuesMonadic(t)
      def mapValues: CanMapValues[V,S,S,V] = _mapValues;
      def zipMapValues: CanZipMapValues[V,S,S,V] = _zipMapValues;

      implicit val zeros: CanCreateZerosLike[V, V] = _zeros
      implicit val copy: CanCopy[V] = _copy;

//      implicit def negV : UnaryOp[V,OpNeg,V] = _negV;

      implicit def addSV : BinaryOp[S,V,OpAdd,V] = _addSV;
      implicit def addVS : BinaryOp[V,S,OpAdd,V] = _addVS;
      implicit def addVV : BinaryOp[V,V,OpAdd,V] = _addVV;

      implicit def subSV : BinaryOp[S,V,OpSub,V] = _subSV;
      implicit def subVS : BinaryOp[V,S,OpSub,V] = _subVS;
      implicit def subVV : BinaryOp[V,V,OpSub,V] = _subVV;

      implicit def mulSV : BinaryOp[S,V,OpMul,V] = _mulSV;
      implicit def mulVS : BinaryOp[V,S,OpMul,V] = _mulVS;
      implicit def mulVV : BinaryOp[V,V,OpMul,V] = _mulVV;

      implicit def divSV : BinaryOp[S,V,OpDiv,V] = _divSV;
      implicit def divVS : BinaryOp[V,S,OpDiv,V] = _divVS;
      implicit def divVV : BinaryOp[V,V,OpDiv,V] = _divVV;

      implicit def divIntoVV = _divIntoVV
      implicit def divIntoVS = _divIntoVS

      implicit def subIntoVV = _subIntoVV
      implicit def subIntoVS = _subIntoVS

      implicit def addIntoVV = _addIntoVV
      implicit def addIntoVS = _addIntoVS

      implicit def mulIntoVV = _mulIntoVV
      implicit def mulIntoVS = _mulIntoVS
    }
  }

}

/**
 * VectorSpace with an innerproduct
 * @author dlwh
 */
trait InnerProductSpace[S,V] extends VectorSpace[S,V] {
  implicit def innerVV : BinaryOp[V,V,OpMulInner,S];
}

object InnerProductSpace {
  implicit def make[V,S](implicit _field : Scalar[S],
                         _numericOps: V=>NumericOps[V],
                         _hasValuesMonadic: V=>HasValuesMonadic[V,S],
                         _mapValues: CanMapValues[V,S,S,V],
                         _zipMapValues: CanZipMapValues[V,S,S,V],
                         _zeros: CanCreateZerosLike[V, V],
//                         _negV : UnaryOp[V,OpNeg,V],
                         _addSV : BinaryOp[S,V,OpAdd,V],
                         _addVS : BinaryOp[V,S,OpAdd,V],
                         _addVV : BinaryOp[V,V,OpAdd,V],

                         _subSV : BinaryOp[S,V,OpSub,V],
                         _subVS : BinaryOp[V,S,OpSub,V],
                         _subVV : BinaryOp[V,V,OpSub,V],

                         _mulSV : BinaryOp[S,V,OpMul,V],
                         _mulVS : BinaryOp[V,S,OpMul,V],
                         _mulVV : BinaryOp[V,V,OpMul,V],

                         _divSV : BinaryOp[S,V,OpDiv,V],
                         _divVS : BinaryOp[V,S,OpDiv,V],
                         _divVV : BinaryOp[V,V,OpDiv,V],
                         _innerVV : BinaryOp[V,V,OpMulInner,S]):InnerProductSpace[S,V] = {
    new InnerProductSpace[S,V] {
      def field : Scalar[S] = _field;
      def numericOps(t: V):NumericOps[V] = _numericOps(t)
      def hasValuesMonadic(t: V):HasValuesMonadic[V,S] = _hasValuesMonadic(t)
      def mapValues: CanMapValues[V,S,S,V] = _mapValues;
      def zipMapValues: CanZipMapValues[V,S,S,V] = _zipMapValues;

      implicit val zeros: CanCreateZerosLike[V, V] = _zeros

//      implicit def negV : UnaryOp[V,OpNeg,V] = _negV;

      implicit def addSV : BinaryOp[S,V,OpAdd,V] = _addSV;
      implicit def addVS : BinaryOp[V,S,OpAdd,V] = _addVS;
      implicit def addVV : BinaryOp[V,V,OpAdd,V] = _addVV;

      implicit def subSV : BinaryOp[S,V,OpSub,V] = _subSV;
      implicit def subVS : BinaryOp[V,S,OpSub,V] = _subVS;
      implicit def subVV : BinaryOp[V,V,OpSub,V] = _subVV;

      implicit def mulSV : BinaryOp[S,V,OpMul,V] = _mulSV;
      implicit def mulVS : BinaryOp[V,S,OpMul,V] = _mulVS;
      implicit def mulVV : BinaryOp[V,V,OpMul,V] = _mulVV;

      implicit def divSV : BinaryOp[S,V,OpDiv,V] = _divSV;
      implicit def divVS : BinaryOp[V,S,OpDiv,V] = _divVS;
      implicit def divVV : BinaryOp[V,V,OpDiv,V] = _divVV;

      implicit def innerVV : BinaryOp[V,V,OpMulInner,S] = _innerVV;
    }
  }

}


trait MutableInnerProductSpace[S,V] extends InnerProductSpace[S,V] with MutableVectorSpace[S,V];

object MutableInnerProductSpace {
  implicit def make[S,V](implicit _field : Scalar[S],
                         _numericOps: V=>MutableNumericOps[V],
                         _hasValuesMonadic: V=>HasValuesMonadic[V,S],
                         _mapValues: CanMapValues[V,S,S,V],
                         _zipMapValues: CanZipMapValues[V,S,S,V],
                         _zeros: CanCreateZerosLike[V, V],
                         _copy: CanCopy[V],
//                         _negV : UnaryOp[V,OpNeg,V],
                         _addSV : BinaryOp[S,V,OpAdd,V],
                         _addVS : BinaryOp[V,S,OpAdd,V],
                         _addVV : BinaryOp[V,V,OpAdd,V],

                         _subSV : BinaryOp[S,V,OpSub,V],
                         _subVS : BinaryOp[V,S,OpSub,V],
                         _subVV : BinaryOp[V,V,OpSub,V],

                         _mulSV : BinaryOp[S,V,OpMul,V],
                         _mulVS : BinaryOp[V,S,OpMul,V],
                         _mulVV : BinaryOp[V,V,OpMul,V],

                         _divSV : BinaryOp[S,V,OpDiv,V],
                         _divVS : BinaryOp[V,S,OpDiv,V],
                         _divVV : BinaryOp[V,V,OpDiv,V],

                         _addIntoVS : BinaryUpdateOp[V,S,OpAdd],
                         _addIntoVV : BinaryUpdateOp[V,V,OpAdd],

                         _mulIntoVS : BinaryUpdateOp[V,S,OpMul],
                         _mulIntoVV : BinaryUpdateOp[V,V,OpMul],

                         _subIntoVS : BinaryUpdateOp[V,S,OpSub],
                         _subIntoVV : BinaryUpdateOp[V,V,OpSub],

                         _divIntoVS : BinaryUpdateOp[V,S,OpDiv],
                         _divIntoVV : BinaryUpdateOp[V,V,OpDiv],
                         _innerVV : BinaryOp[V,V,OpMulInner,S]):MutableInnerProductSpace[S,V] = {
    new MutableInnerProductSpace[S,V] {
      def field : Scalar[S] = _field;
      def numericOps(t: V) = _numericOps(t)
      def hasValuesMonadic(t: V):HasValuesMonadic[V,S] = _hasValuesMonadic(t)
      def mapValues: CanMapValues[V,S,S,V] = _mapValues;
      def zipMapValues: CanZipMapValues[V,S,S,V] = _zipMapValues;

      implicit val zeros: CanCreateZerosLike[V, V] = _zeros
      implicit val copy: CanCopy[V] = _copy;

//      implicit def negV : UnaryOp[V,OpNeg,V] = _negV;

      implicit def addSV : BinaryOp[S,V,OpAdd,V] = _addSV;
      implicit def addVS : BinaryOp[V,S,OpAdd,V] = _addVS;
      implicit def addVV : BinaryOp[V,V,OpAdd,V] = _addVV;

      implicit def subSV : BinaryOp[S,V,OpSub,V] = _subSV;
      implicit def subVS : BinaryOp[V,S,OpSub,V] = _subVS;
      implicit def subVV : BinaryOp[V,V,OpSub,V] = _subVV;

      implicit def mulSV : BinaryOp[S,V,OpMul,V] = _mulSV;
      implicit def mulVS : BinaryOp[V,S,OpMul,V] = _mulVS;
      implicit def mulVV : BinaryOp[V,V,OpMul,V] = _mulVV;

      implicit def divSV : BinaryOp[S,V,OpDiv,V] = _divSV;
      implicit def divVS : BinaryOp[V,S,OpDiv,V] = _divVS;
      implicit def divVV : BinaryOp[V,V,OpDiv,V] = _divVV;

      implicit def innerVV : BinaryOp[V,V,OpMulInner,S] = _innerVV;

      implicit def divIntoVV = _divIntoVV
      implicit def divIntoVS = _divIntoVS

      implicit def subIntoVV = _subIntoVV
      implicit def subIntoVS = _subIntoVS

      implicit def addIntoVV = _addIntoVV
      implicit def addIntoVS = _addIntoVS

      implicit def mulIntoVV = _mulIntoVV
      implicit def mulIntoVS = _mulIntoVS
    }
  }

}
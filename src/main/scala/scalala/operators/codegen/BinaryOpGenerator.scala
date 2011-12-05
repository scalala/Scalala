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

import scala.annotation.implicitNotFound;
import scala.collection.mutable.HashMap;

import scalala.scalar.Scalar;

trait Generator {
  protected def shorten(m : Manifest[_]) =
    m.toString.replaceAll(".*(\\$|\\.)","").replaceAll("\\[|\\]","");

  protected def toImport(m : Manifest[_]) =
    if (m.erasure.getName.contains(".")) "import "+m.erasure.getName else "";
  
  def mkInnerCode : String;
}

abstract class Function2Generator[A,B,To]
(implicit ma : Manifest[A], mb : Manifest[B], mt : Manifest[To])
extends Generator { self =>
  def className : String;

  def mkClassCode : String = {
    """
    |  %s
    |  %s
    |  %s
    |  class %s extends ((%s,%s)=>%s) {
    |    override def apply(a : %s, b : %s) : %s = { %s
    |    }
    |    override def toString = "DynamicallyCompiled"+getClass.getName
    |  }
    """.stripMargin.format(
      toImport(ma), toImport(mb), toImport(mt), className, ma, mb, mt,
      ma, mb, mt, mkInnerCode.replaceAll("\\$a","a").replaceAll("\\$b","b")
    );
  }
  
  def prefix(code : String) : Function2Generator[A,B,To] = {
    new Function2Generator[A,B,To] {
      override def className = "Prefixed"+self.className
      override def mkInnerCode = code + self.mkInnerCode
    }
  }
  
  def getInstance =
    DynamicCompiler.define(className,mkClassCode).newInstance().asInstanceOf[((A,B)=>To)];
}

abstract class JoinGenerator[A,B,To]
(implicit ma : Manifest[A], mb : Manifest[B], mt : Manifest[To])
extends Function2Generator[A,B,To];

class JoinArrayArrayGenerator[A,B,To](override val className : String)
(init : String, loop : Function2Generator[A,B,_], done : String)
(implicit ma : Manifest[A], mb : Manifest[B], mt : Manifest[To])
extends JoinGenerator[Array[A],Array[B],To] {
  def mkInnerCode : String = {
    """
      require($a.length == $b.length)
      %s
      var i = 0;
      while (i < $a.length) {
        { %s }
        i += 1;
      }
      %s
    """.format(
      init,
      loop.mkInnerCode.replaceAll("\\$a","\\$a(i)").replaceAll("\\$b","\\$b(i)"),
      done
    )
  }
}
  

/**
 * Generates inlined, specialized code for BinaryOp[A,B,O,To].
 */
abstract class BinaryOpGenerator[A,B,O<:OpType,To](
  implicit ma : Manifest[A], mb : Manifest[B], mo : Manifest[O], mt : Manifest[To])
extends Function2Generator[A,B,To] {
  def className() =
    shorten(mo)+shorten(ma)+shorten(mb);
  
  override def mkClassCode : String = {
    """
    |  import scalala.operators._
    |  class %s extends BinaryOp[%s,%s,%s,%s] {
    |    override def opType = %s
    |    override def apply(a : %s, b : %s) : %s = { %s
    |    }
    |    override def toString = "DynamicallyCompiled"+getClass.getName
    |  }
    """.stripMargin.format(
      className, ma, mb, shorten(mo), mt,
      shorten(mo),
      ma, mb, mt, mkInnerCode.replaceAll("\\$a","a").replaceAll("\\$b","b")
    );
  }
  
  override def getInstance =
    super.getInstance.asInstanceOf[BinaryOp[A,B,O,To]];
}

object BinaryOpGenerator {
  class BinaryOpScalarScalarInfixGenerator[V1,V2,O<:OpType,RV](val opType : O, symbol : Char)
  (implicit s1 : Scalar[V1], s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[V1], mv2 : Manifest[V2], mo : Manifest[O], mr : Manifest[RV])
  extends BinaryOpGenerator[V1,V2,O,RV] {
    def mkInnerCode = "$a"+symbol+"$b"
  }
  
  implicit def BinaryOpAddScalarScalarGenerator[V1,V2,RV]
  (implicit s1 : Scalar[V1], s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[V1], mv2 : Manifest[V2], mr : Manifest[RV])
  : BinaryOpGenerator[V1,V2,OpAdd,RV] =
    new BinaryOpScalarScalarInfixGenerator(OpAdd,'+');

  implicit def BinaryOpSubScalarScalarGenerator[V1,V2,RV]
  (implicit s1 : Scalar[V1], s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[V1], mv2 : Manifest[V2], mr : Manifest[RV])
  : BinaryOpGenerator[V1,V2,OpSub,RV] =
    new BinaryOpScalarScalarInfixGenerator(OpSub,'-');
  
  implicit def BinaryOpMulScalarScalarGenerator[V1,V2,RV]
  (implicit s1 : Scalar[V1], s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[V1], mv2 : Manifest[V2], mr : Manifest[RV])
  : BinaryOpGenerator[V1,V2,OpMul,RV] =
    new BinaryOpScalarScalarInfixGenerator(OpMul,'*');
  
  implicit def BinaryOpDivScalarScalarGenerator[V1,V2,RV]
  (implicit s1 : Scalar[V1], s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[V1], mv2 : Manifest[V2], mr : Manifest[RV])
  : BinaryOpGenerator[V1,V2,OpDiv,RV] =
    new BinaryOpScalarScalarInfixGenerator(OpDiv,'/');

  implicit def BinaryOpArrayArrayGenerator[V1,V2,O<:OpType,RV]
  (implicit op : BinaryOpGenerator[V1,V2,O,RV],
   mv1 : Manifest[V1], mv2 : Manifest[V2], mo : Manifest[O], mr : Manifest[RV])
  : BinaryOpGenerator[Array[V1],Array[V2],O,Array[RV]]
  = new BinaryOpGenerator[Array[V1],Array[V2],O,Array[RV]] {
    def mkInnerCode = """
      require($a.length == $b.length, "Inputs must be the same length");
      var rv = new Array[%s]($a.length);
      var i = 0;
      while (i < rv.length) {
        rv(i) = %s;
        i += 1;
      }
      rv;
    """.format(mr.toString, op.mkInnerCode.replaceAll("\\$a","\\$a(i)").replaceAll("\\$b","\\$b(i)"));
  }
  
  implicit def BinaryOpArrayScalarGenerator[V1,V2,O<:OpType,RV]
  (implicit op : BinaryOpGenerator[V1,V2,O,RV], s : Scalar[V2],
   mv1 : Manifest[V1], mv2 : Manifest[V2], mo : Manifest[O], mr : Manifest[RV])
  : BinaryOpGenerator[Array[V1], V2, O, Array[RV]]
  = new BinaryOpGenerator[Array[V1], V2, O, Array[RV]] {
    def mkInnerCode = """
      var rv = new Array[%s]($a.length);
      var i = 0;
      while (i < rv.length) {
        rv(i) = %s;
        i += 1;
      }
      rv;
    """.format(mr.toString, op.mkInnerCode.replaceAll("\\$a","\\$a(i)"))
  }
  
  implicit def BinaryOpScalarArrayGenerator[V1,V2,O<:OpType,RV]
  (implicit op : BinaryOpGenerator[V1,V2,O,RV], s : Scalar[V1],
   mv1 : Manifest[V1], mv2 : Manifest[V2], mo : Manifest[O], mr : Manifest[RV])
  : BinaryOpGenerator[V1, Array[V2], O, Array[RV]]
  = new BinaryOpGenerator[V1, Array[V2], O, Array[RV]] {
    def mkInnerCode = """
      var rv = new Array[%s]($b.length);
      var i = 0;
      while (i < rv.length) {
        rv(i) = %s;
        i += 1;
      }
      rv;
    """.format(mr.toString, op.mkInnerCode.replaceAll("\\$b","\\$b(i)"))
  }
  
  
  
  import scalala.tensor.dense._;
  
//  class JoinDenseVectorDenseVectorGenerator[DV[V]<:DenseVector[V],V1,V2,RV]
//  (className : String, setup : code :   
  
  class BinaryOpDenseVectorDenseVectorGenerator[DV[V]<:DenseVector[V],V1,V2,O<:OpType,RV]
  (className : String)(implicit op : BinaryOpGenerator[V1,V2,O,RV],
   s1 : Scalar[V1], s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[DV[V1]], mv2 : Manifest[DV[V2]], mo : Manifest[O], mr : Manifest[DV[RV]])
  extends BinaryOpGenerator[DV[V1],DV[V2],O,DV[RV]] {
    def mkInnerCode = """
      require($a.length == $b.length, "Vectors must have the same length");
      val rv = new Array[%s]($a.length);
      var ia = $a.offset;
      var ib = $b.offset;
      var ir = 0;
      while (ir < rv.length) {
        rv(ir) = %s;
        ia += $a.stride;
        ib += $b.stride;
        ir += 1;
      }
      new %s(rv);
    """.format(sr.manifest.toString,
      op.mkInnerCode.replaceAll("\\$a","\\$a.data(ia)").replaceAll("\\$b","\\$b.data(ib)"),
      className)
  }

  implicit def BinaryOpDenseVectorColDenseVectorColGenerator[V1,V2,O<:OpType,RV]
  (implicit op : BinaryOpGenerator[V1,V2,O,RV], s1 : Scalar[V1],
   s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[V1], mv2 : Manifest[V2], mo : Manifest[O], mr : Manifest[RV])
  : BinaryOpGenerator[DenseVectorCol[V1],DenseVectorCol[V2],O,DenseVectorCol[RV]]
  = new BinaryOpDenseVectorDenseVectorGenerator[DenseVectorCol,V1,V2,O,RV](
    classOf[DenseVectorCol[_]].getName)

  implicit def BinaryOpDenseVectorRowDenseVectorRowGenerator[V1,V2,O<:OpType,RV]
  (implicit op : BinaryOpGenerator[V1,V2,O,RV], s1 : Scalar[V1],
   s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[V1], mv2 : Manifest[V2], mo : Manifest[O], mr : Manifest[RV])
  : BinaryOpGenerator[DenseVectorRow[V1],DenseVectorRow[V2],O,DenseVectorRow[RV]]
  = new BinaryOpDenseVectorDenseVectorGenerator[DenseVectorRow,V1,V2,O,RV](
    classOf[DenseVectorRow[_]].getName)

  class BinaryOpDenseVectorScalarGenerator[DV[V]<:DenseVector[V],V1,V2,O<:OpType,RV]
  (className : String)(implicit op : BinaryOpGenerator[V1,V2,O,RV],
   s1 : Scalar[V1], s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[DV[V1]], mv2 : Manifest[V2], mo : Manifest[O], mr : Manifest[DV[RV]])
  extends BinaryOpGenerator[DV[V1],V2,O,DV[RV]] {
    def mkInnerCode = """
      val rv = new Array[%s]($a.length);
      var ia = $a.offset;
      var ir = 0;
      while (ir < rv.length) {
        rv(ir) = %s;
        ia += $a.stride;
        ir += 1;
      }
      new %s(rv);
    """.format(sr.manifest.toString, op.mkInnerCode.replaceAll("\\$a","\\$a.data(ia)"),className)
  }

  implicit def BinaryOpDenseVectorColScalarGenerator[V1,V2,O<:OpType,RV]
  (implicit op : BinaryOpGenerator[V1,V2,O,RV], s1 : Scalar[V1],
   s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[V1], mv2 : Manifest[V2], mo : Manifest[O], mr : Manifest[RV])
  : BinaryOpGenerator[DenseVectorCol[V1],V2,O,DenseVectorCol[RV]]
  = new BinaryOpDenseVectorScalarGenerator[DenseVectorCol,V1,V2,O,RV](
    classOf[DenseVectorCol[_]].getName)

  implicit def BinaryOpDenseVectorRowScalarGenerator[V1,V2,O<:OpType,RV]
  (implicit op : BinaryOpGenerator[V1,V2,O,RV], s1 : Scalar[V1],
   s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[V1], mv2 : Manifest[V2], mo : Manifest[O], mr : Manifest[RV])
  : BinaryOpGenerator[DenseVectorRow[V1],V2,O,DenseVectorRow[RV]]
  = new BinaryOpDenseVectorScalarGenerator[DenseVectorRow,V1,V2,O,RV](
    classOf[DenseVectorRow[_]].getName)

  class BinaryOpScalarDenseVectorGenerator[DV[V]<:DenseVector[V],V1,V2,O<:OpType,RV]
  (className : String)(implicit op : BinaryOpGenerator[V1,V2,O,RV],
   s1 : Scalar[V1], s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[V1], mv2 : Manifest[DV[V2]], mo : Manifest[O], mr : Manifest[DV[RV]])
  extends BinaryOpGenerator[V1,DV[V2],O,DV[RV]] {
    def mkInnerCode = """
      val rv = new Array[%s]($b.length);
      var ib = $b.offset;
      var ir = 0;
      while (ir < rv.length) {
        rv(ir) = %s;
        ib += $b.stride;
        ir += 1;
      }
      new %s(rv);
    """.format(sr.manifest.toString, op.mkInnerCode.replaceAll("\\$b","\\$b.data(ib)"),className)
  }

  implicit def BinaryOpScalarDenseVectorColGenerator[V1,V2,O<:OpType,RV]
  (implicit op : BinaryOpGenerator[V1,V2,O,RV], s1 : Scalar[V1],
   s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[V1], mv2 : Manifest[V2], mo : Manifest[O], mr : Manifest[RV])
  : BinaryOpGenerator[V1,DenseVectorCol[V2],O,DenseVectorCol[RV]]
  = new BinaryOpScalarDenseVectorGenerator[DenseVectorCol,V1,V2,O,RV](
    classOf[DenseVectorCol[_]].getName)

  implicit def BinaryOpScalarDenseVectorRowGenerator[V1,V2,O<:OpType,RV]
  (implicit op : BinaryOpGenerator[V1,V2,O,RV], s1 : Scalar[V1],
   s2 : Scalar[V2], sr : Scalar[RV], c1 : V1=>RV, c2 : V2=>RV,
   mv1 : Manifest[V1], mv2 : Manifest[V2], mo : Manifest[O], mr : Manifest[RV])
  : BinaryOpGenerator[V1,DenseVectorRow[V2],O,DenseVectorRow[RV]]
  = new BinaryOpScalarDenseVectorGenerator[DenseVectorRow,V1,V2,O,RV](
    classOf[DenseVectorRow[_]].getName)
}

object GeneratedRegistryEntries {
 import scalala.tensor.dense._;
  
  def main(args : Array[String]) {
    val ops = Array("OpAdd","OpSub","OpMul","OpDiv")
    val scalars = Array("Int","Long","Float","Double")
    val tensors = Array("Array","DenseVectorRow","DenseVectorCol")
    
    // returns the scalar result of combining the two operands
    def sr(s1 : String, s2 : String) =
      if (scalars.indexOf(s1) < scalars.indexOf(s2)) s2 else s1;
    
    val opsTensorTensorSame = 
      for (t <- tensors; s1 <- scalars; s2 <- scalars; op <- ops) yield
        "Registry.register[%s[%s],%s[%s],%s,%s[%s]]".format(t,s1,t,s2,op,t,sr(s1,s2))
    
    val opsTensorScalar =
      for (t <- tensors; s1 <- scalars; s2 <- scalars; op <- ops) yield
        "Registry.register[%s[%s],%s,%s,%s[%s]]".format(t,s1,s2,op,t,sr(s1,s2));
      
    val opsScalarTensor =
      for (t <- tensors; s1 <- scalars; s2 <- scalars; op <- ops) yield
        "Registry.register[%s,%s[%s],%s,%s[%s]]".format(s1,t,s2,op,t,sr(s1,s2));
    
    println("""
      |  def init() {
      |    initTensorTensorSame();
      |    initTensorScalar();
      |    initScalarTensor();
      |  }
      |
      |  def initTensorTensorSame() {
      |    %s
      |  }
      |
      |  def initTensorScalar() {
      |    %s
      |  }
      |
      |  def initScalarTensor() {
      |    %s
      |  }
      """.stripMargin.format(
        opsTensorTensorSame.mkString("\n    "),
        opsTensorScalar.mkString("\n    "),
        opsScalarTensor.mkString("\n    ")));
  }
}


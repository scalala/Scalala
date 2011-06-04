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

import scalala.scalar.Scalar;

// The contents of this file were automatically generated:

// println("trait UnaryTupleOps {");
// for (n <- 2 to 22) {
//   val va = (1 to n).map("VA"+_).mkString(",");
//   val rv = (1 to n).map("RV"+_).mkString(",");
//   val ops = (1 to n).map(i => "op! : UnaryOp[VA!,O,RV!]".replaceAll("!",""+i)).mkString(", ");
//   val code = (1 to n).map(i => "op!(v._!)".replaceAll("!",""+i)).mkString(", ");
//   val unaryOp =
// """
//   implicit def OpTuple%d[%s,%s,O<:OpType]
//   (implicit %s)
//   : UnaryOp[(%s),O,(%s)]
//   = new UnaryOp[(%s),O,(%s)] {
//     def opType = op1.opType;
//     def apply(v : (%s)) = (%s);
//   }
// """.format(n,va,rv,ops,va,rv,va,rv,va,code);
//   print(unaryOp);
// }
// println("}");
// 
// println("trait BinaryTupleOps {");
// for (n <- 2 to 22) {
//   val va = (1 to n).map("VA"+_).mkString(",");
//   val vb = (1 to n).map("VB"+_).mkString(",");
//   val rv = (1 to n).map("RV"+_).mkString(",");
// 
//   val opsTupleTuple = (1 to n).map(i => "op! : BinaryOp[VA!,VB!,O,RV!]".replaceAll("!",""+i)).mkString(", ");
//   val shapesTupleTuple = (1 to n).map(i => "c! : CompatibleShape[VA!,VB!]".replaceAll("!",""+i)).mkString(", ");
//   val valueTupleTuple = (1 to n).map(i => "op!(a._!,b._!)".replaceAll("!",""+i)).mkString(", ");
//   val codeTupleTuple =
// """
//   implicit def OpTuple%dTuple%d[%s,%s,%s,O<:OpType]
//   (implicit %s, %s)
//   : BinaryOp[(%s),(%s),O,(%s)]
//   = new BinaryOp[(%s),(%s),O,(%s)] {
//     def opType = op1.opType;
//     def apply(a : (%s), b : (%s)) = (%s);
//   }
// """.format(n, n, va, vb, rv, opsTupleTuple, shapesTupleTuple, va, vb, rv, va, vb, rv, va, vb, valueTupleTuple);
// 
//   val opsTupleScalar = (1 to n).map(i => "op! : BinaryOp[VA!,VS,O,RV!]".replaceAll("!",""+i)).mkString(", ");
//   val valueTupleScalar = (1 to n).map(i => "op!(a._!,s)".replaceAll("!",""+i)).mkString(", ");
//   val codeTupleScalar =
// """
//   implicit def OpTuple%dScalar[%s,VS,%s,O<:OpType]
//   (implicit %s, s : Scalar[VS])
//   : BinaryOp[(%s),VS,O,(%s)]
//   = new BinaryOp[(%s),VS,O,(%s)] {
//     def opType = op1.opType;
//     def apply(a : (%s), s : VS) = (%s);
//   }
// """.format(n, va, rv, opsTupleScalar, va, rv, va, rv, va, valueTupleScalar);
// 
//   val opsScalarTuple = (1 to n).map(i => "op! : BinaryOp[VS,VB!,O,RV!]".replaceAll("!",""+i)).mkString(", ");
//   val valueScalarTuple = (1 to n).map(i => "op!(s,b._!)".replaceAll("!",""+i)).mkString(", ");
//   val codeScalarTuple =
// """
//   implicit def OpScalarTuple%d[VS,%s,%s,O<:OpType]
//   (implicit %s, s : Scalar[VS])
//   : BinaryOp[VS,(%s),O,(%s)]
//   = new BinaryOp[VS,(%s),O,(%s)] {
//     def opType = op1.opType;
//     def apply(s : VS, b : (%s)) = (%s);
//   }
// """.format(n, vb, rv, opsScalarTuple, vb, rv, vb, rv, vb, valueScalarTuple);
// 
//   print(codeTupleTuple);
//   print(codeTupleScalar);
//   print(codeScalarTuple);
// }
// println("}");
// 
// println("object RichTuples {");
// for (n <- 2 to 22) {
//   val tupArg = (1 to n).map((if (n <= 3) "@specialized V" else "V") + _).mkString(", ");
//   val tupVal = (1 to n).map("V"+_).mkString(",");
//   println(
//     "  class RichTuple%d[%s](override val repr : (%s)) extends MutableNumericOps[(%s)];\n"
//       .format(n,tupArg,tupVal,tupVal)
//   );
// }
// println("}\n\n");
// 
// println("trait RichTupleImplicits {");
// println("  import RichTuples._");
// for (n <- 2 to 22) {
//   val tupArg = (1 to n).map((if (n <= 3) "@specialized V" else "V") + _).mkString(", ");
//   val tupVal = (1 to n).map("V"+_).mkString(",");
//   println(
//     "  implicit def richTuple%d[%s](value : (%s)) = new RichTuple%d(value);\n"
//       .format(n,tupArg,tupVal,n)
//   );
// }
// println("}\n");

/**
 * Unary operations on tensors of arity 2 to 22.
 *
 * @author dramage
 */
trait UnaryTupleOps {

  implicit def OpTuple2[VA1,VA2,RV1,RV2,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2])
  : UnaryOp[(VA1,VA2),O,(RV1,RV2)]
  = new UnaryOp[(VA1,VA2),O,(RV1,RV2)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2)) = (op1(v._1), op2(v._2));
  }

  implicit def OpTuple3[VA1,VA2,VA3,RV1,RV2,RV3,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3])
  : UnaryOp[(VA1,VA2,VA3),O,(RV1,RV2,RV3)]
  = new UnaryOp[(VA1,VA2,VA3),O,(RV1,RV2,RV3)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3)) = (op1(v._1), op2(v._2), op3(v._3));
  }

  implicit def OpTuple4[VA1,VA2,VA3,VA4,RV1,RV2,RV3,RV4,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4])
  : UnaryOp[(VA1,VA2,VA3,VA4),O,(RV1,RV2,RV3,RV4)]
  = new UnaryOp[(VA1,VA2,VA3,VA4),O,(RV1,RV2,RV3,RV4)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4));
  }

  implicit def OpTuple5[VA1,VA2,VA3,VA4,VA5,RV1,RV2,RV3,RV4,RV5,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5),O,(RV1,RV2,RV3,RV4,RV5)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5),O,(RV1,RV2,RV3,RV4,RV5)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5));
  }

  implicit def OpTuple6[VA1,VA2,VA3,VA4,VA5,VA6,RV1,RV2,RV3,RV4,RV5,RV6,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6),O,(RV1,RV2,RV3,RV4,RV5,RV6)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6),O,(RV1,RV2,RV3,RV4,RV5,RV6)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6));
  }

  implicit def OpTuple7[VA1,VA2,VA3,VA4,VA5,VA6,VA7,RV1,RV2,RV3,RV4,RV5,RV6,RV7,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7));
  }

  implicit def OpTuple8[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8));
  }

  implicit def OpTuple9[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9));
  }

  implicit def OpTuple10[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9], op10 : UnaryOp[VA10,O,RV10])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9), op10(v._10));
  }

  implicit def OpTuple11[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9], op10 : UnaryOp[VA10,O,RV10], op11 : UnaryOp[VA11,O,RV11])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9), op10(v._10), op11(v._11));
  }

  implicit def OpTuple12[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9], op10 : UnaryOp[VA10,O,RV10], op11 : UnaryOp[VA11,O,RV11], op12 : UnaryOp[VA12,O,RV12])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9), op10(v._10), op11(v._11), op12(v._12));
  }

  implicit def OpTuple13[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9], op10 : UnaryOp[VA10,O,RV10], op11 : UnaryOp[VA11,O,RV11], op12 : UnaryOp[VA12,O,RV12], op13 : UnaryOp[VA13,O,RV13])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9), op10(v._10), op11(v._11), op12(v._12), op13(v._13));
  }

  implicit def OpTuple14[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9], op10 : UnaryOp[VA10,O,RV10], op11 : UnaryOp[VA11,O,RV11], op12 : UnaryOp[VA12,O,RV12], op13 : UnaryOp[VA13,O,RV13], op14 : UnaryOp[VA14,O,RV14])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9), op10(v._10), op11(v._11), op12(v._12), op13(v._13), op14(v._14));
  }

  implicit def OpTuple15[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9], op10 : UnaryOp[VA10,O,RV10], op11 : UnaryOp[VA11,O,RV11], op12 : UnaryOp[VA12,O,RV12], op13 : UnaryOp[VA13,O,RV13], op14 : UnaryOp[VA14,O,RV14], op15 : UnaryOp[VA15,O,RV15])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9), op10(v._10), op11(v._11), op12(v._12), op13(v._13), op14(v._14), op15(v._15));
  }

  implicit def OpTuple16[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9], op10 : UnaryOp[VA10,O,RV10], op11 : UnaryOp[VA11,O,RV11], op12 : UnaryOp[VA12,O,RV12], op13 : UnaryOp[VA13,O,RV13], op14 : UnaryOp[VA14,O,RV14], op15 : UnaryOp[VA15,O,RV15], op16 : UnaryOp[VA16,O,RV16])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9), op10(v._10), op11(v._11), op12(v._12), op13(v._13), op14(v._14), op15(v._15), op16(v._16));
  }

  implicit def OpTuple17[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9], op10 : UnaryOp[VA10,O,RV10], op11 : UnaryOp[VA11,O,RV11], op12 : UnaryOp[VA12,O,RV12], op13 : UnaryOp[VA13,O,RV13], op14 : UnaryOp[VA14,O,RV14], op15 : UnaryOp[VA15,O,RV15], op16 : UnaryOp[VA16,O,RV16], op17 : UnaryOp[VA17,O,RV17])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9), op10(v._10), op11(v._11), op12(v._12), op13(v._13), op14(v._14), op15(v._15), op16(v._16), op17(v._17));
  }

  implicit def OpTuple18[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9], op10 : UnaryOp[VA10,O,RV10], op11 : UnaryOp[VA11,O,RV11], op12 : UnaryOp[VA12,O,RV12], op13 : UnaryOp[VA13,O,RV13], op14 : UnaryOp[VA14,O,RV14], op15 : UnaryOp[VA15,O,RV15], op16 : UnaryOp[VA16,O,RV16], op17 : UnaryOp[VA17,O,RV17], op18 : UnaryOp[VA18,O,RV18])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9), op10(v._10), op11(v._11), op12(v._12), op13(v._13), op14(v._14), op15(v._15), op16(v._16), op17(v._17), op18(v._18));
  }

  implicit def OpTuple19[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9], op10 : UnaryOp[VA10,O,RV10], op11 : UnaryOp[VA11,O,RV11], op12 : UnaryOp[VA12,O,RV12], op13 : UnaryOp[VA13,O,RV13], op14 : UnaryOp[VA14,O,RV14], op15 : UnaryOp[VA15,O,RV15], op16 : UnaryOp[VA16,O,RV16], op17 : UnaryOp[VA17,O,RV17], op18 : UnaryOp[VA18,O,RV18], op19 : UnaryOp[VA19,O,RV19])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9), op10(v._10), op11(v._11), op12(v._12), op13(v._13), op14(v._14), op15(v._15), op16(v._16), op17(v._17), op18(v._18), op19(v._19));
  }

  implicit def OpTuple20[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9], op10 : UnaryOp[VA10,O,RV10], op11 : UnaryOp[VA11,O,RV11], op12 : UnaryOp[VA12,O,RV12], op13 : UnaryOp[VA13,O,RV13], op14 : UnaryOp[VA14,O,RV14], op15 : UnaryOp[VA15,O,RV15], op16 : UnaryOp[VA16,O,RV16], op17 : UnaryOp[VA17,O,RV17], op18 : UnaryOp[VA18,O,RV18], op19 : UnaryOp[VA19,O,RV19], op20 : UnaryOp[VA20,O,RV20])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9), op10(v._10), op11(v._11), op12(v._12), op13(v._13), op14(v._14), op15(v._15), op16(v._16), op17(v._17), op18(v._18), op19(v._19), op20(v._20));
  }

  implicit def OpTuple21[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9], op10 : UnaryOp[VA10,O,RV10], op11 : UnaryOp[VA11,O,RV11], op12 : UnaryOp[VA12,O,RV12], op13 : UnaryOp[VA13,O,RV13], op14 : UnaryOp[VA14,O,RV14], op15 : UnaryOp[VA15,O,RV15], op16 : UnaryOp[VA16,O,RV16], op17 : UnaryOp[VA17,O,RV17], op18 : UnaryOp[VA18,O,RV18], op19 : UnaryOp[VA19,O,RV19], op20 : UnaryOp[VA20,O,RV20], op21 : UnaryOp[VA21,O,RV21])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9), op10(v._10), op11(v._11), op12(v._12), op13(v._13), op14(v._14), op15(v._15), op16(v._16), op17(v._17), op18(v._18), op19(v._19), op20(v._20), op21(v._21));
  }

  implicit def OpTuple22[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VA22,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,RV22,O<:OpType]
  (implicit op1 : UnaryOp[VA1,O,RV1], op2 : UnaryOp[VA2,O,RV2], op3 : UnaryOp[VA3,O,RV3], op4 : UnaryOp[VA4,O,RV4], op5 : UnaryOp[VA5,O,RV5], op6 : UnaryOp[VA6,O,RV6], op7 : UnaryOp[VA7,O,RV7], op8 : UnaryOp[VA8,O,RV8], op9 : UnaryOp[VA9,O,RV9], op10 : UnaryOp[VA10,O,RV10], op11 : UnaryOp[VA11,O,RV11], op12 : UnaryOp[VA12,O,RV12], op13 : UnaryOp[VA13,O,RV13], op14 : UnaryOp[VA14,O,RV14], op15 : UnaryOp[VA15,O,RV15], op16 : UnaryOp[VA16,O,RV16], op17 : UnaryOp[VA17,O,RV17], op18 : UnaryOp[VA18,O,RV18], op19 : UnaryOp[VA19,O,RV19], op20 : UnaryOp[VA20,O,RV20], op21 : UnaryOp[VA21,O,RV21], op22 : UnaryOp[VA22,O,RV22])
  : UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VA22),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,RV22)]
  = new UnaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VA22),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,RV22)] {
    def opType = op1.opType;
    def apply(v : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VA22)) = (op1(v._1), op2(v._2), op3(v._3), op4(v._4), op5(v._5), op6(v._6), op7(v._7), op8(v._8), op9(v._9), op10(v._10), op11(v._11), op12(v._12), op13(v._13), op14(v._14), op15(v._15), op16(v._16), op17(v._17), op18(v._18), op19(v._19), op20(v._20), op21(v._21), op22(v._22));
  }
}

/**
 * Binary operator support for tuples of aritys 2 to 22.
 *
 * @author dramage
 */
trait BinaryTupleOps {
  implicit def OpTuple2Tuple2[VA1,VA2,VB1,VB2,RV1,RV2,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2])
  : BinaryOp[(VA1,VA2),(VB1,VB2),O,(RV1,RV2)]
  = new BinaryOp[(VA1,VA2),(VB1,VB2),O,(RV1,RV2)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2), b : (VB1,VB2)) = (op1(a._1,b._1), op2(a._2,b._2));
  }


  implicit def OpTuple2Scalar[VA1,VA2,VS,RV1,RV2,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], s : Scalar[VS])
  : BinaryOp[(VA1,VA2),VS,O,(RV1,RV2)]
  = new BinaryOp[(VA1,VA2),VS,O,(RV1,RV2)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2), s : VS) = (op1(a._1,s), op2(a._2,s));
  }


  implicit def OpScalarTuple2[VS,VB1,VB2,RV1,RV2,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2),O,(RV1,RV2)]
  = new BinaryOp[VS,(VB1,VB2),O,(RV1,RV2)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2)) = (op1(s,b._1), op2(s,b._2));
  }


  implicit def OpTuple3Tuple3[VA1,VA2,VA3,VB1,VB2,VB3,RV1,RV2,RV3,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3])
  : BinaryOp[(VA1,VA2,VA3),(VB1,VB2,VB3),O,(RV1,RV2,RV3)]
  = new BinaryOp[(VA1,VA2,VA3),(VB1,VB2,VB3),O,(RV1,RV2,RV3)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3), b : (VB1,VB2,VB3)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3));
  }


  implicit def OpTuple3Scalar[VA1,VA2,VA3,VS,RV1,RV2,RV3,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3),VS,O,(RV1,RV2,RV3)]
  = new BinaryOp[(VA1,VA2,VA3),VS,O,(RV1,RV2,RV3)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s));
  }


  implicit def OpScalarTuple3[VS,VB1,VB2,VB3,RV1,RV2,RV3,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3),O,(RV1,RV2,RV3)]
  = new BinaryOp[VS,(VB1,VB2,VB3),O,(RV1,RV2,RV3)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3));
  }


  implicit def OpTuple4Tuple4[VA1,VA2,VA3,VA4,VB1,VB2,VB3,VB4,RV1,RV2,RV3,RV4,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4])
  : BinaryOp[(VA1,VA2,VA3,VA4),(VB1,VB2,VB3,VB4),O,(RV1,RV2,RV3,RV4)]
  = new BinaryOp[(VA1,VA2,VA3,VA4),(VB1,VB2,VB3,VB4),O,(RV1,RV2,RV3,RV4)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4), b : (VB1,VB2,VB3,VB4)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4));
  }


  implicit def OpTuple4Scalar[VA1,VA2,VA3,VA4,VS,RV1,RV2,RV3,RV4,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4),VS,O,(RV1,RV2,RV3,RV4)]
  = new BinaryOp[(VA1,VA2,VA3,VA4),VS,O,(RV1,RV2,RV3,RV4)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s));
  }


  implicit def OpScalarTuple4[VS,VB1,VB2,VB3,VB4,RV1,RV2,RV3,RV4,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4),O,(RV1,RV2,RV3,RV4)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4),O,(RV1,RV2,RV3,RV4)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4));
  }


  implicit def OpTuple5Tuple5[VA1,VA2,VA3,VA4,VA5,VB1,VB2,VB3,VB4,VB5,RV1,RV2,RV3,RV4,RV5,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5),(VB1,VB2,VB3,VB4,VB5),O,(RV1,RV2,RV3,RV4,RV5)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5),(VB1,VB2,VB3,VB4,VB5),O,(RV1,RV2,RV3,RV4,RV5)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5), b : (VB1,VB2,VB3,VB4,VB5)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5));
  }


  implicit def OpTuple5Scalar[VA1,VA2,VA3,VA4,VA5,VS,RV1,RV2,RV3,RV4,RV5,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5),VS,O,(RV1,RV2,RV3,RV4,RV5)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5),VS,O,(RV1,RV2,RV3,RV4,RV5)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s));
  }


  implicit def OpScalarTuple5[VS,VB1,VB2,VB3,VB4,VB5,RV1,RV2,RV3,RV4,RV5,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5),O,(RV1,RV2,RV3,RV4,RV5)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5),O,(RV1,RV2,RV3,RV4,RV5)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5));
  }


  implicit def OpTuple6Tuple6[VA1,VA2,VA3,VA4,VA5,VA6,VB1,VB2,VB3,VB4,VB5,VB6,RV1,RV2,RV3,RV4,RV5,RV6,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6),(VB1,VB2,VB3,VB4,VB5,VB6),O,(RV1,RV2,RV3,RV4,RV5,RV6)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6),(VB1,VB2,VB3,VB4,VB5,VB6),O,(RV1,RV2,RV3,RV4,RV5,RV6)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6), b : (VB1,VB2,VB3,VB4,VB5,VB6)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6));
  }


  implicit def OpTuple6Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VS,RV1,RV2,RV3,RV4,RV5,RV6,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s));
  }


  implicit def OpScalarTuple6[VS,VB1,VB2,VB3,VB4,VB5,VB6,RV1,RV2,RV3,RV4,RV5,RV6,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6),O,(RV1,RV2,RV3,RV4,RV5,RV6)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6),O,(RV1,RV2,RV3,RV4,RV5,RV6)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6));
  }


  implicit def OpTuple7Tuple7[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VB1,VB2,VB3,VB4,VB5,VB6,VB7,RV1,RV2,RV3,RV4,RV5,RV6,RV7,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7),(VB1,VB2,VB3,VB4,VB5,VB6,VB7),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7),(VB1,VB2,VB3,VB4,VB5,VB6,VB7),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7));
  }


  implicit def OpTuple7Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s));
  }


  implicit def OpScalarTuple7[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,RV1,RV2,RV3,RV4,RV5,RV6,RV7,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7));
  }


  implicit def OpTuple8Tuple8[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8));
  }


  implicit def OpTuple8Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s));
  }


  implicit def OpScalarTuple8[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8));
  }


  implicit def OpTuple9Tuple9[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9));
  }


  implicit def OpTuple9Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s));
  }


  implicit def OpScalarTuple9[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9));
  }


  implicit def OpTuple10Tuple10[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], op10 : BinaryOp[VA10,VB10,O,RV10], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9], c10 : CompatibleShape[VA10,VB10])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9), op10(a._10,b._10));
  }


  implicit def OpTuple10Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], op10 : BinaryOp[VA10,VS,O,RV10], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s), op10(a._10,s));
  }


  implicit def OpScalarTuple10[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], op10 : BinaryOp[VS,VB10,O,RV10], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9), op10(s,b._10));
  }


  implicit def OpTuple11Tuple11[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], op10 : BinaryOp[VA10,VB10,O,RV10], op11 : BinaryOp[VA11,VB11,O,RV11], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9], c10 : CompatibleShape[VA10,VB10], c11 : CompatibleShape[VA11,VB11])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9), op10(a._10,b._10), op11(a._11,b._11));
  }


  implicit def OpTuple11Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], op10 : BinaryOp[VA10,VS,O,RV10], op11 : BinaryOp[VA11,VS,O,RV11], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s), op10(a._10,s), op11(a._11,s));
  }


  implicit def OpScalarTuple11[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], op10 : BinaryOp[VS,VB10,O,RV10], op11 : BinaryOp[VS,VB11,O,RV11], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9), op10(s,b._10), op11(s,b._11));
  }


  implicit def OpTuple12Tuple12[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], op10 : BinaryOp[VA10,VB10,O,RV10], op11 : BinaryOp[VA11,VB11,O,RV11], op12 : BinaryOp[VA12,VB12,O,RV12], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9], c10 : CompatibleShape[VA10,VB10], c11 : CompatibleShape[VA11,VB11], c12 : CompatibleShape[VA12,VB12])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9), op10(a._10,b._10), op11(a._11,b._11), op12(a._12,b._12));
  }


  implicit def OpTuple12Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], op10 : BinaryOp[VA10,VS,O,RV10], op11 : BinaryOp[VA11,VS,O,RV11], op12 : BinaryOp[VA12,VS,O,RV12], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s), op10(a._10,s), op11(a._11,s), op12(a._12,s));
  }


  implicit def OpScalarTuple12[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], op10 : BinaryOp[VS,VB10,O,RV10], op11 : BinaryOp[VS,VB11,O,RV11], op12 : BinaryOp[VS,VB12,O,RV12], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9), op10(s,b._10), op11(s,b._11), op12(s,b._12));
  }


  implicit def OpTuple13Tuple13[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], op10 : BinaryOp[VA10,VB10,O,RV10], op11 : BinaryOp[VA11,VB11,O,RV11], op12 : BinaryOp[VA12,VB12,O,RV12], op13 : BinaryOp[VA13,VB13,O,RV13], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9], c10 : CompatibleShape[VA10,VB10], c11 : CompatibleShape[VA11,VB11], c12 : CompatibleShape[VA12,VB12], c13 : CompatibleShape[VA13,VB13])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9), op10(a._10,b._10), op11(a._11,b._11), op12(a._12,b._12), op13(a._13,b._13));
  }


  implicit def OpTuple13Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], op10 : BinaryOp[VA10,VS,O,RV10], op11 : BinaryOp[VA11,VS,O,RV11], op12 : BinaryOp[VA12,VS,O,RV12], op13 : BinaryOp[VA13,VS,O,RV13], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s), op10(a._10,s), op11(a._11,s), op12(a._12,s), op13(a._13,s));
  }


  implicit def OpScalarTuple13[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], op10 : BinaryOp[VS,VB10,O,RV10], op11 : BinaryOp[VS,VB11,O,RV11], op12 : BinaryOp[VS,VB12,O,RV12], op13 : BinaryOp[VS,VB13,O,RV13], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9), op10(s,b._10), op11(s,b._11), op12(s,b._12), op13(s,b._13));
  }


  implicit def OpTuple14Tuple14[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], op10 : BinaryOp[VA10,VB10,O,RV10], op11 : BinaryOp[VA11,VB11,O,RV11], op12 : BinaryOp[VA12,VB12,O,RV12], op13 : BinaryOp[VA13,VB13,O,RV13], op14 : BinaryOp[VA14,VB14,O,RV14], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9], c10 : CompatibleShape[VA10,VB10], c11 : CompatibleShape[VA11,VB11], c12 : CompatibleShape[VA12,VB12], c13 : CompatibleShape[VA13,VB13], c14 : CompatibleShape[VA14,VB14])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9), op10(a._10,b._10), op11(a._11,b._11), op12(a._12,b._12), op13(a._13,b._13), op14(a._14,b._14));
  }


  implicit def OpTuple14Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], op10 : BinaryOp[VA10,VS,O,RV10], op11 : BinaryOp[VA11,VS,O,RV11], op12 : BinaryOp[VA12,VS,O,RV12], op13 : BinaryOp[VA13,VS,O,RV13], op14 : BinaryOp[VA14,VS,O,RV14], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s), op10(a._10,s), op11(a._11,s), op12(a._12,s), op13(a._13,s), op14(a._14,s));
  }


  implicit def OpScalarTuple14[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], op10 : BinaryOp[VS,VB10,O,RV10], op11 : BinaryOp[VS,VB11,O,RV11], op12 : BinaryOp[VS,VB12,O,RV12], op13 : BinaryOp[VS,VB13,O,RV13], op14 : BinaryOp[VS,VB14,O,RV14], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9), op10(s,b._10), op11(s,b._11), op12(s,b._12), op13(s,b._13), op14(s,b._14));
  }


  implicit def OpTuple15Tuple15[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], op10 : BinaryOp[VA10,VB10,O,RV10], op11 : BinaryOp[VA11,VB11,O,RV11], op12 : BinaryOp[VA12,VB12,O,RV12], op13 : BinaryOp[VA13,VB13,O,RV13], op14 : BinaryOp[VA14,VB14,O,RV14], op15 : BinaryOp[VA15,VB15,O,RV15], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9], c10 : CompatibleShape[VA10,VB10], c11 : CompatibleShape[VA11,VB11], c12 : CompatibleShape[VA12,VB12], c13 : CompatibleShape[VA13,VB13], c14 : CompatibleShape[VA14,VB14], c15 : CompatibleShape[VA15,VB15])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9), op10(a._10,b._10), op11(a._11,b._11), op12(a._12,b._12), op13(a._13,b._13), op14(a._14,b._14), op15(a._15,b._15));
  }


  implicit def OpTuple15Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], op10 : BinaryOp[VA10,VS,O,RV10], op11 : BinaryOp[VA11,VS,O,RV11], op12 : BinaryOp[VA12,VS,O,RV12], op13 : BinaryOp[VA13,VS,O,RV13], op14 : BinaryOp[VA14,VS,O,RV14], op15 : BinaryOp[VA15,VS,O,RV15], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s), op10(a._10,s), op11(a._11,s), op12(a._12,s), op13(a._13,s), op14(a._14,s), op15(a._15,s));
  }


  implicit def OpScalarTuple15[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], op10 : BinaryOp[VS,VB10,O,RV10], op11 : BinaryOp[VS,VB11,O,RV11], op12 : BinaryOp[VS,VB12,O,RV12], op13 : BinaryOp[VS,VB13,O,RV13], op14 : BinaryOp[VS,VB14,O,RV14], op15 : BinaryOp[VS,VB15,O,RV15], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9), op10(s,b._10), op11(s,b._11), op12(s,b._12), op13(s,b._13), op14(s,b._14), op15(s,b._15));
  }


  implicit def OpTuple16Tuple16[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], op10 : BinaryOp[VA10,VB10,O,RV10], op11 : BinaryOp[VA11,VB11,O,RV11], op12 : BinaryOp[VA12,VB12,O,RV12], op13 : BinaryOp[VA13,VB13,O,RV13], op14 : BinaryOp[VA14,VB14,O,RV14], op15 : BinaryOp[VA15,VB15,O,RV15], op16 : BinaryOp[VA16,VB16,O,RV16], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9], c10 : CompatibleShape[VA10,VB10], c11 : CompatibleShape[VA11,VB11], c12 : CompatibleShape[VA12,VB12], c13 : CompatibleShape[VA13,VB13], c14 : CompatibleShape[VA14,VB14], c15 : CompatibleShape[VA15,VB15], c16 : CompatibleShape[VA16,VB16])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9), op10(a._10,b._10), op11(a._11,b._11), op12(a._12,b._12), op13(a._13,b._13), op14(a._14,b._14), op15(a._15,b._15), op16(a._16,b._16));
  }


  implicit def OpTuple16Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], op10 : BinaryOp[VA10,VS,O,RV10], op11 : BinaryOp[VA11,VS,O,RV11], op12 : BinaryOp[VA12,VS,O,RV12], op13 : BinaryOp[VA13,VS,O,RV13], op14 : BinaryOp[VA14,VS,O,RV14], op15 : BinaryOp[VA15,VS,O,RV15], op16 : BinaryOp[VA16,VS,O,RV16], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s), op10(a._10,s), op11(a._11,s), op12(a._12,s), op13(a._13,s), op14(a._14,s), op15(a._15,s), op16(a._16,s));
  }


  implicit def OpScalarTuple16[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], op10 : BinaryOp[VS,VB10,O,RV10], op11 : BinaryOp[VS,VB11,O,RV11], op12 : BinaryOp[VS,VB12,O,RV12], op13 : BinaryOp[VS,VB13,O,RV13], op14 : BinaryOp[VS,VB14,O,RV14], op15 : BinaryOp[VS,VB15,O,RV15], op16 : BinaryOp[VS,VB16,O,RV16], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9), op10(s,b._10), op11(s,b._11), op12(s,b._12), op13(s,b._13), op14(s,b._14), op15(s,b._15), op16(s,b._16));
  }


  implicit def OpTuple17Tuple17[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], op10 : BinaryOp[VA10,VB10,O,RV10], op11 : BinaryOp[VA11,VB11,O,RV11], op12 : BinaryOp[VA12,VB12,O,RV12], op13 : BinaryOp[VA13,VB13,O,RV13], op14 : BinaryOp[VA14,VB14,O,RV14], op15 : BinaryOp[VA15,VB15,O,RV15], op16 : BinaryOp[VA16,VB16,O,RV16], op17 : BinaryOp[VA17,VB17,O,RV17], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9], c10 : CompatibleShape[VA10,VB10], c11 : CompatibleShape[VA11,VB11], c12 : CompatibleShape[VA12,VB12], c13 : CompatibleShape[VA13,VB13], c14 : CompatibleShape[VA14,VB14], c15 : CompatibleShape[VA15,VB15], c16 : CompatibleShape[VA16,VB16], c17 : CompatibleShape[VA17,VB17])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9), op10(a._10,b._10), op11(a._11,b._11), op12(a._12,b._12), op13(a._13,b._13), op14(a._14,b._14), op15(a._15,b._15), op16(a._16,b._16), op17(a._17,b._17));
  }


  implicit def OpTuple17Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], op10 : BinaryOp[VA10,VS,O,RV10], op11 : BinaryOp[VA11,VS,O,RV11], op12 : BinaryOp[VA12,VS,O,RV12], op13 : BinaryOp[VA13,VS,O,RV13], op14 : BinaryOp[VA14,VS,O,RV14], op15 : BinaryOp[VA15,VS,O,RV15], op16 : BinaryOp[VA16,VS,O,RV16], op17 : BinaryOp[VA17,VS,O,RV17], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s), op10(a._10,s), op11(a._11,s), op12(a._12,s), op13(a._13,s), op14(a._14,s), op15(a._15,s), op16(a._16,s), op17(a._17,s));
  }


  implicit def OpScalarTuple17[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], op10 : BinaryOp[VS,VB10,O,RV10], op11 : BinaryOp[VS,VB11,O,RV11], op12 : BinaryOp[VS,VB12,O,RV12], op13 : BinaryOp[VS,VB13,O,RV13], op14 : BinaryOp[VS,VB14,O,RV14], op15 : BinaryOp[VS,VB15,O,RV15], op16 : BinaryOp[VS,VB16,O,RV16], op17 : BinaryOp[VS,VB17,O,RV17], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9), op10(s,b._10), op11(s,b._11), op12(s,b._12), op13(s,b._13), op14(s,b._14), op15(s,b._15), op16(s,b._16), op17(s,b._17));
  }


  implicit def OpTuple18Tuple18[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], op10 : BinaryOp[VA10,VB10,O,RV10], op11 : BinaryOp[VA11,VB11,O,RV11], op12 : BinaryOp[VA12,VB12,O,RV12], op13 : BinaryOp[VA13,VB13,O,RV13], op14 : BinaryOp[VA14,VB14,O,RV14], op15 : BinaryOp[VA15,VB15,O,RV15], op16 : BinaryOp[VA16,VB16,O,RV16], op17 : BinaryOp[VA17,VB17,O,RV17], op18 : BinaryOp[VA18,VB18,O,RV18], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9], c10 : CompatibleShape[VA10,VB10], c11 : CompatibleShape[VA11,VB11], c12 : CompatibleShape[VA12,VB12], c13 : CompatibleShape[VA13,VB13], c14 : CompatibleShape[VA14,VB14], c15 : CompatibleShape[VA15,VB15], c16 : CompatibleShape[VA16,VB16], c17 : CompatibleShape[VA17,VB17], c18 : CompatibleShape[VA18,VB18])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9), op10(a._10,b._10), op11(a._11,b._11), op12(a._12,b._12), op13(a._13,b._13), op14(a._14,b._14), op15(a._15,b._15), op16(a._16,b._16), op17(a._17,b._17), op18(a._18,b._18));
  }


  implicit def OpTuple18Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], op10 : BinaryOp[VA10,VS,O,RV10], op11 : BinaryOp[VA11,VS,O,RV11], op12 : BinaryOp[VA12,VS,O,RV12], op13 : BinaryOp[VA13,VS,O,RV13], op14 : BinaryOp[VA14,VS,O,RV14], op15 : BinaryOp[VA15,VS,O,RV15], op16 : BinaryOp[VA16,VS,O,RV16], op17 : BinaryOp[VA17,VS,O,RV17], op18 : BinaryOp[VA18,VS,O,RV18], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s), op10(a._10,s), op11(a._11,s), op12(a._12,s), op13(a._13,s), op14(a._14,s), op15(a._15,s), op16(a._16,s), op17(a._17,s), op18(a._18,s));
  }


  implicit def OpScalarTuple18[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], op10 : BinaryOp[VS,VB10,O,RV10], op11 : BinaryOp[VS,VB11,O,RV11], op12 : BinaryOp[VS,VB12,O,RV12], op13 : BinaryOp[VS,VB13,O,RV13], op14 : BinaryOp[VS,VB14,O,RV14], op15 : BinaryOp[VS,VB15,O,RV15], op16 : BinaryOp[VS,VB16,O,RV16], op17 : BinaryOp[VS,VB17,O,RV17], op18 : BinaryOp[VS,VB18,O,RV18], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9), op10(s,b._10), op11(s,b._11), op12(s,b._12), op13(s,b._13), op14(s,b._14), op15(s,b._15), op16(s,b._16), op17(s,b._17), op18(s,b._18));
  }


  implicit def OpTuple19Tuple19[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], op10 : BinaryOp[VA10,VB10,O,RV10], op11 : BinaryOp[VA11,VB11,O,RV11], op12 : BinaryOp[VA12,VB12,O,RV12], op13 : BinaryOp[VA13,VB13,O,RV13], op14 : BinaryOp[VA14,VB14,O,RV14], op15 : BinaryOp[VA15,VB15,O,RV15], op16 : BinaryOp[VA16,VB16,O,RV16], op17 : BinaryOp[VA17,VB17,O,RV17], op18 : BinaryOp[VA18,VB18,O,RV18], op19 : BinaryOp[VA19,VB19,O,RV19], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9], c10 : CompatibleShape[VA10,VB10], c11 : CompatibleShape[VA11,VB11], c12 : CompatibleShape[VA12,VB12], c13 : CompatibleShape[VA13,VB13], c14 : CompatibleShape[VA14,VB14], c15 : CompatibleShape[VA15,VB15], c16 : CompatibleShape[VA16,VB16], c17 : CompatibleShape[VA17,VB17], c18 : CompatibleShape[VA18,VB18], c19 : CompatibleShape[VA19,VB19])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9), op10(a._10,b._10), op11(a._11,b._11), op12(a._12,b._12), op13(a._13,b._13), op14(a._14,b._14), op15(a._15,b._15), op16(a._16,b._16), op17(a._17,b._17), op18(a._18,b._18), op19(a._19,b._19));
  }


  implicit def OpTuple19Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], op10 : BinaryOp[VA10,VS,O,RV10], op11 : BinaryOp[VA11,VS,O,RV11], op12 : BinaryOp[VA12,VS,O,RV12], op13 : BinaryOp[VA13,VS,O,RV13], op14 : BinaryOp[VA14,VS,O,RV14], op15 : BinaryOp[VA15,VS,O,RV15], op16 : BinaryOp[VA16,VS,O,RV16], op17 : BinaryOp[VA17,VS,O,RV17], op18 : BinaryOp[VA18,VS,O,RV18], op19 : BinaryOp[VA19,VS,O,RV19], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s), op10(a._10,s), op11(a._11,s), op12(a._12,s), op13(a._13,s), op14(a._14,s), op15(a._15,s), op16(a._16,s), op17(a._17,s), op18(a._18,s), op19(a._19,s));
  }


  implicit def OpScalarTuple19[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], op10 : BinaryOp[VS,VB10,O,RV10], op11 : BinaryOp[VS,VB11,O,RV11], op12 : BinaryOp[VS,VB12,O,RV12], op13 : BinaryOp[VS,VB13,O,RV13], op14 : BinaryOp[VS,VB14,O,RV14], op15 : BinaryOp[VS,VB15,O,RV15], op16 : BinaryOp[VS,VB16,O,RV16], op17 : BinaryOp[VS,VB17,O,RV17], op18 : BinaryOp[VS,VB18,O,RV18], op19 : BinaryOp[VS,VB19,O,RV19], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9), op10(s,b._10), op11(s,b._11), op12(s,b._12), op13(s,b._13), op14(s,b._14), op15(s,b._15), op16(s,b._16), op17(s,b._17), op18(s,b._18), op19(s,b._19));
  }


  implicit def OpTuple20Tuple20[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], op10 : BinaryOp[VA10,VB10,O,RV10], op11 : BinaryOp[VA11,VB11,O,RV11], op12 : BinaryOp[VA12,VB12,O,RV12], op13 : BinaryOp[VA13,VB13,O,RV13], op14 : BinaryOp[VA14,VB14,O,RV14], op15 : BinaryOp[VA15,VB15,O,RV15], op16 : BinaryOp[VA16,VB16,O,RV16], op17 : BinaryOp[VA17,VB17,O,RV17], op18 : BinaryOp[VA18,VB18,O,RV18], op19 : BinaryOp[VA19,VB19,O,RV19], op20 : BinaryOp[VA20,VB20,O,RV20], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9], c10 : CompatibleShape[VA10,VB10], c11 : CompatibleShape[VA11,VB11], c12 : CompatibleShape[VA12,VB12], c13 : CompatibleShape[VA13,VB13], c14 : CompatibleShape[VA14,VB14], c15 : CompatibleShape[VA15,VB15], c16 : CompatibleShape[VA16,VB16], c17 : CompatibleShape[VA17,VB17], c18 : CompatibleShape[VA18,VB18], c19 : CompatibleShape[VA19,VB19], c20 : CompatibleShape[VA20,VB20])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9), op10(a._10,b._10), op11(a._11,b._11), op12(a._12,b._12), op13(a._13,b._13), op14(a._14,b._14), op15(a._15,b._15), op16(a._16,b._16), op17(a._17,b._17), op18(a._18,b._18), op19(a._19,b._19), op20(a._20,b._20));
  }


  implicit def OpTuple20Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], op10 : BinaryOp[VA10,VS,O,RV10], op11 : BinaryOp[VA11,VS,O,RV11], op12 : BinaryOp[VA12,VS,O,RV12], op13 : BinaryOp[VA13,VS,O,RV13], op14 : BinaryOp[VA14,VS,O,RV14], op15 : BinaryOp[VA15,VS,O,RV15], op16 : BinaryOp[VA16,VS,O,RV16], op17 : BinaryOp[VA17,VS,O,RV17], op18 : BinaryOp[VA18,VS,O,RV18], op19 : BinaryOp[VA19,VS,O,RV19], op20 : BinaryOp[VA20,VS,O,RV20], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s), op10(a._10,s), op11(a._11,s), op12(a._12,s), op13(a._13,s), op14(a._14,s), op15(a._15,s), op16(a._16,s), op17(a._17,s), op18(a._18,s), op19(a._19,s), op20(a._20,s));
  }


  implicit def OpScalarTuple20[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], op10 : BinaryOp[VS,VB10,O,RV10], op11 : BinaryOp[VS,VB11,O,RV11], op12 : BinaryOp[VS,VB12,O,RV12], op13 : BinaryOp[VS,VB13,O,RV13], op14 : BinaryOp[VS,VB14,O,RV14], op15 : BinaryOp[VS,VB15,O,RV15], op16 : BinaryOp[VS,VB16,O,RV16], op17 : BinaryOp[VS,VB17,O,RV17], op18 : BinaryOp[VS,VB18,O,RV18], op19 : BinaryOp[VS,VB19,O,RV19], op20 : BinaryOp[VS,VB20,O,RV20], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9), op10(s,b._10), op11(s,b._11), op12(s,b._12), op13(s,b._13), op14(s,b._14), op15(s,b._15), op16(s,b._16), op17(s,b._17), op18(s,b._18), op19(s,b._19), op20(s,b._20));
  }


  implicit def OpTuple21Tuple21[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], op10 : BinaryOp[VA10,VB10,O,RV10], op11 : BinaryOp[VA11,VB11,O,RV11], op12 : BinaryOp[VA12,VB12,O,RV12], op13 : BinaryOp[VA13,VB13,O,RV13], op14 : BinaryOp[VA14,VB14,O,RV14], op15 : BinaryOp[VA15,VB15,O,RV15], op16 : BinaryOp[VA16,VB16,O,RV16], op17 : BinaryOp[VA17,VB17,O,RV17], op18 : BinaryOp[VA18,VB18,O,RV18], op19 : BinaryOp[VA19,VB19,O,RV19], op20 : BinaryOp[VA20,VB20,O,RV20], op21 : BinaryOp[VA21,VB21,O,RV21], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9], c10 : CompatibleShape[VA10,VB10], c11 : CompatibleShape[VA11,VB11], c12 : CompatibleShape[VA12,VB12], c13 : CompatibleShape[VA13,VB13], c14 : CompatibleShape[VA14,VB14], c15 : CompatibleShape[VA15,VB15], c16 : CompatibleShape[VA16,VB16], c17 : CompatibleShape[VA17,VB17], c18 : CompatibleShape[VA18,VB18], c19 : CompatibleShape[VA19,VB19], c20 : CompatibleShape[VA20,VB20], c21 : CompatibleShape[VA21,VB21])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9), op10(a._10,b._10), op11(a._11,b._11), op12(a._12,b._12), op13(a._13,b._13), op14(a._14,b._14), op15(a._15,b._15), op16(a._16,b._16), op17(a._17,b._17), op18(a._18,b._18), op19(a._19,b._19), op20(a._20,b._20), op21(a._21,b._21));
  }


  implicit def OpTuple21Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], op10 : BinaryOp[VA10,VS,O,RV10], op11 : BinaryOp[VA11,VS,O,RV11], op12 : BinaryOp[VA12,VS,O,RV12], op13 : BinaryOp[VA13,VS,O,RV13], op14 : BinaryOp[VA14,VS,O,RV14], op15 : BinaryOp[VA15,VS,O,RV15], op16 : BinaryOp[VA16,VS,O,RV16], op17 : BinaryOp[VA17,VS,O,RV17], op18 : BinaryOp[VA18,VS,O,RV18], op19 : BinaryOp[VA19,VS,O,RV19], op20 : BinaryOp[VA20,VS,O,RV20], op21 : BinaryOp[VA21,VS,O,RV21], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s), op10(a._10,s), op11(a._11,s), op12(a._12,s), op13(a._13,s), op14(a._14,s), op15(a._15,s), op16(a._16,s), op17(a._17,s), op18(a._18,s), op19(a._19,s), op20(a._20,s), op21(a._21,s));
  }


  implicit def OpScalarTuple21[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], op10 : BinaryOp[VS,VB10,O,RV10], op11 : BinaryOp[VS,VB11,O,RV11], op12 : BinaryOp[VS,VB12,O,RV12], op13 : BinaryOp[VS,VB13,O,RV13], op14 : BinaryOp[VS,VB14,O,RV14], op15 : BinaryOp[VS,VB15,O,RV15], op16 : BinaryOp[VS,VB16,O,RV16], op17 : BinaryOp[VS,VB17,O,RV17], op18 : BinaryOp[VS,VB18,O,RV18], op19 : BinaryOp[VS,VB19,O,RV19], op20 : BinaryOp[VS,VB20,O,RV20], op21 : BinaryOp[VS,VB21,O,RV21], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9), op10(s,b._10), op11(s,b._11), op12(s,b._12), op13(s,b._13), op14(s,b._14), op15(s,b._15), op16(s,b._16), op17(s,b._17), op18(s,b._18), op19(s,b._19), op20(s,b._20), op21(s,b._21));
  }


  implicit def OpTuple22Tuple22[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VA22,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21,VB22,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,RV22,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VB1,O,RV1], op2 : BinaryOp[VA2,VB2,O,RV2], op3 : BinaryOp[VA3,VB3,O,RV3], op4 : BinaryOp[VA4,VB4,O,RV4], op5 : BinaryOp[VA5,VB5,O,RV5], op6 : BinaryOp[VA6,VB6,O,RV6], op7 : BinaryOp[VA7,VB7,O,RV7], op8 : BinaryOp[VA8,VB8,O,RV8], op9 : BinaryOp[VA9,VB9,O,RV9], op10 : BinaryOp[VA10,VB10,O,RV10], op11 : BinaryOp[VA11,VB11,O,RV11], op12 : BinaryOp[VA12,VB12,O,RV12], op13 : BinaryOp[VA13,VB13,O,RV13], op14 : BinaryOp[VA14,VB14,O,RV14], op15 : BinaryOp[VA15,VB15,O,RV15], op16 : BinaryOp[VA16,VB16,O,RV16], op17 : BinaryOp[VA17,VB17,O,RV17], op18 : BinaryOp[VA18,VB18,O,RV18], op19 : BinaryOp[VA19,VB19,O,RV19], op20 : BinaryOp[VA20,VB20,O,RV20], op21 : BinaryOp[VA21,VB21,O,RV21], op22 : BinaryOp[VA22,VB22,O,RV22], c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2], c3 : CompatibleShape[VA3,VB3], c4 : CompatibleShape[VA4,VB4], c5 : CompatibleShape[VA5,VB5], c6 : CompatibleShape[VA6,VB6], c7 : CompatibleShape[VA7,VB7], c8 : CompatibleShape[VA8,VB8], c9 : CompatibleShape[VA9,VB9], c10 : CompatibleShape[VA10,VB10], c11 : CompatibleShape[VA11,VB11], c12 : CompatibleShape[VA12,VB12], c13 : CompatibleShape[VA13,VB13], c14 : CompatibleShape[VA14,VB14], c15 : CompatibleShape[VA15,VB15], c16 : CompatibleShape[VA16,VB16], c17 : CompatibleShape[VA17,VB17], c18 : CompatibleShape[VA18,VB18], c19 : CompatibleShape[VA19,VB19], c20 : CompatibleShape[VA20,VB20], c21 : CompatibleShape[VA21,VB21], c22 : CompatibleShape[VA22,VB22])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VA22),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21,VB22),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,RV22)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VA22),(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21,VB22),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,RV22)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VA22), b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21,VB22)) = (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4,b._4), op5(a._5,b._5), op6(a._6,b._6), op7(a._7,b._7), op8(a._8,b._8), op9(a._9,b._9), op10(a._10,b._10), op11(a._11,b._11), op12(a._12,b._12), op13(a._13,b._13), op14(a._14,b._14), op15(a._15,b._15), op16(a._16,b._16), op17(a._17,b._17), op18(a._18,b._18), op19(a._19,b._19), op20(a._20,b._20), op21(a._21,b._21), op22(a._22,b._22));
  }


  implicit def OpTuple22Scalar[VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VA22,VS,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,RV22,O<:OpType]
  (implicit op1 : BinaryOp[VA1,VS,O,RV1], op2 : BinaryOp[VA2,VS,O,RV2], op3 : BinaryOp[VA3,VS,O,RV3], op4 : BinaryOp[VA4,VS,O,RV4], op5 : BinaryOp[VA5,VS,O,RV5], op6 : BinaryOp[VA6,VS,O,RV6], op7 : BinaryOp[VA7,VS,O,RV7], op8 : BinaryOp[VA8,VS,O,RV8], op9 : BinaryOp[VA9,VS,O,RV9], op10 : BinaryOp[VA10,VS,O,RV10], op11 : BinaryOp[VA11,VS,O,RV11], op12 : BinaryOp[VA12,VS,O,RV12], op13 : BinaryOp[VA13,VS,O,RV13], op14 : BinaryOp[VA14,VS,O,RV14], op15 : BinaryOp[VA15,VS,O,RV15], op16 : BinaryOp[VA16,VS,O,RV16], op17 : BinaryOp[VA17,VS,O,RV17], op18 : BinaryOp[VA18,VS,O,RV18], op19 : BinaryOp[VA19,VS,O,RV19], op20 : BinaryOp[VA20,VS,O,RV20], op21 : BinaryOp[VA21,VS,O,RV21], op22 : BinaryOp[VA22,VS,O,RV22], s : Scalar[VS])
  : BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VA22),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,RV22)]
  = new BinaryOp[(VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VA22),VS,O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,RV22)] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2,VA3,VA4,VA5,VA6,VA7,VA8,VA9,VA10,VA11,VA12,VA13,VA14,VA15,VA16,VA17,VA18,VA19,VA20,VA21,VA22), s : VS) = (op1(a._1,s), op2(a._2,s), op3(a._3,s), op4(a._4,s), op5(a._5,s), op6(a._6,s), op7(a._7,s), op8(a._8,s), op9(a._9,s), op10(a._10,s), op11(a._11,s), op12(a._12,s), op13(a._13,s), op14(a._14,s), op15(a._15,s), op16(a._16,s), op17(a._17,s), op18(a._18,s), op19(a._19,s), op20(a._20,s), op21(a._21,s), op22(a._22,s));
  }


  implicit def OpScalarTuple22[VS,VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21,VB22,RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,RV22,O<:OpType]
  (implicit op1 : BinaryOp[VS,VB1,O,RV1], op2 : BinaryOp[VS,VB2,O,RV2], op3 : BinaryOp[VS,VB3,O,RV3], op4 : BinaryOp[VS,VB4,O,RV4], op5 : BinaryOp[VS,VB5,O,RV5], op6 : BinaryOp[VS,VB6,O,RV6], op7 : BinaryOp[VS,VB7,O,RV7], op8 : BinaryOp[VS,VB8,O,RV8], op9 : BinaryOp[VS,VB9,O,RV9], op10 : BinaryOp[VS,VB10,O,RV10], op11 : BinaryOp[VS,VB11,O,RV11], op12 : BinaryOp[VS,VB12,O,RV12], op13 : BinaryOp[VS,VB13,O,RV13], op14 : BinaryOp[VS,VB14,O,RV14], op15 : BinaryOp[VS,VB15,O,RV15], op16 : BinaryOp[VS,VB16,O,RV16], op17 : BinaryOp[VS,VB17,O,RV17], op18 : BinaryOp[VS,VB18,O,RV18], op19 : BinaryOp[VS,VB19,O,RV19], op20 : BinaryOp[VS,VB20,O,RV20], op21 : BinaryOp[VS,VB21,O,RV21], op22 : BinaryOp[VS,VB22,O,RV22], s : Scalar[VS])
  : BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21,VB22),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,RV22)]
  = new BinaryOp[VS,(VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21,VB22),O,(RV1,RV2,RV3,RV4,RV5,RV6,RV7,RV8,RV9,RV10,RV11,RV12,RV13,RV14,RV15,RV16,RV17,RV18,RV19,RV20,RV21,RV22)] {
    def opType = op1.opType;
    def apply(s : VS, b : (VB1,VB2,VB3,VB4,VB5,VB6,VB7,VB8,VB9,VB10,VB11,VB12,VB13,VB14,VB15,VB16,VB17,VB18,VB19,VB20,VB21,VB22)) = (op1(s,b._1), op2(s,b._2), op3(s,b._3), op4(s,b._4), op5(s,b._5), op6(s,b._6), op7(s,b._7), op8(s,b._8), op9(s,b._9), op10(s,b._10), op11(s,b._11), op12(s,b._12), op13(s,b._13), op14(s,b._14), op15(s,b._15), op16(s,b._16), op17(s,b._17), op18(s,b._18), op19(s,b._19), op20(s,b._20), op21(s,b._21), op22(s,b._22));
  }
}

object RichTuples {
  class RichTuple2[@specialized V1, @specialized V2](override val repr : (V1,V2)) extends MutableNumericOps[(V1,V2)];

  class RichTuple3[@specialized V1, @specialized V2, @specialized V3](override val repr : (V1,V2,V3)) extends MutableNumericOps[(V1,V2,V3)];

  class RichTuple4[V1, V2, V3, V4](override val repr : (V1,V2,V3,V4)) extends MutableNumericOps[(V1,V2,V3,V4)];

  class RichTuple5[V1, V2, V3, V4, V5](override val repr : (V1,V2,V3,V4,V5)) extends MutableNumericOps[(V1,V2,V3,V4,V5)];

  class RichTuple6[V1, V2, V3, V4, V5, V6](override val repr : (V1,V2,V3,V4,V5,V6)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6)];

  class RichTuple7[V1, V2, V3, V4, V5, V6, V7](override val repr : (V1,V2,V3,V4,V5,V6,V7)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7)];

  class RichTuple8[V1, V2, V3, V4, V5, V6, V7, V8](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8)];

  class RichTuple9[V1, V2, V3, V4, V5, V6, V7, V8, V9](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9)];

  class RichTuple10[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10)];

  class RichTuple11[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11)];

  class RichTuple12[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12)];

  class RichTuple13[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13)];

  class RichTuple14[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14)];

  class RichTuple15[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15)];

  class RichTuple16[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16)];

  class RichTuple17[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17)];

  class RichTuple18[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18)];

  class RichTuple19[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19)];

  class RichTuple20[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20)];

  class RichTuple21[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21)];

  class RichTuple22[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22](override val repr : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21,V22)) extends MutableNumericOps[(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21,V22)];

}


trait RichTupleImplicits {
  import RichTuples._;
  
  implicit def richTuple2[@specialized V1, @specialized V2](value : (V1,V2)) = new RichTuple2(value);

  implicit def richTuple3[@specialized V1, @specialized V2, @specialized V3](value : (V1,V2,V3)) = new RichTuple3(value);

  implicit def richTuple4[V1, V2, V3, V4](value : (V1,V2,V3,V4)) = new RichTuple4(value);

  implicit def richTuple5[V1, V2, V3, V4, V5](value : (V1,V2,V3,V4,V5)) = new RichTuple5(value);

  implicit def richTuple6[V1, V2, V3, V4, V5, V6](value : (V1,V2,V3,V4,V5,V6)) = new RichTuple6(value);

  implicit def richTuple7[V1, V2, V3, V4, V5, V6, V7](value : (V1,V2,V3,V4,V5,V6,V7)) = new RichTuple7(value);

  implicit def richTuple8[V1, V2, V3, V4, V5, V6, V7, V8](value : (V1,V2,V3,V4,V5,V6,V7,V8)) = new RichTuple8(value);

  implicit def richTuple9[V1, V2, V3, V4, V5, V6, V7, V8, V9](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9)) = new RichTuple9(value);

  implicit def richTuple10[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10)) = new RichTuple10(value);

  implicit def richTuple11[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11)) = new RichTuple11(value);

  implicit def richTuple12[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12)) = new RichTuple12(value);

  implicit def richTuple13[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13)) = new RichTuple13(value);

  implicit def richTuple14[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14)) = new RichTuple14(value);

  implicit def richTuple15[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15)) = new RichTuple15(value);

  implicit def richTuple16[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16)) = new RichTuple16(value);

  implicit def richTuple17[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17)) = new RichTuple17(value);

  implicit def richTuple18[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18)) = new RichTuple18(value);

  implicit def richTuple19[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19)) = new RichTuple19(value);

  implicit def richTuple20[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20)) = new RichTuple20(value);

  implicit def richTuple21[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21)) = new RichTuple21(value);

  implicit def richTuple22[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22](value : (V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21,V22)) = new RichTuple22(value);
}


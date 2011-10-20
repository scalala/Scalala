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

/**
 * Marker trait for some operation, be it UnaryOp, BinaryOp, or
 * BinaryUpdateOp.
 *
 * @author dramage
 */
trait OpType;

/**
 * Type marker for BinaryOp A :+ B and BinaryUpdateOp A :+= B.
 *
 * @author dramage
 */
trait OpAdd extends OpType;
object OpAdd extends OpAdd;

/**
 * Type marker for BinaryOp A :- B and BinaryUpdateOp A :-= B.
 *
 * @author dramage
 */
trait OpSub extends OpType;
object OpSub extends OpSub;

/**
 * Type marker for BinaryOp A :* B and BinaryUpdateOp A :*= B.
 *
 * @author dramage
 */
trait OpMul extends OpType;
object OpMul extends OpMul;

/**
 * Type marker for BinaryOp A :/ B and BinaryUpdateOp A:/= B.
 *
 * @author dramage
 */
trait OpDiv extends OpType;
object OpDiv extends OpDiv;

/**
 * Type marker for BinaryOp A :% B and BinaryUpdateOp A:%= B.
 *
 * @author dramage
 */
trait OpMod extends OpType;
object OpMod extends OpMod;

/**
 * Type marker for BinaryOp A :^ B and BinaryUpdateOp A:^= B.
 *
 * @author dramage
 */
trait OpPow extends OpType;
object OpPow extends OpPow;

/**
 * Type marker for BinaryOp A :&lt; B.
 *
 * @author dramage
 */
trait OpLT  extends OpType;
object OpLT  extends OpLT;

/**
 * Type marker for BinaryOp A :&lt;= B.
 *
 * @author dramage
 */
trait OpLTE extends OpType;
object OpLTE extends OpLTE;

/**
 * Type marker for BinaryOp A :&gt; B.
 *
 * @author dramage
 */
trait OpGT  extends OpType;
object OpGT  extends OpGT;

/**
 * Type marker for BinaryOp A :&gt;= B.
 *
 * @author dramage
 */
trait OpGTE extends OpType;
object OpGTE extends OpGTE;

/**
 * Type marker for BinaryOp A :== B.
 *
 * @author dramage
 */
trait OpEq  extends OpType;
object OpEq  extends OpEq;

/**
 * Type marker for BinaryOp A :!= B.
 *
 * @author dramage
 */
trait OpNe  extends OpType;
object OpNe  extends OpNe;

/**
 * Type marker for BinaryUpdateOp A := B.
 *
 * @author dramage
 */
trait OpSet extends OpType;
object OpSet extends OpSet;

/**
 * Type marker for BinaryOp A :&& B
 *
 * @author dramage
 */
trait OpAnd extends OpType;
object OpAnd extends OpAnd;

/**
 * Type marker for BinaryOp A :|| B
 *
 * @author dramage
 */
trait OpOr extends OpType;
object OpOr extends OpOr;

/**
 * Type marker for BinaryOp A :^^ B
 *
 * @author dramage
 */
trait OpXor extends OpType;
object OpXor extends OpXor;

/**
 * Type marker for UnaryOp -A.
 *
 * @author dramage
 */
trait OpNeg extends OpType;
object OpNeg extends OpNeg;

/**
 * Type marker for UnaryOp !A.
 *
 * @author dramage
 */
trait OpNot extends OpType;
object OpNot extends OpNot;


/**
 * Type marker for inner (dot) product of A and B.
 *
 * @author dramage
 */
trait OpMulInner extends OpType;
object OpMulInner extends OpMulInner;

/**
 * Type marker for BinaryOp A * B when A is a row.
 *
 * @author dramage
 */
trait OpMulRowVectorBy extends OpType;
object OpMulRowVectorBy extends OpMulRowVectorBy;

/**
 * Type marker for BinaryOp A * B when A is a column.
 *
 * @author dramage
 */
trait OpMulColVectorBy extends OpType;
object OpMulColVectorBy extends OpMulColVectorBy;

/**
 * Type marker for BinaryOp A * B when A is a matrix.
 *
 * @author dramage
 */
trait OpMulMatrixBy extends OpType;
object OpMulMatrixBy extends OpMulMatrixBy;

/**
 * Type marker for BinaryOp A \ B when A is a matrix.
 *
 * @author dramage
 */
trait OpSolveMatrixBy extends OpType;
object OpSolveMatrixBy extends OpSolveMatrixBy;


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

/**
 * Type marker for BinaryOp A :- B and BinaryUpdateOp A :-= B.
 *
 * @author dramage
 */
trait OpSub extends OpType;

/**
 * Type marker for BinaryOp A :* B and BinaryUpdateOp A :*= B.
 *
 * @author dramage
 */
trait OpMul extends OpType;

/**
 * Type marker for BinaryOp A :/ B and BinaryUpdateOp A:/= B.
 *
 * @author dramage
 */
trait OpDiv extends OpType;

/**
 * Type marker for BinaryOp A :% B and BinaryUpdateOp A:%= B.
 *
 * @author dramage
 */
trait OpMod extends OpType;

/**
 * Type marker for BinaryOp A :^ B and BinaryUpdateOp A:^= B.
 *
 * @author dramage
 */
trait OpPow extends OpType;

/**
 * Type marker for BinaryOp A :&lt; B.
 *
 * @author dramage
 */
trait OpLT  extends OpType;

/**
 * Type marker for BinaryOp A :&lt;= B.
 *
 * @author dramage
 */
trait OpLTE extends OpType;

/**
 * Type marker for BinaryOp A :&gt; B.
 *
 * @author dramage
 */
trait OpGT  extends OpType;

/**
 * Type marker for BinaryOp A :&gt;= B.
 *
 * @author dramage
 */
trait OpGTE extends OpType;

/**
 * Type marker for BinaryOp A :== B.
 *
 * @author dramage
 */
trait OpEq  extends OpType;

/**
 * Type marker for BinaryOp A :!= B.
 *
 * @author dramage
 */
trait OpNe  extends OpType;

/**
 * Type marker for BinaryUpdateOp A := B.
 *
 * @author dramage
 */
trait OpSet extends OpType;

/**
 * Type marker for UnaryOp -A.
 *
 * @author dramage
 */
trait OpNeg extends OpType;

/**
 * Type marker for UnaryOp casting a value to another vaule.
 *
 * @author dramage
 */
trait OpCast extends OpType;

/**
 * Type marker for UnaryOp to take the transpose of A.
 *
 * @author dramage
 */
trait OpTranspose extends OpType;

/**
 * Type marker for BinaryOp A * B when A is a row.
 *
 * @author dramage
 */
trait OpMulRowVectorBy extends OpType;

/**
 * Type marker for BinaryOp A * B when A is a column.
 *
 * @author dramage
 */
trait OpMulColVectorBy extends OpType;

/**
 * Type marker for BinaryOp A * B when A is a matrix.
 *
 * @author dramage
 */
trait OpMulMatrixBy extends OpType;

/**
 * Type marker for BinaryOp A \ B when A is a matrix.
 *
 * @author dramage
 */
trait OpSolveMatrixBy extends OpType;


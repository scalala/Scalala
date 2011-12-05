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

import scala.annotation.implicitNotFound;
import scala.collection.generic.CanBuildFrom;

import scala.collection.mutable.HashMap;

object BinaryOpRegistry {
  val resultTypes =
    HashMap[(Manifest[_],Manifest[_],Manifest[_]),Manifest[_]]();
  
  val ops =
    HashMap[(Manifest[_],Manifest[_],Manifest[_]),BinaryOp[_,_,_,_]]();
  
  def register[A,B,O<:OpType,To](implicit gen : BinaryOp[A,B,O,To], a : Manifest[A], b : Manifest[B], op : Manifest[O], to : Manifest[To]) = {
    resultTypes((a,b,op)) = to
    ops((a,b,op)) = gen
  }
  
  def getResultType[A,B,O<:OpType](implicit a : Manifest[A], b : Manifest[B], op : Manifest[O]) =
    resultTypes((a,b,op));
  
  def getBinaryOp[A,B,O<:OpType,To](implicit a : Manifest[A], b : Manifest[B], op : Manifest[O], to : Manifest[To]) : BinaryOp[A,B,O,To] = {
    require(getResultType[A,B,O] == to, "Mismatched result type: "+to);
    ops((a,b,op)).asInstanceOf[BinaryOp[A,B,O,To]]
  }
}


///*
// * Distributed as part of Scalala, a linear algebra library.
// *
// * Copyright (C) 2008- Daniel Ramage
// *
// * This library is free software; you can redistribute it and/or
// * modify it under the terms of the GNU Lesser General Public
// * License as published by the Free Software Foundation; either
// * version 2.1 of the License, or (at your option) any later version.
// *
// * This library is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// * Lesser General Public License for more details.
// *
// * You should have received a copy of the GNU Lesser General Public
// * License along with this library; if not, write to the Free Software
// * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
// */
//package scalala;
//package collection;
//package generic;
//
//import domain._;
//
//abstract class DomainMapCompanion
//[DD[A]<:IterableDomain[A], From[A,B]<:DomainMap[A,B,DD[A]],
// +ToMutable[A,B]<:MutableDomainMap[A,B,DD[A]],
// +ToSlice[A1,A2,B]<:DomainMapSlice[A1,DD[A1],A2,SetDomain[A2],B,From[A1,B]],
// +ToSliceSeq[A,B]<:DomainMapSliceSeq[A,DD[A],B,From[A,B]]
//]
//{ self =>
//
//  protected def createMutable[A,B](domain : DD[A], initial : B) : ToMutable[A,B];
//  protected def createSlice[A1,A2,B](from : From[A1,B], keymap : scala.collection.Map[A2,A1]) : ToSlice[A1,A2,B];
//  protected def createSliceSeq[A,B](from : From[A,B], keys : Seq[A]) : ToSliceSeq[A,B];
//
//  implicit def canMapValues[A,B,O]
//  (implicit default : DefaultValue[O]) =
//  new DomainMapCanBuildFrom[From[A,B], A, O, DD[A], ToMutable[A,O]] {
//    override def apply(from : From[A,B], domain : DD[A]) =
//      DomainMapBuilder(self.createMutable(domain, default.value));
//  }
//
//  implicit def canSliceFrom[A1,A2,B] =
//  new DomainMapCanSliceFrom[From[A1,B], A1, DD[A1], A2, B, ToSlice[A1,A2,B]] {
//    override def apply(from : From[A1,B], keymap : scala.collection.Map[A2,A1]) =
//      self.createSlice[A1,A2,B](from, keymap);
//  }
//
//  implicit def canSliceSeqFrom[A,B] =
//  new DomainMapCanSliceSeqFrom[From[A,B], A, DD[A], B, ToSliceSeq[A,B]] {
//    override def apply(from : From[A,B], keys : Seq[A]) =
//      self.createSliceSeq[A,B](from, keys);
//  }
//
//}

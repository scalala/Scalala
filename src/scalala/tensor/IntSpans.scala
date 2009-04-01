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
package scalala.tensor.spans;

object IntSpans {
  /** Promotes a range to Range1D. */
  implicit def iRange1D(range : Range) = {
    Range1D(range.start, range.end + (if (range.isInstanceOf[Range.Inclusive]) 1 else 0));
  }
    
  /** Promotes a Range to RangeSet1D. */
  implicit def iRangeSet1D(range : Range) =
    new RangeSet1D(List(range));
    
  /** Promotes a Range1D to a RangeSet1D. */
  implicit def iRangeSet1D(range : Range1D) =
    new RangeSet1D(List(range));

  /** Adds to and until to (Int,Int) tuples. */
  case class Range2DPromoter(start : (Int,Int)) {
    def until (end : (Int,Int)) = SquareRange2D(start, end);
    def to (end : (Int,Int)) = SquareRange2D(start, (end._1+1, end._2+1));
  }
    
  /** Promotes a tuple to a Range2DPromoter. */
  implicit def iRange2DPromoter(start : (Int,Int)) = Range2DPromoter(start);
    
  /** Promotes a Range2D to a RangeSet2D. */
  implicit def iRangeSet2D(range : Range2D) =
    new RangeSet2D(List(range));
}
  
  /**
   * Represents a continuous range of integers from start (inclusive)
   * to end (exclusive).  This is a simpler and well behaved version of
   * scala.Range for use in a RangeSet1D.
   */
  case class Range1D(start : Int, end : Int) extends Collection[Int] with Ordered[Range1D] {
    if (start > end) throw new Predef.IllegalArgumentException("start "+start+" > end "+end);
    
    override def size = end - start;

    def contains(i : Int) = i >= start && i < end;
    
    def mergable(that : Range1D) = {
      (this.start <= that.start && this.end >= that.start) ||
      (that.start <= this.start && that.end >= this.start);
    }
    
    def merge(that : Range1D) = {
      if (!mergable(that)) throw new IllegalArgumentException;
      Range1D(Math.min(this.start, that.start), Math.max(this.end, that.end));
    }
    
    override def elements = (start until end).elements;
    
    override def compare(that : Range1D) = {
      val starts = this.start compare that.start;
      val ends   = this.end compare that.end;
      if (starts != 0) starts else ends;
    }
  }
  
  object Range1D {
    /**
     * Compacts the given list of ranges into the minimum set that can
     * completely represent all numbers in the range.  The returned set
     * will always be non-overlapping.
     */
    def compact(ranges : List[Range1D]) : List[Range1D] = {
      def compact_(ranges : List[Range1D]) : List[Range1D] = {
        ranges match {
          case Nil => Nil;
          case first :: second :: rest if (first mergable second) =>
            compact_((first merge second) :: rest);
          case first :: rest => first :: compact_(rest);
        }
      }
      
      compact_(ranges.toList.sort(_.start < _.start));
    }
  }
  
  /**
   * A set of Ints represented compactly by a set of numerical ranges.
   * 
   * @author dramage
   */
  class RangeSet1D(inRanges : List[Range1D]) extends Set[Int] {
    // ranges sorted by starting position
    val ranges : List[Range1D] = Range1D.compact(inRanges);
    
    override def size = ranges.map(_.size).foldLeft(0)(_+_);
      
    override def contains(x : Int) = 
      ranges.elements.map(_.contains(x)).contains(true);
      
    override def elements =
      ranges.elements.flatMap(_.elements);
    
    override def ++(elems : Iterable[Int]) : Set[Int] = {
      elems match {
        case other : RangeSet1D => {
          new RangeSet1D(this.ranges ++ other.ranges);
        }
        case _ => super.++(elems);
      }
    }
    
    override def -(elem : Int) = throw new UnsupportedOperationException();
    override def +(elem : Int) = throw new UnsupportedOperationException();
    override def empty[B] = Set[B]();
    
    override def equals(other : Any) = {
      other match {
        case that : RangeSet1D => this.ranges == that.ranges;
        case _ => super.equals(other);
      }
    }
  }
  
  /**
   * A set of (Int,Int) tuples represented compactly with a set
   * of Range2D objects.
   * 
   * @author dramage
   */
  abstract case class Range2D() extends Collection[(Int,Int)] {
    /** Returns true if the given element is contained in this range. */
    def contains(i : Int, j : Int) : Boolean;
    
    /** Returns true if this range contains the other one. */
    def contains(other : Range2D) : Boolean =
      other.elements.map(tup => !this.contains(tup)).contains(true);
    
    /** Returns true if the given element is contained in this range. */
    final def contains(tup : (Int,Int)) : Boolean = contains(tup._1, tup._2);
  }
  
  /**
   * All numbers within the given square range from, i.e. from
   * [(start._1,end._1), (start._2, end._2)).
   */
  case class SquareRange2D(start : (Int,Int), end : (Int,Int)) extends Range2D {
    if (start._1 >= end._1 || start._2 >= end._2) throw new Predef.IllegalArgumentException;
    
    override def contains(i : Int, j : Int) =
      i >= start._1 && i < end._1 && j >= start._2 && j < end._2;
    
    override def contains(other : Range2D) = other match {
      case that : SquareRange2D =>
        (this.start._1 <= that.start._1 && this.start._2 <= that.start._2 &&
         this.end._1 >= that.end._1 && this.end._2 >= that.end._2);
      case _ => super.contains(other);
    }
    
    override def size =
      (end._1 - start._1) * (end._2 - start._2);
    
    override def elements = {
      for (i <- (start._1 until end._1).elements;
           j <- (start._2 until end._2).elements)
        yield (i,j);
    }
  }
  
  /** A set of (Int,Int)'s represented compactly by a set of Range2D's. */
  class RangeSet2D(inRanges : List[Range2D]) extends Set[(Int,Int)] {
    val ranges =
      (for (i <- 0 until inRanges.length;
           if !(0 until i).map(j => inRanges(j).contains(inRanges(i))).contains(true))
        yield inRanges(i)).toList;
    
    override lazy val size = elements.map(tup => 1).foldLeft(0)(_+_);
    
    override def contains(x : (Int,Int)) = 
      ranges.elements.map(_.contains(x)).contains(true);
      
    override def elements =
      for (i <- (0 until ranges.size).elements;
           e <- ranges(i).elements;
           if !(0 until i).map(j => ranges(j).contains(e)).contains(true))
        yield e;
    
    override def -(elem : (Int,Int)) = throw new UnsupportedOperationException();
    override def +(elem : (Int,Int)) = throw new UnsupportedOperationException();
    override def empty[B] = Set[B]();
  }

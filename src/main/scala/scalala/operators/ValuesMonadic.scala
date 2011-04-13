package scalala.operators

import scalala.generic.collection.CanMapValues

/**
 *
 * Represents objects that can have their values mapped.
 * @author dlwh
 */
trait ValuesMonadic[+This,V] {
  def repr: This;
  def map[TT>:This,O,That](fn : V => O)
  (implicit bf : CanMapValues[TT, V, O, That]) : That =
    bf.map(repr,fn);

}

trait HasValuesMonadic[+This,V] {
  def values: ValuesMonadic[This,V];
}

object HasValuesMonadic {
  implicit def arrayHasValues[V](arr: Array[V]): HasValuesMonadic[Array[V], V]  = new HasValuesMonadic[Array[V],V] {
    def values = new ValuesMonadic[Array[V],V] {
      def repr = arr;
    }
  }
}

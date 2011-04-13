package scalala.operators

/**
 * The InnerProduct (dot product) of two objects.
 * @author dlwh
 */
trait InnerProduct[-T1,-T2,+Result] extends ((T1,T2)=>Result) {
  def apply(t1: T1, t2: T2):Result;
}
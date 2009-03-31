package scalala.collection;

/**
 * An entry that mutates upon calls to next.
 * 
 * @author dramage
 */
abstract class MutatingEntry[A,B] {
  def index : A;
  def get : B;
  def set(value : B) : Unit;
  
  def moveToFirst : Unit;
  def moveToNext : Unit;
  def hasNext : Boolean;
  
  def tuple = (index,get);
  
  override def toString : String =
    index + " " + get;

  override def equals(other : Any) = {
    other match {
      case that : MutatingEntry[_,_] => this eq that;
      case _ => false;
    }
  }
  
  override def hashCode =
    throw new UnsupportedOperationException("Entries inherently mutate");
}

/**
 * An iterator around a single MutatingEntry.
 * 
 * @author dramage
 */
class MutatingEntryIterator[A,B](entry : MutatingEntry[A,B]) extends Iterator[MutatingEntry[A,B]] {
  override def hasNext = entry.hasNext;
  override def next = { entry.moveToNext; entry; }
}

/**
 * Provides support for iterating the active elements of a 
 */
trait MutatingEntries[A,B] extends MutablePartialMap[A,B] {
  def activeEntry : MutatingEntry[A,B];
  
  def activeMutatingElements = new MutatingEntryIterator(activeEntry);
  
  override def activeElements : Iterator[(A,B)] = new Iterator[(A,B)] {
    val iter = activeMutatingElements;
    override def hasNext = iter.hasNext;
    override def next = iter.next.tuple;
  }
  
  override def activeKeys : Iterator[A] = new Iterator[A] {
    val iter = activeMutatingElements;
    override def hasNext = iter.hasNext;
    override def next = iter.next.index;
  }
  
  override def activeValues : Iterator[B] = new Iterator[B] {
    val iter = activeMutatingElements;
    override def hasNext = iter.hasNext;
    override def next = iter.next.get;
  }
}

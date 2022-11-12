class Nfa[A](/* TODO : define the constructor params */) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = ??? // TODO implement map

  def next(state:A, c: Char): Set[A] = ??? // TODO implement next

  def accepts(str: String): Boolean = ??? // TODO implement accepts

  def getStates : Set[A] = ??? // TODO implement getStates

  def isFinal(state: A): Boolean = ???  // TODO implement isFinal
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {
  def fromPrenex(str: String): Nfa[Int] = ??? // TODO implement Prenex -> Nfa transformation.

  // You can add more methods to this object
}
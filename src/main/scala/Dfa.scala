class Dfa[A] (/* TODO : define the constructor params */){

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = ??? // TODO implement map

  def next(state:A, c: Char): A = ??? // TODO implement next

  def accepts(str: String): Boolean = ??? // TODO implement accepts

  def getStates : Set[A] = ??? // TODO implement getStates

  def isFinal(state: A): Boolean = ???  // TODO implement isFinal
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromPrenex(str: String): Dfa[Int] = ??? // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa

  // You can add more methods to this object
}

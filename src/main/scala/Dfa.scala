class Dfa[A] (val start: A, val sinkState:A, val finalStates: Set[A], val transitions: Map[(A, Char), A]) {
  def map[B](f: A => B) : Dfa[B] = {
    // map the states of the DFA to new states using the function f
    // the new DFA should have the same transitions as the old DFA
    // the new DFA should have the same final states as the old DFA
    // the new DFA should have the start state that is the result of applying f to the start state of the old DFA
    new Dfa(f(start), f(sinkState), finalStates.map(f), transitions.map{case ((a, c), b) => (f(a), c) -> f(b)})
  }

  def next(state:A, c: Char): A = {
    // get the next state of the DFA by applying the transition function
    // if there is no transition for the given state and character, return a sink state
    transitions.getOrElse((state, c), sinkState)
  }

  def accepts(str: String): Boolean = {
    val state = str.foldLeft(start)((state, c) => {next(state, c)} )
    isFinal(state)
  }

  def getStates : Set[A] = {
    // get all states of the DFA
    transitions.keys.map(_._1).toSet
  }

  def isFinal(state: A): Boolean = {
    // check if the state is in the final states set
    finalStates.contains(state)
  }

  override def toString: String = {
    "Dfa(" + start + ", " + finalStates + ", " + transitions + ")"
  }
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromPrenex(str: String): Dfa[Int] = {
    // first transform to dfaAux which have the states represented as a set of states
    val dfaAux = DfaAux.fromPrenex(str)

    // then transform to dfa
    // create a map that maps the states of the aux DFA to new states
    val stateMap = dfaAux.getStates.zipWithIndex.toMap
    // create a new DFA with the new states
    new Dfa(stateMap(dfaAux.start),
          -1,
          dfaAux.finalStates.map(s => stateMap(s)),
          dfaAux.transitions.map{case ((s, c), s2) => (stateMap(s), c) -> stateMap(s2)})
  }

  // You can add more methods to this object
}

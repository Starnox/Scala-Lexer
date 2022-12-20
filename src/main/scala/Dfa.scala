import Dfa.fromDfaAux

class Dfa[A] (val start: A, val sinkState:A, val finalStates: Set[A], val transitions: Map[(A, Char), A]) {

  /**
   * Map the states of this NFA to a new set of states using the function f
   * @param f the function to map the states
   * @tparam B the type of the new states
   * @return a new NFA with the states mapped using the function f
   */
  def map[B](f: A => B) : Dfa[B] = {
    new Dfa(f(start), f(sinkState), finalStates.map(f), transitions.map{case ((a, c), b) => (f(a), c) -> f(b)})
  }

  /**
   * Get the next state of the DFA by applying the transition function
   * If there is no transition for the given state and character, return a sink state
   * @param state the current state
   * @param c the character
   * @return the next state or the sink state
   */
  def next(state:A, c: Char): A = {
    transitions.getOrElse((state, c), sinkState)
  }

  /**
   * Check if the DFA accepts the string str
   * @param str the string to check
   * @return true if the DFA accepts the string, false otherwise
   */
  def accepts(str: String): Boolean = {
    val state = str.foldLeft(start)((state, c) => {next(state, c)} ) // simple foldLeft to go through the string
    isFinal(state)
  }

  /**
   * Get all states of the DFA
   * @return a set of all states
   */
  def getStates : Set[A] = {
    transitions.keys.map(_._1).toSet
  }

  /**
   * Check if the state is in the final states set thus being a final state
   * @param state the state to check
   * @return true if the state is in the final states set, false otherwise
   */
  def isFinal(state: A): Boolean = {
    finalStates.contains(state)
  }

  override def toString: String = {
    "Dfa(" + start + ", " + finalStates + ", " + transitions + ")"
  }
}

object Dfa {

  /**
   * Convert a string to a DFA by first converting it to a aux DFA and then converting the aux DFA to a DFA
   * @param str the string to convert
   * @return a DFA
   */
  def fromPrenex(str: String): Dfa[Int] = {
    fromDfaAux(DfaAux.fromPrenex(str))
  }

  /**
   * Convert an aux DFA to a DFA
   * @param dfaAux the aux DFA to convert
   * @return a DFA
   */
  def fromDfaAux(dfaAux: DfaAux[Int]): Dfa[Int] = {
    val stateMap = dfaAux.getStates.zipWithIndex.toMap // maps the states of the aux DFA to new states
    // create a DFA with the new states
    new Dfa(stateMap(dfaAux.start),
          -1,
          dfaAux.finalStates.map(s => stateMap(s)),
          dfaAux.transitions.map{case ((s, c), s2) => (stateMap(s), c) -> stateMap(s2)})
  }

  /**
   * Convert an aux DFA to a DFA maintaining the properties of the final states
   * @param dfaAux the aux DFA to convert
   * @param properties the properties of the final states
   * @return a DFA
   */
  def fromDfaAuxWithProperties(dfaAux: DfaAux[Int], properties: Map[Int, String]): (Dfa[Int], Map[Int,Set[String]]) = {
    val stateMap = dfaAux.getStates.zipWithIndex.toMap // maps the states of the aux DFA to new states
    // Add the properties of the states to the index
    val newProperties = stateMap.map{case (s, i) => i -> s.flatMap(properties.get)}
    // create a DFA with the new states
    (new Dfa(stateMap(dfaAux.start),
          -1,
          dfaAux.finalStates.map(s => stateMap(s)),
          dfaAux.transitions.map{case ((s, c), s2) => (stateMap(s), c) -> stateMap(s2)}), newProperties)
  }

  def convertToMinDfa(dfa: Dfa[Int]) : Dfa[Int] = {
    // Brzozowski minimisation
    val dfaAux = DfaAux(dfa)
    fromDfaAux(DfaAux(fromDfaAux(dfaAux.reverse())).reverse())
  }

}

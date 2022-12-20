import scala.collection.mutable

class DfaAux[A] (val start: Set[A], val finalStates: Set[Set[A]], val transitions: Map[(Set[A], Char), Set[A]]) {

  /**
   * Get all states of the DFA
   * @return a set of all states
   */
  def getStates : Set[Set[A]] = {
    transitions.keys.map(_._1).toSet ++ transitions.values.toSet ++ finalStates ++ Set(start)
  }

  /**
   * Reverse the given DFA
   * @return the new reversed DFA
   */
  def reverse(): DfaAux[A] = {
    var reversedTransitions = Map[(Set[A], Char), Set[A]]()
    for (t <- transitions) {
      reversedTransitions = reversedTransitions + ((t._2, t._1._2) -> (reversedTransitions.getOrElse((t._2, t._1._2), Set[A]()) ++ t._1._1))
    }
    new DfaAux(finalStates.flatten, Set(start), reversedTransitions)
  }

  override def toString: String = {
    "DfaAux(" + start + ", " + finalStates + ", " + transitions + ")"
  }
}

object DfaAux {
  /**
   * Convert a NFA to an aux DFA
   * @param nfa the NFA to convert
   * @return the aux DFA
   */
  def fromNfa(nfa: Nfa[Int]) : DfaAux[Int] = {
    val nfaStart = nfa.start
    val nfaFinalStates = nfa.finalStates
    val alphabet = nfa.getAlphabet - 'ε'

    // create a queue and add the start state to it and epsilon transitions
    val queue = new mutable.Queue[Set[Int]]()
    queue.enqueue(nfa.getEpsilonClosure(nfaStart))

    // init
    val dfaStart = queue.head
    var dfaTransitions = Map[(Set[Int], Char), Set[Int]]()
    var finalStates = Set[Set[Int]]()
    var allStates = Set[Set[Int]]()

    // as long as the queue is not empty, create a new DFA and add the new set of states to the queue
    var visited = Set[Set[Int]]()
    visited += dfaStart
    nfa.epsilonClosureCache = nfa.preCalculateEpsilonCLosure()
    while (queue.nonEmpty) {
      val statesSet = queue.dequeue()
      allStates += statesSet

      var newTransitions = alphabet.map(c => (statesSet, c) -> statesSet.flatMap(s => nfa.next(s, c))).toMap
      // add epsilon closure to the transitions
      newTransitions = newTransitions.map(t => t._1 -> t._2.flatMap(s => nfa.epsilonClosureCache.getOrElse(s, Set(s))))
      // filter out the transitions that point to the sink state
      val filteredTransitions = newTransitions.filter(_._2.nonEmpty)
      // add the new transitions to the queue if they aren't in the visited set
      filteredTransitions.values.foreach(s => if (!visited.contains(s)) {
        queue.enqueue(s)
        visited += s
      })
      dfaTransitions = dfaTransitions ++ filteredTransitions
    }

    // create final states
    allStates.foreach(s => if (s.exists(nfaFinalStates.contains)) finalStates += s)

    new DfaAux(dfaStart, finalStates, dfaTransitions)

  }

  /**
   * Convert a string to an aux DFA
   * @param str the string to convert
   * @return the aux DFA
   */
  def fromPrenex(str: String): DfaAux[Int] = {
    val nfa = Nfa.fromPrenex(str)
    fromNfa(nfa)
  }

  /**
   * Convert a dfa to an aux dfa
   * @param dfa the dfa to convert
   * @return the aux DFA
   */
  def apply [A](dfa: Dfa[A]) : DfaAux[A] = {
    new DfaAux(Set(dfa.start), Set(dfa.finalStates), dfa.transitions.map(t => (Set(t._1._1), t._1._2) -> Set(t._2)))
  }
}
import scala.collection.mutable

class DfaAux[A] (val start: Set[A], val finalStates: Set[Set[A]], val transitions: Map[(Set[A], Char), Set[A]]) {
  override def toString: String = {
    "DfaAux(" + start + ", " + finalStates + ", " + transitions + ")"
  }

  def getStates : Set[Set[A]] = {
    // get all states of the DFA
    transitions.keys.map(_._1).toSet ++ transitions.values.toSet ++ finalStates ++ Set(start)
  }
}

object DfaAux {
  def fromNfa(nfa: Nfa[Int]) : DfaAux[Int] = {
    val nfaStart = nfa.start
    val nfaFinalStates = nfa.finalStates
    val alphabet = nfa.getAlphabet - 'ε'

    // create a queue and add the start state to it and epsilon transitions
    val queue = new mutable.Queue[Set[Int]]()
    queue.enqueue(Set(nfaStart) ++ nfa.next(nfaStart, 'ε'))

    val dfaStart = queue.head
    var dfaTransitions = Map[(Set[Int], Char), Set[Int]]()
    var finalStates = Set[Set[Int]]()
    var allStates = Set[Set[Int]]()
    // as long as the queue is not empty, create a new DFA and add the transitions to the queue
    // and add the transitions to the queue if they haven't been processed yet
    var visited = Set[Set[Int]]()
    // add the start state to the visited set
    visited += queue.head
    while (queue.nonEmpty) {
      val statesSet = queue.dequeue()
      allStates += statesSet
      val newTransitions = alphabet.map(c => (statesSet, c) -> statesSet.flatMap(s => nfa.next(s, c))).toMap
      // filter out the transitions that point to the empty set
      val filteredTransitions = newTransitions.filter(_._2.nonEmpty)
      // add the new transitions to the queue if they aren't in the visited set
      filteredTransitions.values.foreach(s => if (!visited.contains(s)) {
        queue.enqueue(s)
        visited += s
      })
      dfaTransitions = dfaTransitions ++ filteredTransitions
    }
    // add the final states to the final states set
    allStates.foreach(s => if (s.exists(nfaFinalStates.contains)) finalStates += s)

    new DfaAux(dfaStart, finalStates, dfaTransitions)

  }

  def fromPrenex(str: String): DfaAux[Int] = {
    val nfa = Nfa.fromPrenex(str)
    fromNfa(nfa)
  }
}
import scala.collection.mutable
import scala.collection.mutable.Stack
// NFA (q0, F, δ)
// q0: initial state
// F: set of final states
// δ: transition function -> maps (state, character) to a set of states -> non-deterministic

class Nfa[A](val start: A, val finalStates: Set[A], val transitions: Map[(A, Char), Set[A]]) {

  // applies the f function to all states in the NFA and returns a new NFA with the new states
  def map[B](f: A => B) : Nfa[B] = {
    val newStart = f(start)
    val newFinalStates = finalStates.map(f)
    val newTransitions = transitions.map{case ((a, c), b) => (f(a), c) -> b.map(f)}
    new Nfa(newStart, newFinalStates, newTransitions)
  }

  def next(state:A, c: Char): Set[A] = {
    // create variable to hold the result
    var result = transitions.getOrElse((state, c), Set())
    // add epsilon transitions
    val epsilonTransitions = transitions.getOrElse((state, 'ε'), Set())
    // apply the character transition to all states that can be reached by epsilon transitions
    result = result ++ epsilonTransitions.flatMap(next(_, c)) ++ result.flatMap(next(_, 'ε'))
    // add the epsilon transitions that can be reached by the character transition
    //result = result ++ result.flatMap(next(_, 'ε'))
    result
  }

  def accepts(str: String): Boolean = {
    // Check if the NFA accepts the string str by simulating the NFA on the string with epsilon transitions
    // and checking if the final state is in the set of final states
    if (finalStates.contains(start))
      return true

    var states = Set(start)

    if(str == "")
      states = next(start, 'ε')
    else
      states = str.foldLeft(Set(start))((states, c) => {states.flatMap(next(_, c)) } )

    // check if the set of states that we can get by following the transitions
    // for each character in the string contains a final state
    // very similar to the last exercise in the first lab
    states.intersect(finalStates).nonEmpty
  }

  def getStates : Set[A] = {
    // get all states of the NFA
    transitions.keys.map(_._1).toSet
  }

  def getLastState: A = {
    // get the last state of the NFA
    finalStates.last
  }

  def isFinal(state: A): Boolean = {
    // check if the state is in the final states set
    finalStates.contains(state)
  }

  override def toString: String = {
    // print the NFA in a readable format
    val sb = new StringBuilder
    sb.append("NFA")

    // print the initial state
    sb.append(s" (q0 = $start, ")
    // print the transitions
    sb.append("δ = {")
    transitions.foreach{case ((a, c), b) => sb.append(s"($a, $c) -> $b, ")}
    sb.append("}, ")
    // print the final states
    sb.append(s"F = $finalStates)")
    sb.append("\n")
    sb.toString
  }
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {

  def getNewState(nfa: Nfa[Int]): Int = {
    // get the last state of the NFA and increment it by 1
    nfa.getLastState + 1
  }

  def fromUnion(nfaA: Nfa[Int], nfaB: Nfa[Int]): Nfa[Int] = {
    // Create a new NFA that accepts the union of the languages of nfaA and nfaB
    // The new NFA should have a new start state, new final states and new transitions
    // The new start state should have transitions to the start states of nfaA and nfaB

    // get the new start state that is the maximum of the last states of nfaA and nfaB
    val newStart = getNewState(nfaA) max getNewState(nfaB)
    val newEnd = newStart + 1

    var transitions = Map[(Int, Char), Set[Int]]()

    // add the transitions from the new start state to the start states of nfaA and nfaB
    transitions += (newStart, 'e') -> Set(nfaA.start, nfaB.start)
    // add the transitions from the final states of nfaA and nfaB to the new end state
    transitions += (nfaA.getLastState, 'e') -> Set(newEnd)
    transitions += (nfaB.getLastState, 'e') -> Set(newEnd)

    // add the transitions from nfaA and nfaB to the new transitions map
    transitions ++= nfaA.transitions
    transitions ++= nfaB.transitions

    // create the new NFA
    new Nfa[Int](newStart, Set(newEnd), transitions)
  }

  def fromPrenex(str: String): Nfa[Int] = {
    // create a new NFA from a prenex expression
    // example of a prenex polish expression: CONCAT UNION a b UNION c d => (a U b)(c U d)
    // Split string into tokens which will be in polish notation
    var tokens = str.split(" ").toList

    if (tokens.isEmpty) {
      // if the string is empty, return an NFA that accepts the empty string
      return new Nfa[Int](0, Set(0), Map())
    }

    val stack = mutable.Stack[String]()

    var counter = 0
    def createNFA(): Nfa[Int] = {
      // Create the transitions
      var transitions = Map[(Int, Char), Set[Int]]()
      // Create the final states
      var finalStates = Set[Int]()


      if (tokens.isEmpty) {
        // if the tokens list is empty, return an NFA that accepts the empty string
        return new Nfa[Int](counter, Set(counter), Map())
      }

      stack.push(tokens.head)
      tokens = tokens.tail
      // get the top of the stack and add the next token to the stack
      val token = stack.top

      // if the token is epsilon, return the NFA with the current counter as the start state and the final state
      if (token == "eps") {
        stack.pop()
        return new Nfa[Int](counter, Set(counter), transitions)
      }
      // check if the token is a character
      else if (token.length == 1) {
        // create 2 new states
        val q0 = counter
        val q1 = counter + 1
        counter += 2

        // add the transition
        transitions = transitions + ((q0, token.charAt(0)) -> Set(q1))
        // add the final state
        finalStates = finalStates + q1
        stack.pop()
        return new Nfa[Int](q0, finalStates, transitions)
      }
      else if (token == "UNION") {
        // create 2 new states
        val q0 = counter
        val q1 = counter + 1
        counter += 2

        // create the NFA for the first operand
        val nfa1 = createNFA()
        // create the NFA for the second operand
        val nfa2 = createNFA()

        // return the NFA for the union
        // add the transitions
        transitions = transitions ++ nfa1.transitions ++ nfa2.transitions
        // add the transitions from the new start state to the start states of nfa1 and nfa2
        transitions = transitions + ((q0, 'ε') -> Set(nfa1.start, nfa2.start))

        // add the transitions from all final states of nfa1 and nfa2 to the new end state
        transitions = transitions + ((nfa1.getLastState, 'ε') -> Set(q1))
        transitions = transitions + ((nfa2.getLastState, 'ε') -> Set(q1))

        // add the final state
        finalStates = finalStates + q1
        stack.pop()
        return new Nfa[Int](q0, finalStates, transitions)
      }
      else if (token == "CONCAT") {
        // create the NFA for the first operand
        val nfa1 = createNFA()
        // create the NFA for the second operand
        val nfa2 = createNFA()

        // add epsilon transition between the final state of the first NFA and the start state of the second NFA
        transitions = transitions + ((nfa1.getLastState, 'ε') -> Set(nfa2.start))
        // add the transitions
        transitions = transitions ++ nfa1.transitions
        transitions = transitions ++ nfa2.transitions

        // add the final state
        finalStates = nfa2.finalStates
        stack.pop()
        return new Nfa[Int](nfa1.start, finalStates, transitions)
      }
      else if (token == "STAR") {
        // create 2 new states
        val q0 = counter
        val q1 = counter + 1
        counter += 2

        // create the NFA for the operand
        val nfa = createNFA()

        // add the transitions
        transitions = transitions ++ nfa.transitions
        // add the transitions from the new start state to the start state of nfa and the new end state
        transitions = transitions + ((q0, 'ε') -> Set(nfa.start, q1))
        // add the transitions from the final state of nfa to the new end state and to the start state of nfa
        transitions = transitions + ((nfa.getLastState, 'ε') -> Set(q1, nfa.start))

        // add the final state
        finalStates = finalStates + q1
        stack.pop()
        return new Nfa[Int](q0, finalStates, transitions)
      }
      else if (token == "PLUS") {
        // create 2 new states
        val q0 = counter
        val q1 = counter + 1
        counter += 2

        // create the NFA for the operand
        val nfa = createNFA()

        // add the transitions
        transitions = transitions ++ nfa.transitions
        // add the transitions from the new start state to the start state of nfa and the new end state
        transitions = transitions + ((q0, 'ε') -> Set(nfa.start))
        // add the transitions from the final state of nfa to the new end state and to the start state of nfa
        transitions = transitions + ((nfa.getLastState, 'ε') -> Set(q1, nfa.start))

        // add the final state
        finalStates = finalStates + q1
        stack.pop()
        return new Nfa[Int](q0, finalStates, transitions)
      }
      else if (token == "MAYBE") {
        // union between the NFA for the operand and the NFA for epsilon
        fromUnion(createNFA(), fromPrenex("eps"))
      }
      else if (token == "void") {
        return new Nfa[Int](0, Set[Int](), transitions)
      }
      else {
        throw new IllegalArgumentException("Invalid token: " + token)
      }

      new Nfa[Int](0, Set(0), transitions)
    }
    createNFA()
  }

  // You can add more methods to this object
}
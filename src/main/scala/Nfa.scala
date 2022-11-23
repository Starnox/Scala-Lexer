import scala.collection.mutable
// NFA (q0, F, δ)
// q0: initial state
// F: set of final states
// δ: transition function -> maps (state, character) to a set of states -> non-deterministic
class Nfa[A](val start: A, val finalStates: Set[A], val transitions: Map[(A, Char), Set[A]]) {

  /**
   * Map the states of this NFA to a new set of states using the function f
   * @param f the function to map the states
   * @tparam B the type of the new states
   * @return a new NFA with the states mapped using the function f
   */
  def map[B](f: A => B) : Nfa[B] = {
    val newStart = f(start)
    val newFinalStates = finalStates.map(f)
    val newTransitions = transitions.map{case ((a, c), b) => (f(a), c) -> b.map(f)}
    new Nfa(newStart, newFinalStates, newTransitions)
  }

  /**
   * Get the alphabet of the NFA
   * @return a set of characters that are in the alphabet
   */
  def getAlphabet: Set[Char] = transitions.keySet.map(_._2)

  /**
   * Get the next states of the NFA by applying the transition function including the epsilon transitions
   * @param state the current state
   * @param c the character
   * @return a set of next states
   */
  def next(state:A, c: Char): Set[A] = {
    val epsilonTransitions = transitions.getOrElse((state, 'ε'), Set()) // get the epsilon transitions
    var result = transitions.getOrElse((state, c), Set()) // get the transitions for the character
    result ++= result.flatMap(next(_, 'ε')) // add the epsilon transitions for the transitions for the character
    result ++= epsilonTransitions.flatMap(next(_, c)) // apply the character transition to all states that can be reached by epsilon transitions
    result
  }

  /**
   * Check if the NFA accepts the string str
   * @param str the string to check
   * @return true if the NFA accepts the string, false otherwise
   */
  def accepts(str: String): Boolean = {
    var states = Set(start)

    if(str == "")
      states = states ++ next(start, 'ε')
    else
      states = str.foldLeft(Set(start))((states, c) => {states.flatMap(next(_, c)) } ) // all possible states after c

    states.intersect(finalStates).nonEmpty
  }

  /**
   * Get all states of the NFA
   * @return a set of all states
   */
  def getStates : Set[A] = {
    transitions.keys.map(_._1).toSet
  }

  /**
   * Get last state of the NFA
   * @return the last state
   */
  def getLastState: A = {
    finalStates.last
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
    // print the NFA in a readable format
    val sb = new mutable.StringBuilder
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

object Nfa {
  /**
   * This method takes a string in prenex normal form and returns an NFA that accepts the language of the string
   * @param str the string in prenex normal form
   * @return an NFA that accepts the language of the string
   */
  def fromPrenex(str: String): Nfa[Int] = {

    // Split string into tokens
    var tokens = Helpers.customSplit(str)
    var counter = 0

    def createNFA(): Nfa[Int] = {
      // if the tokens list is empty, return an NFA that accepts the empty string
      if (tokens.isEmpty) {
        return new Nfa[Int](counter, Set(counter), Map())
      }

      // get the next token and remove it from the list
      val token = tokens.head
      tokens = tokens.tail

      val prevState = counter
      token match {
        case "eps" => {
          new Nfa[Int](counter, Set(counter), Map())
        }
        case "' '" => {
          counter += 2
          fromCharacter(' ', prevState)
        }
        case "UNION" => {
          counter += 2
          fromUnion(createNFA(), createNFA(), prevState)
        }
        case "CONCAT" => {
          counter += 2
          fromConcat(createNFA(), createNFA(), prevState)
        }
        case "STAR" => {
          counter += 2
          fromStar(createNFA(), prevState)
        }

        case "PLUS" => {
          counter += 2
          fromPlus(createNFA(), prevState)
        }
        case "MAYBE" => fromMaybe(createNFA())
        case "void" => new Nfa[Int](0, Set[Int](), Map())
        case token => {
          // if the token is a character, return the NFA with the current counter as the start state and the final state
          if (token.length == 1) {
            counter += 2
            return fromCharacter(token.charAt(0), prevState)
          }
          throw new IllegalArgumentException("Invalid token: " + token)
        }
      }
    }

    createNFA()
  }

  /**
   * This method takes a character and returns an NFA that accepts the language of the character
   * @param c the character
   * @param stateCounter the counter for the states
   * @return an NFA that accepts the language of the character
   */
  def fromCharacter(c: Char, stateCounter: Int): Nfa[Int] = {
    val start       = stateCounter
    val finalStates = Set(stateCounter + 1)
    val transitions = Map((start, c) -> finalStates)

    new Nfa(start, finalStates, transitions)
  }

  /**
   * Create a new NFA that accepts the union of the languages of nfaA and nfaB
   * @param nfaA the first NFA
   * @param nfaB the second NFA
   * @param stateCounter the counter for the states
   * @return a new NFA that accepts the union of the languages of nfaA and nfaB
   */
  def fromUnion(nfaA: Nfa[Int], nfaB: Nfa[Int], stateCounter: Int): Nfa[Int] = {
    val newStart  = stateCounter
    val newEnd    = stateCounter + 1

    var transitions = nfaA.transitions ++ nfaB.transitions
    // add the transitions from the new start state to the start states of nfaA and nfaB
    transitions += (newStart, 'ε') -> Set(nfaA.start, nfaB.start)
    // add the transitions from the final states of nfaA and nfaB to the new end state
    transitions += (nfaA.getLastState, 'ε') -> Set(newEnd)
    transitions += (nfaB.getLastState, 'ε') -> Set(newEnd)

    val finalState = Set(newEnd)

    // create the new NFA
    new Nfa[Int](newStart, finalState, transitions)
  }

  /**
   * Create a new NFA that accepts the concatenation of the languages of nfaA and nfaB
   * @param nfaA the first NFA
   * @param nfaB the second NFA
   * @param stateCounter the counter for the states
   * @return a new NFA that accepts the concatenation of the languages of nfaA and nfaB
   */
  def fromConcat(nfaA: Nfa[Int], nfaB: Nfa[Int], stateCounter: Int) : Nfa[Int] = {
    var transitions = nfaA.transitions ++ nfaB.transitions
    // add the epsilon transition from the final state of nfaA to the start state of nfaB
    transitions += (nfaA.getLastState, 'ε') -> Set(nfaB.start)
    new Nfa[Int](nfaA.start, nfaB.finalStates, transitions)
  }

  /**
   * Create a new NFA that accepts the Kleene star of the language of nfa
   * @param nfa the NFA
   * @param stateCounter the counter for the states
   * @return a new NFA that accepts the Kleene star of the language of nfa
   */
  def fromStar(nfa: Nfa[Int], stateCounter: Int): Nfa[Int] = {
    val newStart  = stateCounter
    val newEnd    = stateCounter + 1

    var transitions = nfa.transitions
    // add the transitions from the new start state to the start states of nfaA and nfaB
    transitions += (newStart, 'ε') -> Set(nfa.start, newEnd)
    // add the transitions from the final states of nfaA and nfaB to the new end state
    transitions += (nfa.getLastState, 'ε') -> Set(newEnd, nfa.start)

    val finalState = Set(newEnd)
    // create the new NFA
    new Nfa[Int](newStart, finalState, transitions)
  }

  /**
   * Create a new NFA that accepts the Kleene plus of the language of nfa
   * @param nfa the NFA
   * @param stateCounter the counter for the states
   * @return a new NFA that accepts the Kleene plus of the language of nfa
   */
  def fromPlus (nfa: Nfa[Int], stateCounter: Int): Nfa[Int] = {
    val newStart  = stateCounter
    val newEnd    = stateCounter + 1

    var transitions = nfa.transitions
    // add the transitions from the new start state to the start states of nfaA and nfaB
    transitions += (newStart, 'ε') -> Set(nfa.start)
    // add the transitions from the final states of nfaA and nfaB to the new end state
    transitions += (nfa.getLastState, 'ε') -> Set(newEnd, nfa.start)

    val finalState = Set(newEnd)
    // create the new NFA
    new Nfa[Int](newStart, finalState, transitions)
  }

  /**
   * Create a new NFA that accepts the language of nfa with an optional character
   * @param nfa the NFA
   * @return a new NFA that accepts the language of nfa with an optional character
   */
  def fromMaybe(nfa: Nfa[Int]) : Nfa[Int] = {
    var transitions = nfa.transitions
    // add the epsilon transition from the start state to the final state
    transitions = transitions + ((nfa.start, 'ε') -> Set(nfa.getLastState))
    transitions = transitions ++ nfa.transitions

    new Nfa[Int](nfa.start, nfa.finalStates, transitions)
  }
}
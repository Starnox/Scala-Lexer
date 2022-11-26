import scala.collection.mutable
// NFA (q0, F, δ)
// q0: initial state
// F: set of final states
// δ: transition function -> maps (state, character) to a set of states -> non-deterministic
class Nfa[A](val start: A, val finalStates: Set[A], val transitions: Map[(A, Char), Set[A]]) {
  var epsilonClosureCache: Map[A, Set[A]] = Map[A, Set[A]]()

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

  def preCalculateEpsilonCLosure(): Map[A, Set[A]] = {
    val epsilonClosure = mutable.Map[A, Set[A]]()
    val states = getStates
    states.foreach(state => epsilonClosure += (state) -> getEpsilonClosure(state))
    epsilonClosure.toMap
  }

  /**
   * Get the alphabet of the NFA
   * @return a set of characters that are in the alphabet
   */
  def getAlphabet: Set[Char] = transitions.keySet.map(_._2)

  /**
   * Epislon closure of state
   * @param state the state to get the epsilon closure of
   * @return the epsilon closure of the state
   */
  def getEpsilonClosure(state: A): Set[A] = {
    val visited = mutable.Set[A]()
    val stack = mutable.Stack[A]()
    stack.push(state)
    while (stack.nonEmpty) {
      val state = stack.pop()
      if (!visited.contains(state)) {
        visited.add(state)
        val epsilonTransitions = transitions.getOrElse((state, 'ε'), Set())
        epsilonTransitions.foreach(stack.push)
      }
    }
    // filter out the states that only have epsilon transitions
    val alphabet = getAlphabet - 'ε'
    visited.filter(s => {
      alphabet.exists(c => transitions.contains((s, c))) || finalStates.contains(s)
    }).toSet
  }

  /**
   * Get the next states of the NFA by applying the transition function including the epsilon transitions
   * @param state the current state
   * @param c the character
   * @return a set of next states
   */
  def next(state:A, c: Char): Set[A] = {
    var result = Set[A]()
    val epsilonClosure = epsilonClosureCache.getOrElse(state, Set()) // get the epsilon closure of the state
    epsilonClosure.foreach(s => result = result ++ transitions.getOrElse((s, c), Set())) // add the transitions for the epsilon closure
    result
  }

  /**
   * Check if the NFA accepts the string str
   * @param str the string to check
   * @return true if the NFA accepts the string, false otherwise
   */
  def accepts(str: String): Boolean = {
    var resStates = Set(start)
    epsilonClosureCache = preCalculateEpsilonCLosure()

    if(str == "")
      resStates = resStates ++ next(start, 'ε')
    else
      resStates = str.foldLeft(epsilonClosureCache.getOrElse(start,Set()))((states, c) => {states.flatMap(next(_, c)) } ) // all possible states after c

    resStates = resStates.flatMap(getEpsilonClosure) // get the epsilon closure of all states
    resStates.intersect(finalStates).nonEmpty
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

  def fromAst(ast: Ast[Char,String]): Nfa[Int] = {
    // create a new NFA from an AST
    var counter = 0

    // go recursively through the AST and create the transitions
    def go(ast: Ast[Char,String]): Nfa[Int] = {
      if (ast.isOperator) {
        ast.getValue match {
          // Concatenation
          case Right("CONCAT") => {
            val first = go(ast.getFirst)
            val second = go(ast.getSecond)
            var transitions = first.transitions ++ second.transitions
            transitions += ((first.getLastState, 'ε') -> Set(second.start))
            val finalStates = second.finalStates
            new Nfa(first.start, finalStates, transitions)
          }
          // Union
          case Right("UNION") => {
            val left = go(ast.getFirst)
            val right = go(ast.getSecond)
            counter += 2

            val newStart = counter - 2
            val newFinal = counter - 1

            var transitions = left.transitions ++ right.transitions
            transitions += ((newStart, 'ε') -> Set(left.start, right.start))
            transitions += ((left.getLastState, 'ε') -> Set(newFinal))
            transitions += ((right.getLastState, 'ε') -> Set(newFinal))

            new Nfa(newStart, Set(newFinal), transitions)
          }
          // Kleene star
          case Right("STAR") => {
            val nfa = go(ast.getFirst)
            counter += 2
            val newStart = counter - 2
            val newFinal = counter - 1

            var transitions = nfa.transitions
            transitions += ((newStart, 'ε') -> Set(nfa.start, newFinal))
            transitions += ((nfa.getLastState, 'ε') -> Set(newFinal, nfa.start))

            new Nfa(newStart, Set(newFinal), transitions)
          }
          case _ => throw new Exception("Unknown operator")
        }
      }
      else {
        ast.getValue match {
          case Left(c) => {
            counter += 2
            val transitions = ((counter - 2, c) -> Set(counter-1))
            new Nfa(counter - 2, Set(counter - 1), Map(transitions))
          }
          case _ => throw new Exception("Invalid character")
        }
      }
    }
    go(ast)
  }

  /**
   * This method takes a string in prenex normal form and returns an NFA that accepts the language of the string
   * @param str the string in prenex normal form
   * @return an NFA that accepts the language of the string
   */
  def fromPrenex(str: String): Nfa[Int] = {
    // create an AST from the string
    val ast = Ast.fromPrenex(str)
    // create an NFA from the AST
    fromAst(ast)
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
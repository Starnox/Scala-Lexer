import Lexer._

class Lexer (spec: String, dfa: Dfa[Int], stateMap: Map[Int, Set[String]], priorities: Map[String, Int]) {

  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */
  def lex(word: String): Either[String,List[(String,String)]] = {
    // return generic error if the word is empty
    Left("Empty word")
  }
}

object Lexer {

  def apply(spec: String): Lexer = {
    // split the string into lines
    val lines = spec.split("\n")
    // remove empty lines
    var maximumState = -1
    var nfaList = List[Nfa[Int]]()
    var stateToToken = Map[Int,String]()
    var priorities = Map[String,Int]()
    var currentPriority = 0

    for (line <- lines) {
      // remove the ';' from the end of the line
      val newLine = line.substring(0, line.length - 1)
      // split by : and remove whitespace
      val lineSplit = newLine.split(":").map(_.trim)

      // set the priority of the lexem
      priorities += (lineSplit(1) -> currentPriority)

      //val nfa = Nfa.fromPrenex(Regex.toPrenex(lineSplit(1)))
      var nfa = Nfa.fromAst(Ast.fromInfix(lineSplit(1)))


      // get the maximum state of the nfa
      if (maximumState != -1)
        // add the last maximum state to the current nfa
        nfa = nfa.map((state) => (state + maximumState))

      maximumState = nfa.getLastState + 1
      // create mapping between the final states and the lexem
      for (state <- nfa.finalStates)
        stateToToken += (state -> lineSplit(1))

      // add the nfa to the list
      nfaList = nfaList :+ nfa

      currentPriority += 1
    }
    // Add a new state and then create epsilon transitions from this state to all the start states of the NFAs
    var epsilonTransitions = Map[(Int, Char), Set[Int]]()
    // add an epsilon transition from the maximum state to each start node of the NFAs
    for (nfa <- nfaList) {
      epsilonTransitions = epsilonTransitions + ((maximumState, 'ε') -> (epsilonTransitions.getOrElse((maximumState, 'ε'), Set()) + nfa.start))
    }
    val mergedNfa = new Nfa(maximumState, nfaList.flatMap(_.finalStates).toSet,
                            nfaList.flatMap(_.transitions).toMap ++ epsilonTransitions)

    // create a dfaAux (group of states) from the merged nfa
    val dfaAux = DfaAux.fromNfa(mergedNfa)

    // convert to dfa but keep the state mapping
    val (dfa, newStateToToken) = Dfa.fromDfaAuxWithProperties(dfaAux, stateToToken)

    new Lexer(spec, dfa, newStateToToken, priorities)
  }
}
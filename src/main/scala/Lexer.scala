import Lexer._

class Lexer (spec: String, dfa: Dfa[Int], stateMap: Map[Int, Set[String]], priorities: Map[String, Int]
            ,skipWhiteSpaces: Boolean = true, skipNewLines: Boolean = true) {

  def getTokenHeighestPriority(tokens: Set[String]): String = {
    tokens.minBy(priorities(_))
  }
  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */
  def lex(word: String): Either[String,List[(String,String)]] = {
    // go through the word and split it into lexems
    // as it goes through the word it also remembers the last lexems it found
    // if it finds a lexem that is not in the dfa it returns an error
    var result = List[(String,String)]()
    var currentLine = 0

    def getNextToken(str: String, start: Int): Either[String, (String, String, Int)] = {
      var lastTokenSet = Set[String]()
      var state = dfa.start
      var i = start
      var end = start
      while (i < str.length && dfa.next(state, str(i)) != -1) {
        if (str(i) == '\n')
          currentLine += 1

        state = dfa.next(state, str(i))
        if (dfa.isFinal(state)) {
          lastTokenSet = stateMap(state)
          end = i + 1
        }
        i += 1
      }
      // if we reached the sink state without going through a final state
      if (lastTokenSet == Set()) {
        if (i != str.length)
          Left("No viable alternative at character " + i + ", line " + currentLine)
        else
          Left("No viable alternative at character EOF, line " + currentLine)
      } else {
        Right(str.substring(start, end), getTokenHeighestPriority(lastTokenSet), end)
      }
    }
    var i = 0
    while (i < word.length) {
      while (i != word.length &&
        ((skipWhiteSpaces && word(i) == ' ') || (skipNewLines && word(i) == '\n') || word(i) == '\t')) {
        i += 1
      }
      if (i != word.length) {
        getNextToken(word, i) match {
          case Left(error) => return Left(error)
          case Right((lexem, token, next)) => {
            result = (lexem, token) :: result
            i = next
          }
        }
      }
    }
    if (result.isEmpty)
      return Left("No viable alternative at character EOF, line " + currentLine)


    Right(result.reverse)
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
    var skipNewLines = true
    var skipWhiteSpaces = true

    for (line <- lines) {
      // remove the ';' from the end of the line
      val newLine = line.substring(0, line.length - 1)
      // split by : and remove whitespace
      val lineSplit = newLine.split(":").map(_.trim)

      // set the priority of the lexem
      priorities += (lineSplit(0) -> currentPriority)

      //val nfa = Nfa.fromPrenex(Regex.toPrenex(lineSplit(1)))
      val regex = lineSplit(1)
      var newRegex = ""
      if (regex == "\'\\n\'") {
        newRegex = '\n'.toString
        skipNewLines = false
      }
      else
        newRegex = regex

      if (regex == "\' \'")
        skipWhiteSpaces = false;

      var nfa = Nfa.fromAst(Ast.fromInfix(newRegex))

      // get the maximum state of the nfa
      if (maximumState != -1)
        // add the last maximum state to the current nfa
        nfa = nfa.map((state) => (state + maximumState))

      maximumState = nfa.getLastState + 1
      // create mapping between the final states and the lexem
      for (state <- nfa.finalStates)
        stateToToken += (state -> lineSplit(0))

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
    new Lexer(spec, dfa, newStateToToken, priorities, skipWhiteSpaces, skipNewLines)
  }
}
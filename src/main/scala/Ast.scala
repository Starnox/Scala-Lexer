import scala.collection.mutable
import scala.util.control.Breaks

/**
 * A class that represents a generic Abstract Syntax Tree
 *
 * @param astValue  the value of the AST
 * @param astFirst  the first child of the AST
 * @param astSecond the second child of the AST
 * @tparam A the type of the value
 * @tparam B the type of the operator
 */
class Ast[A, B](astValue: Either[A, B], astFirst: Ast[A, B], astSecond: Ast[A, B]) {

  def getFirst: Ast[A, B] = astFirst

  def getSecond: Ast[A, B] = astSecond

  def getValue: Either[A, B] = astValue

  def isLeaf: Boolean = astFirst == null && astSecond == null

  def isNode: Boolean = !isLeaf

  def isValue: Boolean = {
    astValue match {
      case Left(_) => true
      case Right(_) => false
    }
  }

  def isOperator: Boolean = {
    astValue match {
      case Left(_) => false
      case Right(_) => true
    }
  }

  /**
   * Print the AST in a prefix notation
   *
   * @return the AST in a prefix notation
   */
  override def toString: String = {
    if (isOperator) {
      astValue match {
        case Right(op) =>
          var astFirstString = if (astFirst == null) "" else astFirst.toString
          astFirstString = if (astFirstString == " ") "' '" else astFirstString
          var astSecondString = if (astSecond == null) "" else astSecond.toString
          astSecondString = if (astSecondString == " ") "' '" else astSecondString
          val operatorExtended = op match {
            case '.' => "CONCAT"
            case '|' => "UNION"
            case '*' => "STAR"
            case '?' => "MAYBE"
            case '+' => "PLUS"
            case _ => throw new Exception("Unknown operator")
          }
          if (astSecond == null)
            operatorExtended + " " + astFirstString
          else
            operatorExtended + " " + astFirstString + " " + astSecondString
        case _ => throw new Exception("Should not happen")
      }
    }
    else {
      astValue match {
        case Left(c) => if (c.toString == " ") "' '" else c.toString
        case _ => throw new Exception("Should not happen")
      }
    }

  }

  /**
   * Two ASTs are equal if they have the same value and the same children
   *
   * @param obj the object to compare to
   * @return true if the ASTs are equal, false otherwise
   */
  override def equals(obj: Any): Boolean = {
    obj match {
      case that: Ast[A, B] =>
        if (this.isLeaf && that.isLeaf) this.astValue == that.getValue
        else if (this.isNode && that.isNode)
          this.astValue == that.getValue && this.astFirst == that.getFirst && this.astSecond == that.getSecond
        else false

      case _ => false
    }
  }
}


object Ast {
  def apply(astValue: Either[Char, Char], firstValue: Ast[Char, Char], secondValue: Ast[Char, Char]): Ast[Char, Char] = {
    new Ast(astValue, firstValue, secondValue)
  }

  /**
   * Create an AST from a infix notation string using the Shunting Yard algorithm
   *
   * @param str the string to parse
   * @return the AST
   */
  def fromInfix(str: String): Ast[Char, Char] = {
    val tokens = Regex.preprocess(str.toList)
    // the output will be a stack of ast nodes
    val output = mutable.Stack[Ast[Char, Char]]()
    // the operators will be a stack of operators
    val operators = mutable.Stack[Char]()

    // when we pop an operator, we need to know if it is a unary or binary operator
    // this is because we need to know if we need to pop one or two nodes from the output stack
    // then we need to push the result back to the output stack
    def popOperation(currentOperation: Char): Unit = {
      val opPriority = Regex.getPriorityOfOperator(currentOperation)
      if (opPriority == 3) {
        // unary operator
        val topAst = output.pop()
        output.push(Ast(Right(currentOperation), topAst, null))
      } else {
        // binary operator
        val topAst2 = output.pop()
        val topAst1 = output.pop()
        output.push(Ast(Right(currentOperation), topAst1, topAst2))
      }
    }

    // go through the tokens
    for (token <- tokens) {
      token match {
        case Left(c) => output.push(Ast(Left(c), null, null)) // if the token is a character, push it to the output
        case Right(op) =>
          // if the operator is a left parenthesis, push it to the operators stack
          if (op == '(')
            operators.push(op)
          // if the operator is a right parenthesis
          else if (op == ')') {
            // pop operators from the operators stack and push them to the output until a left parenthesis is found
            while (operators.top != '(')
              popOperation(operators.pop())
            // pop the left parenthesis from the operators stack
            operators.pop()
          }
          // not a parenthesis
          else {
            val opPriority = Regex.getPriorityOfOperator(op)
            // if the current operator has higher priority or the stack is empty, push it to the operators stack
            if (operators.isEmpty || opPriority > Regex.getPriorityOfOperator(operators.top))
              operators.push(op)

            else {
              val loop = new Breaks
              loop.breakable {
                while (operators.nonEmpty) {
                  val opTop = operators.top
                  val opTopPriority = Regex.getPriorityOfOperator(opTop)

                  if (op == '.') { // concatenation is right associative
                    if (opPriority < opTopPriority)
                      popOperation(operators.pop())
                    else loop.break
                  }
                  else {
                    if (opPriority <= opTopPriority)
                      popOperation(operators.pop())
                    else loop.break
                  }
                }
              }
              operators.push(op)
            }
          }
      }
    }
    // pop operators from the operators stack and push them to the output until the operators stack is empty
    while (operators.nonEmpty) {
      popOperation(operators.pop())
    }
    // the output stack should contain only one element, which is the ast
    output.pop()
  }

  /**
   * Create an AST from a prefix notation string
   *
   * @param str the string to parse
   * @return the AST
   */
  def fromPrenex(str: String): Ast[Char, Char] = {
    var tokens = Helpers.customSplit(str)

    def createAST(): Ast[Char, Char] = {
      // if the tokens list is empty, return null
      if (tokens.isEmpty) {
        return null
      }
      // get the next token and remove it from the list
      val token = tokens.head
      tokens = tokens.tail

      token match {
        case "eps" => new Ast[Char, Char](Left('ε'), null, null)
        case "' '" => new Ast[Char, Char](Left(' '), null, null)
        case "UNION" => new Ast[Char, Char](Right('|'), createAST(), createAST())
        case "CONCAT" => new Ast[Char, Char](Right('.'), createAST(), createAST())
        case "STAR" => new Ast[Char, Char](Right('*'), createAST(), null)
        case "PLUS" =>
          val ast = createAST() // convert plus to concat star
          new Ast[Char, Char](Right('.'), ast, new Ast[Char, Char](Right('*'), ast, null))

        case "MAYBE" =>
          val ast = createAST() // convert maybe to union with eps
          new Ast[Char, Char](Right('|'), ast, new Ast[Char, Char](Left('ε'), null, null))

        case _ => new Ast[Char, Char](Left(token.charAt(0)), null, null)
      }
    }

    createAST()
  }
}

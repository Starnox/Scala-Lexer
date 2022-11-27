import scala.collection.mutable
import scala.util.control.Breaks
import scala.util.control.Breaks.break

// Abstract Syntax Tree for the language
// A is the Character type
// B is the operator type
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

  override def toString: String = {
    // print ast in prefix notation
    if (isOperator) {
      astValue match {
        case Right(op) => {
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
        }
        case _ => throw new Exception("Should not happen")
      }
    }
    else {
      astValue match {
        case Left(c) => c.toString
        case _ => throw new Exception("Should not happen")
      }
    }

  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: Ast[A, B] => {
        if (this.isLeaf && that.isLeaf) {
          this.astValue == that.getValue
        } else if (this.isNode && that.isNode) {
          this.astValue == that.getValue && this.astFirst == that.getFirst && this.astSecond == that.getSecond
        } else {
          false
        }
      }
      case _ => false
    }
  }
}


object Ast {
  def apply(astValue: Either[Char, Char], firstValue: Ast[Char, Char], secondValue: Ast[Char, Char]): Ast[Char, Char] = {
    new Ast(astValue, firstValue, secondValue)
  }

  def fromInfix(str: String): Ast[Char, Char] = {
    // convert infix to ast using shunting yard algorithm
    // get tokens
    val tokens = Regex.preprocess(str.toList)
    // the output will be a stack of ast nodes
    val output = mutable.Stack[Ast[Char, Char]]()
    // the operators will be a stack of operators
    val operators = mutable.Stack[Char]()

    def popOperation(currentOperation: Char) : Unit = {
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
        case Left(c) => {
          // if the token is a character, push it to the output
          output.push(Ast(Left(c), null, null))
        }
        case Right(op) => {
          // check for special cases

          // if the operator is a left parenthesis, push it to the operators stack
          if (op == '(') {
            operators.push(op)
          }
            // if the operator is a right parenthesis
          else if (op == ')') {
            // pop operators from the operators stack and push them to the output until a left parenthesis is found
            while (operators.top != '(') {
              popOperation(operators.pop())
            }
            // pop the left parenthesis from the operators stack
            operators.pop()
          }
            // not a parenthesis
          else {
            val opPriority = Regex.getPriorityOfOperator(op)
            // if the current operator has higher priority or the stack is empty, push it to the operators stack
            if (operators.isEmpty ||  opPriority > Regex.getPriorityOfOperator(operators.top)) {
              operators.push(op)
            }
            else {
              val loop = new Breaks;
              loop.breakable {
                while (operators.nonEmpty) {
                  val opTop = operators.top
                  val opTopPriority = Regex.getPriorityOfOperator(opTop)
                  // concatenation is right associative
                  if (op == '.') {
                    if (opPriority < opTopPriority) {
                      popOperation(operators.pop())
                    }
                    else
                      loop.break;
                  }
                  else {
                    if (opPriority <= opTopPriority) {
                      popOperation(operators.pop())
                    }
                    else
                      loop.break;
                  }

                }
              }
              operators.push(op)
            }
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

  def fromPrenex(str: String): Ast[Char, Char] = {
    // This function should construct an AST out of a prenex expression.
    // Example fromPrenex("CONCAT STAR a b") should return an AST with
    // root node as CONCAT and left child as STAR with a as its child and the right child of concatenation as b
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
        case "eps" => {
          new Ast[Char, Char](Left('ε'), null, null)
        }
        case "' '" => {
          new Ast[Char, Char](Left(' '), null, null)
        }
        case "UNION" => {
          new Ast[Char, Char](Right('|'), createAST(), createAST())
        }
        case "CONCAT" => {
          new Ast[Char, Char](Right('.'), createAST(), createAST())
        }
        case "STAR" => {
          new Ast[Char, Char](Right('*'), createAST(), null)
        }
        case "PLUS" => {
          val ast = createAST()
          // convert plus to concat star
          new Ast[Char, Char](Right('.'), ast, new Ast[Char, Char](Right('*'), ast, null))
        }
        case "MAYBE" => {
          // convert maybe to union with eps
          val ast = createAST()
          new Ast[Char, Char](Right('|'), ast, new Ast[Char, Char](Left('ε'), null, null))
        }
        case _ => {
          new Ast[Char, Char](Left(token.charAt(0)), null, null)
        }
      }
    }

    createAST()
  }
}

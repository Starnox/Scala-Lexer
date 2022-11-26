import scala.collection.mutable

// Abstract Syntax Tree for the language
// A is the Character type
// B is the operator type
class Ast[A,B](astValue: Either[A,B], astFirst: Ast[A,B], astSecond: Ast[A,B]) {
  def getFirst: Ast[A,B] = astFirst

  def getSecond: Ast[A,B] = astSecond

  def getValue: Either[A,B] = astValue

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
          val astFirstString = if (astFirst == null) "" else astFirst.toString
          val astSecondString = if (astSecond == null) "" else astSecond.toString
          if (astSecond == null)
            op.toString + " " + astFirstString
          else
            op.toString + " " + astFirstString + " " + astSecondString
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
      case that: Ast[A,B] => {
        if(this.isLeaf && that.isLeaf) {
          this.astValue == that.getValue
        } else if(this.isNode && that.isNode) {
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
  def apply(astValue: Either[Char,String], firstValue: Ast[Char,String], secondValue: Ast[Char,String]): Ast[Char,String] = {
    new Ast(astValue, firstValue, secondValue)
  }

  def fromPrenex(str: String): Ast[Char,String] = {
    // This function should construct an AST out of a prenex expression.
    // Example fromPrenex("CONCAT STAR a b") should return an AST with
    // root node as CONCAT and left child as STAR with a as its child and the right child of concatenation as b
    var tokens = Helpers.customSplit(str)

    def createAST(): Ast[Char,String] = {
      // if the tokens list is empty, return null
      if (tokens.isEmpty) {
        return null
      }
      // get the next token and remove it from the list
      val token = tokens.head
      tokens = tokens.tail

      token match {
        case "eps" => {
          new Ast[Char,String](Left('ε'), null, null)
        }
        case "' '" => {
          new Ast[Char,String](Left(' '), null, null)
        }
        case "UNION" => {
          new Ast[Char,String](Right("UNION"), createAST(), createAST())
        }
        case "CONCAT" => {
          new Ast[Char,String](Right("CONCAT"), createAST(), createAST())
        }
        case "STAR" => {
          new Ast[Char,String](Right("STAR"), createAST(), null)
        }
        case "PLUS" => {
          val ast = createAST()
          // convert plus to concat star
          new Ast[Char,String](Right("CONCAT"), ast, new Ast[Char,String](Right("STAR"), ast, null))
        }
        case "MAYBE" => {
          // convert maybe to union with eps
          val ast = createAST()
          new Ast[Char,String](Right("UNION"), ast, new Ast[Char,String](Left('ε'), null, null))
        }
        case _ => {
          new Ast[Char,String](Left(token.charAt(0)), null, null)
        }
      }
    }
    createAST()
  }
}

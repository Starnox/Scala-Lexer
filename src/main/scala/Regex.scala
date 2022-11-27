object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  def preprocess(s:List[Char]): List[Either[Char,Char]] = {
    // handle epsilon case
    var i = 0
    var newS = s
    while (i < newS.length - 2) {
      if (s(i) == 'e' && s(i + 1) == 'p' && s(i + 2) == 's')
        newS = s.patch(i, List('ε'), 3)
      i += 1
    }

    // convert escaped characters
    i = 0
    while (i < newS.length - 1) {
      if (newS(i) == '\\' || newS(i) == '\'' || newS(i) == '\"') {
        newS = newS.patch(i, List(newS(i + 1)), 2)
      }
      i += 1
    }
    newS = newS.filter(_ != '\'')

    // convert special inputs like [0-9] to their correct form
    i = 0
    while (i < newS.length - 1) {
      if (newS(i) == '[') {
        val firstParam = newS(i + 1)
        val secondParam = newS(i + 3)
        val range = firstParam.to(secondParam).toList
        // append between each characters of the range the character '|'
        val newRange = range.foldLeft(List[Char]())((acc, c) => acc :+ c :+ '|')
        // remove the last '|'
        val finalRange = newRange.dropRight(1)
        // surround the range with parentheses
        val finalRangeWithParentheses = '(' +: finalRange :+ ')'
        // replace the original range with the new range
        newS = newS.patch(i, finalRangeWithParentheses, 5)
        i += finalRangeWithParentheses.length - 1
      }
      i += 1
    }

    var res = (for (c <- newS) yield{
      c match {
        case '*' => Right('*')
        case '+' => Right('+')
        case '?' => Right('?')
        case '|' => Right('|')
        case '.' => Right('.')
        case '(' => Right('(')
        case ')' => Right(')')
        case '[' => Right('[')
        case ']' => Right(']')
        // check for epsilon which is represented by 3 characters "eps"


        case x => Left(x)
      }
    })

    // add a Right('.') between every two Left characters, between a closing parenthesis and a Left character
    // to the right of a unary operator
    i = 0
    while (i < res.length - 1) {
      if (res(i).isLeft && res(i + 1).isLeft) {
        res = res.patch(i + 1, List(Right('.')), 0)
      }
      else if (res(i).isRight && res(i) == Right(')') && res(i + 1).isLeft) {
        res = res.patch(i + 1, List(Right('.')), 0)
      }
      else if(res(i).isLeft && res(i + 1).isRight && res(i + 1) == Right('(')) {
        res = res.patch(i + 1, List(Right('.')), 0)
      }
      else if(res(i).isRight && res(i) == Right('*') && (res(i + 1).isLeft || res(i + 1) == Right('('))) {
        res = res.patch(i + 1, List(Right('.')), 0)
      }
      else if(res(i).isRight && res(i) == Right(')') && res(i+1).isRight && res(i+1) == Right('(')) {
        res = res.patch(i + 1, List(Right('.')), 0)
      }
      else if((res(i) == Right('?') || res(i) == Right('+')) && (res(i + 1).isLeft || res(i + 1) == Right('('))) {
        res = res.patch(i + 1, List(Right('.')), 0)
      }

      i += 1
    }
    res
    // TODO convert special inputs like [0-9] to their correct form
  }

  def getPriorityOfOperator(op:Char): Int = {
    op match {
      case '*' => 3
      case '+' => 3
      case '?' => 3
      case '.' => 2
      case '|' => 1
      case '(' => 0
      case ')' => 0
    }
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    // convert infix to ast
    val ast = Ast.fromInfix(str)
    // convert ast to prenex
    ast.toString
  }
}

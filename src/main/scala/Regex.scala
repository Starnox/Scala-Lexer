object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  def preprocess(s:List[Char]): List[Either[Char,Char]] = ???

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = ???
}

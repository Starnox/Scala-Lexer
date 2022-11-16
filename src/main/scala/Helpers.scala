class Helpers {

}

object Helpers {
  /**
   * Custom implementation of split
   * Split the string str by whitespace but keep the whitespace in quotes
   * e.g. "a ' ' b" -> List("a", " ", "b")
   * @param str the string to split
   * @return a list of strings that are the result of splitting the string
   */
  def customSplit(str: String) : List[String] = {
    var result = List[String]()
    var current = ""
    var inQuotes = false
    for (c <- str) {
      if (c == ' ' && !inQuotes) {
        result = current :: result
        current = ""
      }
      else {
        if (c == '\'')
          inQuotes = !inQuotes
        current += c
      }
    }
    result = current :: result
    result.reverse
  }
}

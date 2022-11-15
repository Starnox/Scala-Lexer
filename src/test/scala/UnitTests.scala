class UnitTests extends munit.FunSuite {
  test ("Test Maybe") {
    assert(Nfa.fromPrenex("MAYBE a").accepts(""))
  }

  test("test UNION") {
    assert(Nfa.fromPrenex("UNION a eps").accepts("a"))
    assert(!Nfa.fromPrenex("UNION a eps").accepts("b"))
    assert(Nfa.fromPrenex("UNION a eps").accepts(""))
  }

  test("test concat and union") {
    //println(Nfa.fromPrenex("UNION CONCAT a b CONCAT c d"))
//    assert(Nfa.fromPrenex("UNION CONCAT a b CONCAT c d").accepts("ab"))
//    assert(Nfa.fromPrenex("UNION CONCAT a b CONCAT c d").accepts("cd"))
//    assert(!Nfa.fromPrenex("UNION CONCAT a b CONCAT c d").accepts("ac"))
//    assert(!Nfa.fromPrenex("UNION CONCAT a b CONCAT c d").accepts("abc"))
    assert(Nfa.fromPrenex("CONCAT PLUS c UNION a PLUS b").accepts("cb"))
  }

  test("dfa aux") {
    val nfa = new Nfa[Int](0, Set(1), Map(
      (0, 'a') -> Set(0,1),
      (0, 'b') -> Set(0),
      (1, 'a') -> Set(2),
      (1, 'b') -> Set(2),
      (2, 'a') -> Set(3),
      (2, 'b') -> Set(3)
    ))
  }

  test("a*") {
    val nfa = Nfa.fromPrenex("STAR a")
    val dfaAux = DfaAux.fromNfa(nfa)
    val dfa = Dfa.fromPrenex("STAR a")
    assert(dfa.accepts("a"))
    assert(dfa.accepts("aa"))
    assert(dfa.accepts("aaa"))
    assert(!dfa.accepts("b"))
  }

  test("UNION a b") {
//    val nfa = Nfa.fromPrenex("UNION a b")
//    println(nfa)
//    val dfaAux = DfaAux.fromNfa(nfa)
//    println(dfaAux)
    val dfa = Dfa.fromPrenex("UNION a b")
  }

  test("character") {
    val dfa = Dfa.fromPrenex("a")
    assert(dfa.accepts("a"))
    assert(!dfa.accepts("b"))
  }
}

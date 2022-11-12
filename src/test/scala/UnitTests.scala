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
    println(Nfa.fromPrenex("UNION CONCAT a b CONCAT c d"))
//    assert(Nfa.fromPrenex("UNION CONCAT a b CONCAT c d").accepts("ab"))
//    assert(Nfa.fromPrenex("UNION CONCAT a b CONCAT c d").accepts("cd"))
//    assert(!Nfa.fromPrenex("UNION CONCAT a b CONCAT c d").accepts("ac"))
//    assert(!Nfa.fromPrenex("UNION CONCAT a b CONCAT c d").accepts("abc"))
    assert(Nfa.fromPrenex("CONCAT PLUS c UNION a PLUS b").accepts("cb"))
  }
}

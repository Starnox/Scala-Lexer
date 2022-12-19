class DfaTests extends munit.FunSuite {

  test("Test eps (1p)") {
    assert(Dfa.fromPrenex("eps").accepts(""))
    assert(!Dfa.fromPrenex("eps").accepts(" "))
  }

  test ("New line") {
    val s = "\n"
    val nfa = Nfa.fromAst(Ast.fromInfix(s))
    val dfa = Dfa.fromDfaAux(DfaAux.fromNfa(nfa))
    assert(dfa.accepts("\n"))
    assert(!dfa.accepts(" "))
    assert(!dfa.accepts("\\"))

  }

  test("Test void (1p)") {
    val s = "void"
    assert(!Dfa.fromPrenex(s).accepts(""))
    assert(!Dfa.fromPrenex(s).accepts("a"))
    assert(!Dfa.fromPrenex(s).accepts("void"))
  }

  test("Test space (1p)") {
    val s = "' '"
    assert(Dfa.fromPrenex(s).accepts(" "))
    assert(!Dfa.fromPrenex(s).accepts(""))
  }

  test("Test character (1p)") {
    val s = "a"
    assert(Dfa.fromPrenex(s).accepts("a"))
    assert(!Dfa.fromPrenex(s).accepts("b"))
  }

  test("Test weird characters (1p)") {
    "$@*	({\"'\r\n".foreach(
      x => {
        assert(Dfa.fromPrenex(x+"").accepts(x+""))
      }
    )
  }

  test("Test concat (1p)") {
    val s = "CONCAT a b"
    assert(Dfa.fromPrenex(s).accepts("ab"))
    assert(!Dfa.fromPrenex(s).accepts("a"))
  }

  test("Test star (1p)") {
    val s = "STAR a"
    assert(Dfa.fromPrenex(s).accepts(""))
    assert(Dfa.fromPrenex(s).accepts("a"))
    assert(Dfa.fromPrenex(s).accepts("aa"))
    assert(Dfa.fromPrenex(s).accepts("aaaaaaaaaaaaaaaaa"))
  }

  test("Test union (1p)") {
    val s = "UNION a b"
    assert(!Dfa.fromPrenex(s).accepts(""))
    assert(Dfa.fromPrenex(s).accepts("a"))
    assert(Dfa.fromPrenex(s).accepts("b"))
    assert(!Dfa.fromPrenex(s).accepts("ab"))
  }

  test("Test complex 1 (10p)") {
    val s = "STAR UNION a b"
    assert(Dfa.fromPrenex(s).accepts("aaababaaabaaaaa"))
    assert(Dfa.fromPrenex(s).accepts("aaaaaaaaaa"))
    assert(Dfa.fromPrenex(s).accepts("bbbbbbbbbbb"))
    assert(!Dfa.fromPrenex(s).accepts("baaabbbabaacabbbaaabbb"))
  }

  test("A to Z (2p)") {
    val str = "[A-Z]"
    //assert(Regex.toPrenex(str) == "UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION A B C D E F G H I J K L M N O P R S T U V W X Y Z")
    val s = "UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION A B C D E F G H I J K L M N O P R S T U V W X Y Z"
    val dfa = Dfa.fromPrenex(s)
    assert(dfa.accepts("K"))
    assert(dfa.accepts("T"))
    assert(dfa.accepts("U"))
  }

  test("all (6p)") {
    val str = "([0-9]*|b+)c?d(da)(\' \'|[A-Z]|\'a\')?"
    val s = "CONCAT CONCAT CONCAT UNION STAR UNION UNION UNION UNION UNION UNION UNION UNION UNION 0 1 2 3 4 5 6 7 8 9 CONCAT b STAR b UNION c eps d CONCAT CONCAT d a UNION UNION ' ' UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION A B C D E F G H I J K L M N O P R S T U V W X Y Z a eps"
    val dfa = Dfa.fromPrenex(s)
    assert(dfa.accepts("bdda"))
    assert(dfa.accepts("28121274849cdda"))
    assert(dfa.accepts("dda"))
    assert(dfa.accepts("bbbbbbcddaa"))
    assert(dfa.accepts("bddaT"))
    assert(dfa.accepts("07cdda "))
    assert(!dfa.accepts("07bcdda "))
  }

  test("Test complex 2 (10p)") {
    val s = "STAR CONCAT a b"
    assert(Dfa.fromPrenex(s).accepts("ababababab"))
    assert(!Dfa.fromPrenex(s).accepts("abababababa"))
    assert(!Dfa.fromPrenex(s).accepts("abababaabab"))
  }

  test("Test complex 3 (10p)") {
    val s = "CONCAT UNION b STAR a STAR c"
    assert(Dfa.fromPrenex(s).accepts("aaaaaaaaaccccc"))
    assert(Dfa.fromPrenex(s).accepts("bccccccccc"))
    assert(!Dfa.fromPrenex(s).accepts("bbbbccccccccc"))

  }

  test("Test complex 4 (10p)") {
    val s = "CONCAT a STAR a"
		assert(Dfa.fromPrenex(s).accepts("aaa"))
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(!Dfa.fromPrenex(s).accepts(""))
  }
  test("Test complex 5 (10p)") {
    val s = "CONCAT STAR a STAR b"
    assert(Dfa.fromPrenex(s).accepts(""))
    assert(Dfa.fromPrenex(s).accepts("a"))
    assert(Dfa.fromPrenex(s).accepts("b"))
    assert(Dfa.fromPrenex(s).accepts("ab"))
    assert(Dfa.fromPrenex(s).accepts("aaaaaaa"))
    assert(Dfa.fromPrenex(s).accepts("bbbbb"))
    assert(Dfa.fromPrenex(s).accepts("aaabbbbb"))
    assert(!Dfa.fromPrenex(s).accepts("aaabbbbbab"))
  }

  test("Test complex 6 debug (10p)") {
    val s = "UNION STAR a STAR b"
    val dfa = Dfa.fromPrenex(s)
    assert(dfa.accepts(""))
    assert(dfa.accepts("a"))
    assert(dfa.accepts("b"))
    assert(!dfa.accepts("ab"))
    assert(dfa.accepts("aaaaaaa"))
    assert(dfa.accepts("bbbbb"))
    assert(!dfa.accepts("aaabbbbbab"))
  }

  test("Test complex 6 (10p)") {
    val s = "UNION STAR a STAR b"
    assert(Dfa.fromPrenex(s).accepts(""))
    assert(Dfa.fromPrenex(s).accepts("a"))
    assert(Dfa.fromPrenex(s).accepts("b"))
    assert(!Dfa.fromPrenex(s).accepts("ab"))
    assert(Dfa.fromPrenex(s).accepts("aaaaaaa"))
    assert(Dfa.fromPrenex(s).accepts("bbbbb"))
    assert(!Dfa.fromPrenex(s).accepts("aaabbbbbab"))
  }

  test("Test complex 7 (10p)") {
    val s = "CONCAT UNION a b UNION b a"
    assert(Dfa.fromPrenex(s).accepts("ab"))
    assert(Dfa.fromPrenex(s).accepts("aa"))
    assert(Dfa.fromPrenex(s).accepts("bb"))
    assert(Dfa.fromPrenex(s).accepts("ba"))
    assert(!Dfa.fromPrenex(s).accepts("a"))
    assert(!Dfa.fromPrenex(s).accepts("b"))
  }

  test("DFA map") {
    val regexes = List(
      ("CONCAT UNION b STAR a STAR c", "abc"),
			("CONCAT a STAR a", "a"),
			("CONCAT a UNION b STAR CONCAT c d", "abcd")
    )

    assert(regexes.forall(p => { // run test for each tuple in "regexes"
      val regex = p._1
      val alphabet = p._2

      def f(x: Int): Int = x + 2

      val dfa = Dfa.fromPrenex(regex)
      val mapped_dfa = dfa.map(f)

      val states = dfa.getStates
      val mapped_states = mapped_dfa.getStates

      // check if the new set of states is the result of mapping f on the old set
      (states.map(f) == mapped_states) &&
      // check if the same applies to the set of final states
      (states.forall(s => dfa.isFinal(s) == mapped_dfa.isFinal(f(s)))) &&
      // check if f(old_delta(old_state,c)) = new_delta(new_state, c) for each state-character pair
      (
        alphabet.forall(c =>
          states.forall(s =>
            f(dfa.next(s,c)) == mapped_dfa.next(f(s), c)
          )
        )
      )
    }))
  }
}

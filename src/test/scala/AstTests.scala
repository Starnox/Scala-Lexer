class AstTests extends munit.FunSuite{
  test("Test Ast Concat star a b") {
    val s = "CONCAT STAR a b"
    val ast = Ast.fromPrenex(s)
    println(ast)
    assertEquals(ast, Ast(Right("CONCAT"), Ast(Right("STAR"), Ast(Left('a'), null, null), null), Ast(Left('b'), null, null)))
  }

  test("Test Ast CONCAT a CONCAT STAR b CONCAT c STAR d") {
    val s = "CONCAT a CONCAT STAR b CONCAT c STAR d"
    val ast = Ast.fromPrenex(s)
    println(ast)
    assertEquals(ast, Ast(Right("CONCAT"), Ast(Left('a'), null, null), Ast(Right("CONCAT"), Ast(Right("STAR"), Ast(Left('b'), null, null), null), Ast(Right("CONCAT"), Ast(Left('c'), null, null), Ast(Right("STAR"), Ast(Left('d'), null, null), null)))))

  }

}

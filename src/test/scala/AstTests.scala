class AstTests extends munit.FunSuite{
  test("Test Ast Concat star a b") {
    val s = "CONCAT STAR a b"
    val ast = Ast.fromPrenex(s)
    println(ast)
    assertEquals(ast, Ast(Right('.'), Ast(Right('*'), Ast(Left('a'), null, null), null), Ast(Left('b'), null, null)))
  }

  test("Test Ast CONCAT a CONCAT STAR b CONCAT c STAR d") {
    val s = "CONCAT a CONCAT STAR b CONCAT c STAR d"
    val ast = Ast.fromPrenex(s)
    println(ast)
    assertEquals(ast, Ast(Right('.'), Ast(Left('a'), null, null), Ast(Right('.'), Ast(Right('*'), Ast(Left('b'), null, null), null), Ast(Right('.'), Ast(Left('c'), null, null), Ast(Right('*'), Ast(Left('d'), null, null), null)))))

  }

}

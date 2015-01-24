import fpinscala.parsing._
def test[Parser[+A]](P: Parsers[Parser]): Parser[String] = {
  import P._
  run(slice(("a"|"b").many))("aaba") // todo: enable chars 'a'|'b'
  char('a').many.slice.map(_.size)

  "abra" | "cadabra"
}

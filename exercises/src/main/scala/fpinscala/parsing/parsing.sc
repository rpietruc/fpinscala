import fpinscala.parsing._
import fpinscala.datastructures._

def test[Parser[+A]](P: Parsers[Parser]): Parser[String] = {
  import P._
  run(slice(("a" | "b").many))("aaba") // todo: enable chars 'a'|'b'
  char('a').many.slice.map(_.size)
  char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)

  ("[0-9]+".r)
    .flatMap(digit => {
      val n = digit.toInt;
      listOfN(n, char('a')).map(a => n) }
    )

  "abra" | "cadabra"
}


trait JSON

object JSON {

  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val space: Parser[String] = "[:space:]*".r

    def braced(begin: Parser[String], end: Parser[String]): Parser[String] => Parser[String] =
      a => (begin ** a ** end).map { case ((b, a), e) => a }

    def bracedSame(beginEnd: Parser[String]) =
      braced(beginEnd, beginEnd)

    def bracedString = bracedSame("\"")
    def bracedArray = braced("[", "]")
    def bracedJObject = braced("{", "}")
    def bracedSpace = bracedSame(space)

//    val spaces = char(' ').many.slice

    //  {
    //    "Company name" : "Microsoft Corporation",
    //    "Ticker" : "MSFT",
    //    "Active" : true,
    //    "Price"
    //      : 30.66,
    //    "Shares outstanding" : 8.38e9,
    //    "Related companies" :
    //    [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
    //  }

    // helpers
    def optional(s: Parser[String]): Parser[String] = s | space
    val someString = "[:word:]*".r
    val validString = "[:alpha:][:word:]*".r // "[a-zA-Z_][a-zA-Z0-9_]*".r
    val coma = bracedSpace(",")
    val mapSymbol = bracedSpace(":")

    // name
    val name = bracedString(validString)

    // values
    val boolean = "true" | "false"
    val stringValue = bracedString(someString) // in braces

    // array
//    val arrayElements: Parser[List[String]] =
//      (stringValue ** optional((coma ** arrayElements).map { case (c, e) => e })
//        ).map { case (a, l) => a :: l }
//    val array = bracedArray(space ** optional(arrayElements) ** space)

//    val value = boolean | stringValue | array | jobject

    // map : value
//    val mapPair = name ** mapSymbol ** value
//    val mapElements = mapPair ** optional(coma ** mapElements)

//    val jobject = bracedJObject(space ** optional(mapElements) ** space)
    ("abra" | "cadabra").map(JString)
  }
}

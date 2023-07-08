object GivensVsImplicits {
  case class Person(name: String):
    def greet: String = s"Hello, my name is $name"

  object Implicits {
    // implicit conversion (discouraged)
    import scala.language.implicitConversions
    implicit def stringToPerson(string: String): Person = Person(string)
    "Jinhoon".greet

    // syntax ambiguous
    implicit val implicitSize: Int = 4
    private def getMap(implicit size: Int): Map[String, Int] = ???
    getMap(3)
    val map: Map[String, Int] = getMap
    map("bob")
    // getMap("Bob") <- error
  }

  object Givens {
    given stringToPerson: Conversion[String, Person] with {
      override def apply(string: String): Person = Person(string)
    }
    "Jinhoon".greet

    given givenSize: Int = 4
    private def getMap(using size: Int): Map[String, Int] = ???
    getMap(using 3)
    getMap("Bob")
  }
}

case class Person(surname: String, name: String, age: Int)

object Givens {
  private val personOrdering: Ordering[Person] = (x: Person, y: Person) => {
    x.surname.compareTo(y.surname)
  }

  private def listPeople(people: Seq[Person])(ordering: Ordering[Person]): Seq[Person] = ???
  private def findFirstPersonInOrder(people: Seq[Person])(ordering: Ordering[Person]): Person = ???


  private def listPeopleWithDefault(people: Seq[Person])(using ordering: Ordering[Person]): Seq[Person] =
    people.sorted(ordering)

  private def findFirstPersonInOrderWithDefault(people: Seq[Person])(using ordering: Ordering[Person]): Person =
    people.min(ordering)

  private val people = List(Person("Bae", "Jinhoon", 30), Person("Aamir", "Helena", 35), Person("Ji", "Eunjin", 29))

  private def sortThings[T](things: List[T])(using ordering: Ordering[T]): List[T] =
    things.sorted(ordering)

  private val mayPeople: List[Option[Person]] =
    List(Option(Person("Bae", "Jinhoon", 30)), Option.empty, Option(Person("Aamir", "Helena", 35)), Option.empty, Option(Person("Ji", "Eunjin", 29)))

  def main(args: Array[String]): Unit = {
      // import given
      import DefaultValues.defaultPersonOrdering

      println(personOrdering.compare(people.head, people.last))
      println(defaultPersonOrdering.compare(people.head, people.last))

      println(listPeopleWithDefault(people))
      println(findFirstPersonInOrderWithDefault(people))

      println(people.sorted)
      println(people.min)

      import DefaultValues.optionOrdering
      println(sortThings(mayPeople))
  }


}

object DefaultValues {
  given defaultPersonOrdering: Ordering[Person] with
    override def compare(x: Person, y: Person): Int =
      x.surname.compareTo(y.surname)

  given optionOrdering[T] (using ordering: Ordering[T]): Ordering[Option[T]] with
    override def compare(x: Option[T], y: Option[T]): Int =
      (x, y) match
        case (None, None) => 0
        case (None, _) => 1
        case (_, None) => -1
        case (Some(p1), Some(p2)) => ordering.compare(p1, p2)
}

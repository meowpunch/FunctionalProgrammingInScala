case class Person(surname: String, name: String, age: Int)

object Givens {
  private val personOrderingByName: Ordering[Person] = (x: Person, y: Person) => {
    x.name.compareTo(y.name)
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

      val ab = Person("A", "B", 1)
      val ba = Person("B", "A", 1)
      println(personOrderingByName.compare(ab, ba))
      println(defaultPersonOrdering.compare(ab, ba))

      println(listPeopleWithDefault(people))
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

package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite :

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))

    assert(contains(x => x > 0, 1))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets :
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets :
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  trait TestSets2 extends TestSets :
    val s4 = union(s1, s2) // 1, 2
    val s5 = union(s2, s3) // 2, 3
    val s6 = union(s4, s5) // 1, 2, 3

  test("interset contains all elements in both sets") {
    new TestSets2 :
      val s = intersect(s4, s5)
      assert(!contains(s, 1), "Intersect 1")
      assert(contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
  }

  test("diff") {
    new TestSets2 :
      assert(contains(diff(s5, s4), 3), "diff 3")
  }

  test("filter") {
    new TestSets2 :
      val s = filter(s5, x => x > 2)
      assert(!contains(s, 1))
      assert(!contains(s, 2))
      assert(contains(s, 3))
  }

  trait TestSets3 extends TestSets2 :
    val bigS = (-500 to 1500)
      .map(x => singletonSet(x))
      .reduce(union)

  test("bigS") {
    new TestSets3 :
      assert(contains(bigS, -500))
      assert(contains(bigS, 1500))
      assert(!contains(bigS, -501))
      assert(!contains(bigS, 1501))
  }

  test("forall") {
    new TestSets3 :
      // bigS [-500, 1500]
      assert(forall(bigS, x => -500 <= x && x <= 1500))
      assert(forall(bigS, x => -1000 <= x && x <= 1000))
      assert(!forall(bigS, x => -1000 <= x && x <= 500))
  }

  test("exist") {
    new TestSets3 :
      // bigS [-500, 1500]
      assert(exists(bigS, x => -1000 <= x && x <= -500))
      assert(!exists(bigS, x => -1000 <= x && x < -500))
      assert(!exists(bigS, x => 1000 < x && x <= 1500))
  }

  test("map") {
    new TestSets3 :
      // [-500, 1500], [0, 1500], [-1500, 0]
      val s = map(filter(bigS, x => x > 0), x => -x)
      assert(!contains(s, 500))
      assert(contains(s, -500))
      assert(!contains(s, -1500))
      
  }

  import scala.concurrent.duration.*

  override val munitTimeout = 10.seconds

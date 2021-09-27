package patmat

class HuffmanSuite extends munit.FunSuite:
  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    /*
     *    c => 1, a => 2, b => 3, d => 4
     *          cabd
     *        cab
     *      ca
     *    c   a  b  d
     */
    val t3 = createCodeTree("aabbbcdddd".toList)
  }


  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t2), List('a','b','d'))
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times aab") {
    assertEquals(times("aab".toList), List(('a', 2), ('b', 1)))
  }

  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton") {
    new TestTrees:
      assert(!singleton(List(t1, t2)))
      assert(singleton(List(t1)))
  }


  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("createTree from chars") {
    val charlist = "aabbbc".toList
    assertEquals(createCodeTree(charlist), Fork(Fork(Leaf('c',1),Leaf('a',2),List('c', 'a'),3), Leaf('b',3), List('c', 'a', 'b'), 6))
  }

  test("decode") {
    new TestTrees:
      assertEquals(decode(t3, List(0,0,0,1)), List('c','d'))
  }

  test("encode") {
    new TestTrees:
      assertEquals(encode(t3)(List('a','b')), List(0,0,1,0,1))
  }

  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }

  test("quickEncode") {
    new TestTrees:
      assertEquals(quickEncode(t3)(List('a','b')), List(0,0,1,0,1))
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds

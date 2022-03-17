package u03

import org.junit.*
import org.junit.Assert.*
import Streams.*
import Lists.*
import u02.Optionals.Option
import u02.Modules.*

class StreamTest:
  import Stream.*
  import List.*

  val s = Stream.take(Stream.iterate(0)( _ + 1))(10)

  @Test def testDrop() =
    assertEquals(Cons(6, Cons(7,Cons(8, Cons(9,Nil())))), toList(Stream.drop(s)(6)))

  @Test def testConstant() =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), toList(take(constant("x"))(5)))

  @Test def testFibonacci() =
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons (5, Cons(8, Cons(13, Nil())))))))), toList(take(fibonacci())(8)))
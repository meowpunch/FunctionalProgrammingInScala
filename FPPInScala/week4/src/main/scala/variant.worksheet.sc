// in scala, array is not covariant
val a: Array[Int] = Array(1, 2, 3)
val b: Array[AnyVal] = a // error
// Int is subtype of AnyVal. But,
// Array of Int is not subtype of Array of AnyVal

// List is covariant because of being immutable
// In Scala, Immutable types can be covariant
val c: List[Int] = List(1, 2, 3)
val d: List[AnyVal] = c

class Covariance[+T]:
  def apply(): T = ???
  def update(x: T) = ??? // error
  def update2[U >: T](x: U) = ???
class Nonvariance[T]:
  def apply(): T = ???
  def update(x: T) = ???
class Contravariance[-T]:
  def apply(): T = ??? // error
  def apply2[U <: T](): U = ???
  def update(x: T) = ???

// Covariant type parameters can only appear in method result
// Contravariant type parameters can only appear in method parameters
// Invaraint type parameters can appear anywhere

class AutoMobile

class Car extends AutoMobile
class Boat extends AutoMobile

val cov1: Covariance[AutoMobile] = new Covariance[Car]
val cov2: Covariance[Car] = new Covariance[AutoMobile] //error!

val contv1: Contravariance[AutoMobile] = new Contravariance[Car] //error!
val contv2: Contravariance[Car] = new Contravariance[AutoMobile]


// List is Covariant
val l1 = List(AutoMobile(), Car())
val l2 = List(Car(), Boat())
//val l2 =
l2.appended(AutoMobile())
l2.prepended(AutoMobile())

List
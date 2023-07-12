object Monads {

  case class SafeValue[+T](private val internalValue: T) { // "constructor" = pure or unit
    def get: T = synchronized {
      // does something interesting
      internalValue
    }

    def transform[S](transformer: T => SafeValue[S]): SafeValue[S] = synchronized {
      transformer.apply(internalValue)
    }

  }

  // as external API
  def gimmeSafeValue[T](value: T): SafeValue[T] = SafeValue(value)

  // ETW
  val safeString = gimmeSafeValue("Fun to learn scala")

  // extract
  val string = safeString.get
  // transform
  val upperCaseString = string.toUpperCase
  // wrap
  val safeUpperCaseString = gimmeSafeValue(upperCaseString)


  val safeUpperCaseStringFromTransformer = safeString.transform(s => SafeValue(s.toUpperCase))


}

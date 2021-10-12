class Polynom(nonZeroTerm: Map[Int, Double]):
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = nonZeroTerm.withDefaultValue(0.0)

  def +(other: Polynom): Polynom =
    Polynom(other.terms.foldLeft(terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] =
    val (exp, coeff) = term
    terms + (exp -> (terms(exp) + coeff))


  //    Polynom(
  //      terms ++ other.terms.map((exp, coeff) => (exp, terms(exp) + coeff))
  //    )

  //    Polynom((this.terms.toList ++ other.terms.toList).groupBy(_._1).map {
  //      case (k, v) => (k, v.map(_._2).sum)
  //    }.toMap)

  override def toString: String = {
    lazy val termStrings =
      for {
        (exp, coeff) <- terms.toList.sorted.reverse
      } yield {
        val exponent = if exp == 0 then "" else s"x^$exp"
        s"$coeff$exponent"
      }

    if terms.isEmpty then "0"
    else termStrings.mkString(" + ")
  }

0 -> 2
val p1 = Polynom(0 -> 2, 1 -> 3, 2 -> 1)
val p2 = Polynom(0 -> 3, 1 -> 2, 3 -> 1)
p1 + p2
Polynom(Map())
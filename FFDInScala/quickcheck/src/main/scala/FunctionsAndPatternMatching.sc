enum JSON:
  case Seq (elems: List[JSON])
  case Obj (bindings: Map[String, JSON])

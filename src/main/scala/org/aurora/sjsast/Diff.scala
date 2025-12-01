package org.aurora.sjsast

/**
  * Generic diffing utilities for PCM structures.
  *
  * Presence marks whether a node exists on the left, right, both (and equal),
  * or both but different. DiffNode forms a tree that mirrors the PCM shape.
  */
object Diff:

  enum Presence:
    case LeftOnly, RightOnly, Same, Different

  case class DiffNode[T](
    path: List[String],
    left: Option[T],
    right: Option[T],
    status: Presence,
    children: List[DiffNode[_]] = Nil
  )

  trait Differ[T]:
    def diff(path: List[String], left: Option[T], right: Option[T]): DiffNode[T]

  def diff[T](left: T, right: T)(using d: Differ[T]): DiffNode[T] =
    d.diff(Nil, Some(left), Some(right))

  private def presence[T](left: Option[T], right: Option[T], equals: Boolean): Presence =
    (left, right) match
      case (Some(_), None) => Presence.LeftOnly
      case (None, Some(_)) => Presence.RightOnly
      case (Some(_), Some(_)) =>
        if equals then Presence.Same else Presence.Different
      case (None, None) => Presence.Same

  private def aggregateStatus[T](left: Option[T], right: Option[T], children: List[DiffNode[_]]): Presence =
    val equals = (for
      l <- left
      r <- right
    yield l == r).getOrElse(false)

    val base = presence(left, right, equals)
    if base == Presence.Same && children.exists(_.status != Presence.Same) then Presence.Different
    else base

  private def diffMap[T](pathPrefix: List[String], left: Map[String, T], right: Map[String, T])(using d: Differ[T]): List[DiffNode[_]] =
    (left.keySet ++ right.keySet).toList.map { key =>
      d.diff(pathPrefix :+ key, left.get(key), right.get(key))
    }

  private def diffSet[T <: SjsNode](pathPrefix: List[String], left: Set[T], right: Set[T])(using d: Differ[T]): List[DiffNode[_]] =
    val leftMap  = left.map(n => n.name -> n).toMap
    val rightMap = right.map(n => n.name -> n).toMap
    diffMap(pathPrefix, leftMap, rightMap)

  // PCM and top-level CIO routing
  given Differ[PCM] with
    def diff(path: List[String], left: Option[PCM], right: Option[PCM]): DiffNode[PCM] =
      val lCio = left.map(_.cio).getOrElse(Map.empty)
      val rCio = right.map(_.cio).getOrElse(Map.empty)
      val children = diffCio(path, lCio, rCio)
      DiffNode(path, left, right, aggregateStatus(left, right, children), children)

  private def diffCio(pathPrefix: List[String], left: Map[String, CIO], right: Map[String, CIO]): List[DiffNode[_]] =
    (left.keySet ++ right.keySet).toList.map { key =>
      key match
        case "Orders" =>
          given Differ[Orders] = ordersDiffer
          ordersDiffer.diff(pathPrefix :+ key, left.get(key).collect { case o: Orders => o }, right.get(key).collect { case o: Orders => o })
        case "Issues" =>
          given Differ[Issues] = issuesDiffer
          issuesDiffer.diff(pathPrefix :+ key, left.get(key).collect { case i: Issues => i }, right.get(key).collect { case i: Issues => i })
        case "Clinical" =>
          given Differ[Clinical] = clinicalDiffer
          clinicalDiffer.diff(pathPrefix :+ key, left.get(key).collect { case c: Clinical => c }, right.get(key).collect { case c: Clinical => c })
        case other =>
          DiffNode(pathPrefix :+ other, left.get(other), right.get(other), Presence.Different, Nil)
    }

  // Orders
  private val ordersDiffer: Differ[Orders] = new Differ[Orders]:
    def diff(path: List[String], left: Option[Orders], right: Option[Orders]): DiffNode[Orders] =
      val lSet = left.map(_.ngo).getOrElse(Set.empty)
      val rSet = right.map(_.ngo).getOrElse(Set.empty)
      val children = diffSet(path, lSet, rSet)(using ngoDiffer)
      DiffNode(path, left, right, aggregateStatus(left, right, children), children)

  private val ngoDiffer: Differ[NGO] = new Differ[NGO]:
    def diff(path: List[String], left: Option[NGO], right: Option[NGO]): DiffNode[NGO] =
      val lCoords = left.map(_.orderCoordinates).getOrElse(Set.empty)
      val rCoords = right.map(_.orderCoordinates).getOrElse(Set.empty)

      val coordChildren = diffSet(path, lCoords, rCoords)(using orderCoordinateDiffer)
      val refChild = quReferencesDiffer.diff(path :+ "refs", left.map(_.quRefs), right.map(_.quRefs))
      val children = refChild :: coordChildren
      DiffNode(path, left, right, aggregateStatus(left, right, children), children)

  private val orderCoordinateDiffer: Differ[OrderCoordinate] = new Differ[OrderCoordinate]:
    def diff(path: List[String], left: Option[OrderCoordinate], right: Option[OrderCoordinate]): DiffNode[OrderCoordinate] =
      val refChild = quReferencesDiffer.diff(path :+ "refs", left.map(_.refs), right.map(_.refs))
      val children = List(refChild)
      DiffNode(path, left, right, aggregateStatus(left, right, children), children)

  // Issues
  private val issuesDiffer: Differ[Issues] = new Differ[Issues]:
    def diff(path: List[String], left: Option[Issues], right: Option[Issues]): DiffNode[Issues] =
      val lSet = left.map(_.ics).getOrElse(Set.empty)
      val rSet = right.map(_.ics).getOrElse(Set.empty)
      val children = diffSet(path, lSet, rSet)(using issueCoordinateDiffer)
      DiffNode(path, left, right, aggregateStatus(left, right, children), children)

  private val issueCoordinateDiffer: Differ[IssueCoordinate] = new Differ[IssueCoordinate]:
    def diff(path: List[String], left: Option[IssueCoordinate], right: Option[IssueCoordinate]): DiffNode[IssueCoordinate] =
      DiffNode(path, left, right, aggregateStatus(left, right, Nil), Nil)

  // Clinical
  private val clinicalDiffer: Differ[Clinical] = new Differ[Clinical]:
    def diff(path: List[String], left: Option[Clinical], right: Option[Clinical]): DiffNode[Clinical] =
      val lSet = left.map(_.ngc).getOrElse(Set.empty)
      val rSet = right.map(_.ngc).getOrElse(Set.empty)
      val children = diffSet(path, lSet, rSet)(using ngcDiffer)
      DiffNode(path, left, right, aggregateStatus(left, right, children), children)

  private val ngcDiffer: Differ[NGC] = new Differ[NGC]:
    def diff(path: List[String], left: Option[NGC], right: Option[NGC]): DiffNode[NGC] =
      val lSet = left.map(_.ccoords).getOrElse(Set.empty)
      val rSet = right.map(_.ccoords).getOrElse(Set.empty)
      val children = diffSet(path, lSet, rSet)(using clinicalCoordinateValueDiffer)
      val refChild = quReferencesDiffer.diff(path :+ "refs", left.map(_.quRefs), right.map(_.quRefs))
      val allChildren = refChild :: children
      DiffNode(path, left, right, aggregateStatus(left, right, allChildren), allChildren)

  private val clinicalCoordinateValueDiffer: Differ[ClinicalCoordinateValue] = new Differ[ClinicalCoordinateValue]:
    def diff(path: List[String], left: Option[ClinicalCoordinateValue], right: Option[ClinicalCoordinateValue]): DiffNode[ClinicalCoordinateValue] =
      DiffNode(path, left, right, aggregateStatus(left, right, Nil), Nil)

  // References
  private val quReferencesDiffer: Differ[QuReferences] = new Differ[QuReferences]:
    def diff(path: List[String], left: Option[QuReferences], right: Option[QuReferences]): DiffNode[QuReferences] =
      val lSet = left.map(_.refs).getOrElse(Set.empty)
      val rSet = right.map(_.refs).getOrElse(Set.empty)
      val children = diffSet(path, lSet, rSet)(using quReferenceDiffer)
      DiffNode(path, left, right, aggregateStatus(left, right, children), children)

  private val quReferenceDiffer: Differ[QuReference] = new Differ[QuReference]:
    def diff(path: List[String], left: Option[QuReference], right: Option[QuReference]): DiffNode[QuReference] =
      DiffNode(path, left, right, aggregateStatus(left, right, Nil), Nil)

end Diff

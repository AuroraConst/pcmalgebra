package org.aurora.sjsast
import cats.Show
import cats.syntax.show._ 

object ShowAurora:
  val newline = "\n"

  given Show[PCM] = Show.show { (p: PCM) =>

    // Helper: safely get CIO element, cast, and show it
    def showCioElement[A <: CIO](key: String)(using Show[A]): String =
      p.cio
        .get(key)
        .map(_.asInstanceOf[A])
        .map(_.show)
        .getOrElse("")

    val sections = List(
      showCioElement[Clinical]("Clinical"),
      showCioElement[Issues]("Issues"),
      showCioElement[Orders]("Orders")
    ).filter(_.nonEmpty)

    val cioShow = sections.mkString(newline)

    // If module exists, prefer module.show, else show CIO
    p.module.fold(cioShow)(_.show)
  }

  given Show[Clinical] = Show.show { c =>
    val groups  = c.ngc.map(_.show).mkString(newline)
    val narr    = c.narrative.map(_.name).mkString(" ")

    List(
      s"${c.name}:",
      narr,
      groups
    ).filter(_.nonEmpty)
    .mkString(newline)
  }


  given Show[Issues] =  Show.show{
      (i: Issues) => 
        val result = i.ics.map{(ic:IssueCoordinate) => ic.name}.mkString(newline)
        s"$result"
    }


  given Show[Orders] = Show.show{
      (o: Orders) => 
        val result = o.ngo.map{(ngo:NGO) => ngo.show}.mkString(newline)
        val name = o.name
        s"$name:$newline$result"
    }
  
  given Show[OrderCoordinate] = Show.show{
      (rc: OrderCoordinate) => 
        val quref = rc.refs.show
        val narratives = rc.narratives.mkString(" ")
        val name = rc.name
        s"$name($quref) $narratives"
      }
      
  given Show[NGO] = Show.show{
      (ng: NGO) => 
        val result = ng.orderCoordinates.map{(oc:OrderCoordinate) => oc.show}.mkString(newline)
        val name = ng.name
        s"$name$newline$result"
    }

  given Show[NGC] = Show.show { ngc =>
    val coordsShow = ngc.ccoords.map(_.show).mkString(newline)
    val narratives = ngc.narrative.map(_.name).mkString(" ")

    List(
      ngc.name,
      narratives,
      coordsShow
    ).filter(_.nonEmpty).mkString(newline)
  }

  given Show[ClinicalCoordinateValue] = Show.show{
      (ccv: ClinicalCoordinateValue) => 
        val refs = ccv.refs.map{(rc:RefCoordinate) => rc.name}.mkString(",")
        val narratives = ccv.narrative.map{(nl:NL_STATEMENT) => nl.name}.mkString(" ")
        val qus = ccv.qu.map{(qu:QU) => qu}.mkString(",")
        val name = ccv.name
        s"$name [refs: $refs] [narrative: $narratives] [qu: $qus]"
    }

  given Show[QuReference]  = Show.show{
    (ref:QuReference) =>
      val qu = ref.qu
      val name = ref.name
      s"$qu$name"
  }

  given Show[QuReferences] = Show.show{
    (quRef:QuReferences) =>
      val result = quRef.refs.map{(ref:QuReference) => ref.show}.mkString(",")
      s"$result"
  }

  given Show[Module] = Show.show { (m: Module) =>

    def showCioElement[A <: CIO](key: String)(using Show[A]): String =
      m.cio
        .get(key)
        .map(_.asInstanceOf[A])
        .map(_.show)
        .getOrElse("")

    val sections = List(
      showCioElement[Issues]("Issues"),
      showCioElement[Orders]("Orders"),
      // showCioElement[Clinical]("Clinical")
    ).filter(_.nonEmpty) 

    sections.mkString(newline)
  }

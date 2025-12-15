package org.aurora.sjsast

//TODO I think this is a better IR than using Map[String,Set[CIO]]
// case class PCM(c:Set[Clinical], o:Set[Orders], i:Set[Issues])

// Map[String,Set[CIO]]

case class PCM(cio:Map[String,CIO]) 

object PCM :      

    
  private def cioFromModuleOrElse (p:GenAst.PCM):Map[String,CIO] = 
    p.module.map{_.elements}
    .getOrElse(p.elements)
    .toList
      .map(x => x.$type -> x)
      .map{(t,o) =>
        t match {
          case "Issues" => t -> Issues.apply(o.asInstanceOf[GenAst.Issues])
          case "Orders" => t -> Orders.apply(o.asInstanceOf[GenAst.Orders])
          case "Clinical" => t -> Clinical.apply(o.asInstanceOf[GenAst.Clinical])
        }

      }.toMap

  def apply(p:GenAst.PCM) :PCM = 
    val cio = cioFromModuleOrElse(p)
    PCM(cio)


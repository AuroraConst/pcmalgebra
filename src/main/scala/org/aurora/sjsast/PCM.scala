package org.aurora.sjsast

import scala.annotation.targetName

type CIO = Clinical|Issues|Orders


//TODO note a PCM can be a Module or it can be Map[String,CIO] this begs the question whether
//the PCM should take one parameter of type Map[String,CIO] | Module
case class PCM(cio:Map[String,CIO], module:Option[Module]=None) extends SjsNode :
  override val name = "PCM"

  def merge(p:PCM):PCM = 
    PCM( cio |+| p.cio) 


  override def merge(p: SjsNode): SjsNode = merge(p.asInstanceOf[PCM])


object PCM :      
  def apply(cio: Map[String, CIO]): PCM =
    new PCM(cio, None)

  def apply(module: Module): PCM =
     new PCM(Map[String,CIO](), Some(module))  

  @targetName("applyFromSjsNode")
  def apply(map: Map[String, SjsNode]): PCM =
    val converted = map.collect {
      case (k, v: Clinical) => k -> v
      case (k, v: Issues)   => k -> v
      case (k, v: Orders)   => k -> v
    }
    PCM(converted)
    
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
    p.module.toOption.fold{PCM(cioFromModuleOrElse(p))}{m => PCM(Module(m))}


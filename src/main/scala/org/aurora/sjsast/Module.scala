package org.aurora.sjsast

import scala.annotation.targetName


case class Module(_name:String,cio:Map[String,CIO]) extends SjsNode :
  override val name = _name

  //TODO merging modules should be meaningless??
  def merge(p:Module):Module = 
    // PCM( cio |+| p.cio) 
    ???


  override def merge(p: SjsNode): SjsNode = merge(p.asInstanceOf[Module])


object Module :      
  def apply(m:GenAst.Module): Module =
    new Module(m.name, cioFromModuleElements(m))

  //TODO what is this annotation for?
  @targetName("applyFromSjsNode")
  def apply(map: Map[String, SjsNode]): Module =
    val converted = map.collect {
      case (k, v: Clinical) => k -> v
      case (k, v: Issues)   => k -> v
      case (k, v: Orders)   => k -> v
    }
    // PCM(converted)
    ???
    
  //converts string based type representations from typescript to Scala based types
  private def cioFromModuleElements (m:GenAst.Module):Map[String,CIO] = 
    m.elements
    .toList
      .map(x => x.$type -> x)
      .map{(t,o) =>
        t match {
          case "Issues" => t -> Issues.apply(o.asInstanceOf[GenAst.Issues])
          case "Orders" => t -> Orders.apply(o.asInstanceOf[GenAst.Orders])
          case "Clinical" => t -> Clinical.apply(o.asInstanceOf[GenAst.Clinical])
        }

      }.toMap

  //TODO ?delete
  // def apply(p:GenAst.PCM) :PCM = 
  //   val cio = cioFromModuleOrElse(p)
  //   PCM(cio)


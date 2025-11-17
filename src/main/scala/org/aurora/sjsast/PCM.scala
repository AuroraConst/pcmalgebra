package org.aurora.sjsast

import scala.annotation.targetName
import scala.scalajs.js

type CIO = Clinical|Issues|Orders

case class PCM(cio: Map[String, CIO]) extends SjsNode :
  override val name = "PCM"

  def merge(p:PCM):PCM = 
    PCM( cio |+| p.cio) 


  override def merge(p: SjsNode): SjsNode = merge(p.asInstanceOf[PCM])


object PCM :      
  def apply(cio: Map[String, CIO]): PCM =
    new PCM(cio)

  @targetName("applyFromSjsNode")
  def apply(map: Map[String, SjsNode]): PCM =
    val converted = map.collect {
      case (k, v: Clinical) => k -> v
      case (k, v: Issues)   => k -> v
      case (k, v: Orders)   => k -> v
    }
    PCM(converted)

  private def cioFromElements(elements: List[js.Dynamic]): Map[String, CIO] =
    elements.map { element =>
      val elementType = element.selectDynamic("$type").asInstanceOf[String]
      val mapped: CIO = elementType match
        case "Issues" => Issues.apply(element.asInstanceOf[GenAst.Issues])
        case "Orders" => Orders.apply(element.asInstanceOf[GenAst.Orders])
        case "Clinical" => Clinical.apply(element.asInstanceOf[GenAst.Clinical])
      elementType -> mapped
    }.toMap

  def apply(p: GenAst.PCM): PCM =
    p.module.toOption match
      case Some(_) =>
        throw new IllegalArgumentException(
          "PCM.`apply cannot handle module declarations. Use ModulePCM instead."
        )
      case None =>
        val elements = p.elements.toList.map(_.asInstanceOf[js.Dynamic])
        PCM(cioFromElements(elements))

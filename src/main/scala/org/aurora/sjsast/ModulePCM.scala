package org.aurora.sjsast

import scala.scalajs.js

/**
 * PCM implementation at module level.
 */
case class ModulePCM(module: Module) extends SjsNode:
  override val name: String = module.name

  def toPCM: PCM = PCM(module.cio)

  override def merge(p: SjsNode): SjsNode =
    throw new UnsupportedOperationException("Merging ModulePCM instances is not supported.")

object ModulePCM:
  def apply(p: GenAst.PCM): ModulePCM =
    val moduleAst = p.module.toOption.getOrElse {
      throw new IllegalArgumentException("PCM does not contain a module declaration.")
    }
    ModulePCM(Module(moduleAst))

package org.aurora.examples

import org.aurora.sjsast.*
import org.aurora.utils.fileutils
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import org.aurora.sjsast.ShowAurora.given
import cats.syntax.show._ 


object TestAliasing:

  def main(): Unit =
    val result = testAliasing()
    result.foreach { pcm =>
      println("=== Merged PCM ===")
      println(pcm.show)
    }

  def testAliasing(): Future[PCM] =
    // Paths to your aurora files
    val chfModulePath = "src/test/resources/chf_drkim.aurora"
    val miModulePath = "src/test/resources/MI_DrKim_2024.aurora"
    val testFilePath = "src/test/resources/Test.aurora"

    for {
      // Load modules
      chfLangium <- fileutils.parse(chfModulePath).toFuture
      miLangium <- fileutils.parse(miModulePath).toFuture
      // Load main file
      testLangium <- fileutils.parse(testFilePath).toFuture
    } yield {
      val modules = Map(
        "chf_drkim" -> ModulePCM(chfLangium).toPCM,
        "MI_DrKim_2024" -> ModulePCM(miLangium).toPCM
      )
      val testPCM = PCM(testLangium)

      // Merge with aliasing
      ModuleToPCM.mergeWithImports(testPCM, modules)
    }

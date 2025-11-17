package org.aurora

import org.aurora.sjsast.*
import scala.concurrent.Future
import org.aurora.sjsast.ShowAurora.given
import cats.syntax.show._ 
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * Example showing how to merge PCMs with module imports and aliasing.
 */
object ModuleMergeExample:

  /**
   * Load and merge multiple PCM files with module resolution.
   * 
   * @param mainFile Path to the main .aurora file
   * @param moduleFiles Map of module names to their file paths
   * @return Future containing the merged PCM
   */
  def loadAndMerge(
    mainFile: String, 
    moduleFiles: Map[String, String]
  ): Future[PCM] =
    for {
      // Load all module PCMs
      modulePCMs <- Future.sequence(
        moduleFiles.map { case (name, path) =>
          org.aurora.utils.fileutils.parse(path)
            .toFuture
            .map { langiumPCM =>
              val modulePCM = ModulePCM(langiumPCM)
              name -> modulePCM.toPCM
            }
        }
      ).map(_.toMap)
      
      // Load main file PCM
      mainLangiumPCM <- org.aurora.utils.fileutils.parse(mainFile).toFuture
      mainPCM = PCM(mainLangiumPCM)
      
      // Merge with imports resolved
      mergedPCM = ModuleToPCM.mergeWithImports(mainPCM, modulePCMs)
      
    } yield mergedPCM

  /**
   * Example usage with your CHF and MI modules.
   */
  def exampleUsage(): Future[Unit] =
    val moduleFiles = Map(
      "chf_drkim" -> "src/test/resources/chf_drkim.aurora",
      "MI_DrKim_2024" -> "src/test/resources/MI_DrKim_2024.aurora"
    )
    
    for {
      merged <- loadAndMerge("src/test/resources/Test.aurora", moduleFiles)
      _ = println("=== Merged PCM ===")
      _ = println(merged.show)
    } yield ()

  /**
   * Direct example without file loading (for testing).
   */
  def directExample(): PCM =
    // Simulate CHF module
    val chfOrders = Orders(
      Set(NGO(
        "Treatment",
        Set(
          OrderCoordinate("furosemide", Set.empty, QuReferences(Set(QuReference("", "chf"))))
        ),
        Set.empty,
        QuReferences(Set.empty),
        Set.empty
      )),
      Set.empty
    )
    val chfModule = PCM(Map(
      "Issues" -> Issues(Set(IssueCoordinate("chf", Set.empty)), Set.empty),
      "Orders" -> chfOrders
    ))

    // Simulate MI module  
    val miOrders = Orders(
      Set(NGO(
        "Treatment",
        Set(
          OrderCoordinate("asa", Set.empty, QuReferences(Set(QuReference("!!", "mi"))))
        ),
        Set.empty,
        QuReferences(Set.empty),
        Set.empty
      )),
      Set.empty
    )
    val miModule = PCM(Map(
      "Issues" -> Issues(Set(IssueCoordinate("mi", Set.empty)), Set.empty),
      "Orders" -> miOrders
    ))

    // Main file that imports with custom names
    val mainIssues = Issues(
      Set(
        IssueCoordinate("heart_failure", Set.empty, Some("chf_drkim")),
        IssueCoordinate("cardiac_event", Set.empty, Some("MI_DrKim_2024"))
      ),
      Set.empty
    )
    val mainPCM = PCM(Map("Issues" -> mainIssues))

    // Module registry
    val modules = Map(
      "chf_drkim" -> chfModule,
      "MI_DrKim_2024" -> miModule
    )

    // Merge with aliasing
    ModuleToPCM.mergeWithImports(mainPCM, modules)

end ModuleMergeExample

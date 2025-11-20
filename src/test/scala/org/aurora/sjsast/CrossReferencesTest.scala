package org.aurora.sjsast

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.Thenable.Implicits.*
import scala.scalajs.js.UndefOrOps

import typings.auroraLangium.distTypesSrcLanguageGeneratedAstMod.{
  PCM as LangiumPCM,
  Issues,
  MODULE
}
import typings.auroraLangium.distTypesSrcLanguageAuroraModuleMod.AuroraServices
import scala.scalajs.js
import typings.langium.libSyntaxTreeMod.AstNode
import typings.langium.libWorkspaceAstDescriptionsMod.ReferenceDescription
import typings.vscodeUri.mod.URI
import typings.langium.libWorkspaceDocumentsMod.LangiumDocument

class CrossReferencesTest extends BaseAsyncTest:
  private val moduleText =
    """module: Thunder_Bay_Regional_CHF
      |
      |Issues:
      |ICBase : base
      |""".stripMargin.trim

  private val referencingText =
    """Issues:
      |IC1 : ic1 from Thunder_Bay_Regional_CHF
      |""".stripMargin.trim

  // Reuse the CLI helper so that Langium services are configured once for all tests.
  private lazy val auroraServicesF: Future[AuroraServices] =
    // use a dynamic require to avoid relying on a typed member that may not exist in the generated facade
    js.Dynamic.global.require("aurora-langium/cli")
      .asInstanceOf[js.Dynamic]
      .selectDynamic("getAuroraServices")()
      .asInstanceOf[js.Thenable[js.Dynamic]]
      .toFuture
      .map(_.Aurora.asInstanceOf[AuroraServices])

  "Aurora cross references" should {
    "find references to module across documents" in {
      for {
        services <- auroraServicesF
        moduleDoc <- parseAuroraDocument(services, moduleText, "file:///module.aurora")
        referencingDoc <- parseAuroraDocument(services, referencingText, "file:///referencing.aurora")
        _ <- services.shared.workspace.DocumentBuilder
          .build(js.Array(referencingDoc, moduleDoc))
          .toFuture
      } yield {
        val modulePCM = moduleDoc.parseResult.value.asInstanceOf[LangiumPCM]
        val moduleNode = modulePCM.module.toOption.getOrElse(fail("Expected module definition"))

        val references = collectReferences(services, moduleNode)
        references should have length 1
        references.head.sourceUri.toString() should be("file:///referencing.aurora")
        rangeString(references.head) should be("1:15->1:39")

        val referencingPCM = referencingDoc.parseResult.value.asInstanceOf[LangiumPCM]
        val issues = referencingPCM.elements.toSeq
          .collectFirst {
            case issue if issue.asInstanceOf[js.Dynamic].selectDynamic("$type").asInstanceOf[String] == "Issues" =>
              issue.asInstanceOf[Issues]
          }
          .getOrElse(fail("Expected Issues block"))

        val coord = issues.coord.headOption.getOrElse(fail("Expected at least one issue coordinate"))
        coord.mods.length should be(1)

        val resolvedModule = coord.mods(0).ref.toOption.getOrElse(fail("Expected resolved module reference"))
        resolvedModule.name should be(moduleNode.name)
      }
    }
  }

  private def parseAuroraDocument(
      services: AuroraServices,
      content: String,
      uri: String
  ): Future[LangiumDocument[LangiumPCM]] =
    val parsedUri = URI.parse(uri)
    val documentFactory = services.shared.workspace.LangiumDocumentFactory
    val document = documentFactory
      .asInstanceOf[js.Dynamic]
      .applyDynamic("fromString")(content, parsedUri)
      .asInstanceOf[LangiumDocument[LangiumPCM]]
    val baseDocument = document.asInstanceOf[LangiumDocument[AstNode]]
    services.shared.workspace.LangiumDocuments.addDocument(baseDocument)
    services.shared.workspace.DocumentBuilder
      .build(js.Array(baseDocument))
      .toFuture
      .map(_ => document)

  private def collectReferences(services: AuroraServices, node: AstNode): Seq[ReferenceDescription] =
    services.shared.workspace.IndexManager
      .findAllReferences(node, nodePath(services, node))
      .toArray()
      .toSeq

  private def rangeString(ref: ReferenceDescription): String =
    val start = ref.segment.range.start
    val end = ref.segment.range.end
    s"${start.line}:${start.character}->${end.line}:${end.character}"

  private def nodePath(services: AuroraServices, node: AstNode): String =
    services.workspace.AstNodeLocator.getAstNodePath(node)

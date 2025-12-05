package org.aurora.sjsast

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.Thenable.Implicits.*

import typings.auroraLangium.cliMod.getEmptyAuroraServices
import typings.auroraLangium.distTypesSrcLanguageAuroraModuleMod.AuroraServices
import typings.auroraLangium.cliMod.{GenAst as LangiumGenAst}
import typings.langium.libSyntaxTreeMod.AstNode
import typings.langium.libWorkspaceAstDescriptionsMod.ReferenceDescription
import typings.langium.libWorkspaceDocumentBuilderMod.BuildOptions
import typings.langium.libWorkspaceDocumentsMod.LangiumDocument
import typings.vscodeUri.mod.URI

class CrossRefTest extends BaseAsyncTest:

  private val moduleText =
    """module: Thunder_Bay_Regional_CHF
      |
      |Issues:
      |ICBase : base""".stripMargin

  private val referencingText =
    """Issues:
      |IC1 : ic1 from Thunder_Bay_Regional_CHF""".stripMargin

  private val brokenReferenceText =
    """Issues:
      |IC1 : ic1 from Thunder_Bay_Regional_CHF""".stripMargin

  "Aurora cross references" should {

    "find references to module across documents" in {
      val entries = Seq(
        "file:///module.aurora" -> moduleText,
        "file:///referencing.aurora" -> referencingText
      )

      withAuroraServices { services =>
        for
          docs <- buildAuroraWorkspace(services, entries)
          moduleDoc = docs("file:///module.aurora")
          referencingDoc = docs("file:///referencing.aurora")
          _ = moduleDoc.diagnostics.toOption.fold(0)(_.length) shouldBe 0
          _ = referencingDoc.diagnostics.toOption.fold(0)(_.length) shouldBe 0
          modulePCM = moduleDoc.parseResult.value.asInstanceOf[js.Dynamic]
          moduleNodeDynamic = modulePCM
            .selectDynamic("module")
            .asInstanceOf[js.UndefOr[js.Dynamic]]
            .toOption
            .getOrElse(fail("Expected module in module PCM"))
          moduleNode = moduleNodeDynamic.asInstanceOf[AstNode]
          references = collectReferences(services, moduleNode)
          _ = references.length shouldBe 1
          reference = references.head
          _ = reference.sourceUri.toString shouldBe "file:///referencing.aurora"
          _ = rangeString(reference) shouldBe "1:15->1:39"
          referencingPCM = referencingDoc.parseResult.value.asInstanceOf[js.Dynamic]
          issues = firstIssues(referencingPCM)
          coordArray = issues.selectDynamic("coord").asInstanceOf[js.Array[js.Dynamic]]
          coord = coordArray.toSeq.lift(0).getOrElse(fail("Expected issue coordinate"))
          modsArray = coord.selectDynamic("mods").asInstanceOf[js.Array[js.Dynamic]]
          _ = modsArray.length shouldBe 1
          resolvedName = modsArray
            .toSeq
            .lift(0)
            .flatMap(mod =>
              mod
                .selectDynamic("ref")
                .asInstanceOf[js.UndefOr[js.Dynamic]]
                .toOption
                .map(_.selectDynamic("name").asInstanceOf[String])
            )
            .getOrElse(fail("Expected resolved module reference"))
        yield {
          val moduleName = moduleNodeDynamic.selectDynamic("name").asInstanceOf[String]
          resolvedName shouldBe moduleName
        }
      }
    }

    "flag unresolved reference when target module is missing" in {
      val entries = Seq(
        "file:///broken-reference.aurora" -> brokenReferenceText
      )

      withAuroraServices { services =>
        for
          docs <- buildAuroraWorkspace(services, entries)
          referencingDoc = docs("file:///broken-reference.aurora")
          diagnosticsCount = referencingDoc.diagnostics.toOption.fold(0)(_.length)
          _ = diagnosticsCount should be > 0
          brokenPCM = referencingDoc.parseResult.value.asInstanceOf[js.Dynamic]
          issues = firstIssues(brokenPCM)
          coordArray = issues.selectDynamic("coord").asInstanceOf[js.Array[js.Dynamic]]
          coord = coordArray.toSeq.lift(0).getOrElse(fail("Expected issue coordinate"))
          modsArray = coord.selectDynamic("mods").asInstanceOf[js.Array[js.Dynamic]]
          _ = modsArray.length shouldBe 1
          firstMod = modsArray.toSeq.lift(0).getOrElse(fail("Expected module reference"))
          refOption = firstMod
            .selectDynamic("ref")
            .asInstanceOf[js.UndefOr[js.Dynamic]]
            .toOption
          errorOption = firstMod
            .selectDynamic("error")
            .asInstanceOf[js.UndefOr[js.Dynamic]]
            .toOption
        yield {
          refOption shouldBe empty
          errorOption.isDefined shouldBe true
        }
      }
    }
  }

  private def withAuroraServices[A](f: AuroraServices => Future[A]): Future[A] =
    getEmptyAuroraServices().toFuture.flatMap(bundle => f(bundle.Aurora))

  private def buildAuroraWorkspace(services: AuroraServices, entries: Seq[(String, String)]): Future[Map[String, LangiumDocument[AstNode]]] =
    val factory = services.shared.workspace.LangiumDocumentFactory
    val documents = services.shared.workspace.LangiumDocuments

    val docs: Seq[(String, LangiumDocument[AstNode])] = entries.map { case (uri, content) =>
      val parsedUri = URI.parse(uri).asInstanceOf[typings.langium.libUtilsUriUtilsMod.URI]
      val doc = factory.fromString[AstNode](content, parsedUri)
      documents.addDocument(doc)
      uri -> doc
    }

    val docArray: js.Array[LangiumDocument[AstNode]] = js.Array(docs.map(_._2)*)
    val buildOptions = (new js.Object).asInstanceOf[BuildOptions]
    buildOptions.validation = true

    services.shared.workspace.DocumentBuilder
      .build(docArray, buildOptions)
      .toFuture
      .map(_ => docs.toMap)

  private def collectReferences(services: AuroraServices, node: AstNode): Seq[ReferenceDescription] =
    val stream = services.shared.workspace.IndexManager.findAllReferences(node, nodePath(services, node))
    stream.toArray().toSeq

  private def rangeString(ref: ReferenceDescription): String =
    val start = ref.segment.range.start
    val end = ref.segment.range.end
    s"${start.line.toInt}:${start.character.toInt}->${end.line.toInt}:${end.character.toInt}"

  private def nodePath(services: AuroraServices, node: AstNode): String =
    services.workspace.AstNodeLocator.getAstNodePath(node)

  private def firstIssues(pcm: js.Dynamic): js.Dynamic =
    val elements = pcm.selectDynamic("elements").asInstanceOf[js.Array[js.Dynamic]]
    elements
      .toSeq
      .find(elem => LangiumGenAst.isIssues(elem.asInstanceOf[js.Any]))
      .getOrElse(fail("Expected Issues block"))
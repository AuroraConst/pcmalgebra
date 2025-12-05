package org.aurora.sjsast
import scala.concurrent.Future
import scala.scalajs.js.Thenable.Implicits.*
import org.aurora.sjsast.Diff.{DiffNode, Presence}

/**
  * policy  based validation test
  */
class SakiPolicyValidationTest extends BaseAsyncTest:

  // base folder where the policy and candidate PCM fixtures live
  private val sakiPath =
    s"${fileutils.testResourcesPath}${fileutils.separator}org${fileutils.separator}aurora${fileutils.separator}sjsast${fileutils.separator}saki"

  // design time policy PCM
  private val policyPath    = s"$sakiPath${fileutils.separator}chf_policy.aurora"
  // candidate PCM used to trigger missing-coordinate detection
  private val candidatePath = s"$sakiPath${fileutils.separator}chf_candidate.aurora"


  /**
    * while inspecting the candiate PCM against the policy PCM,
    * we are expecting to find missing order coordinates for issue "a"
    */
  "Policy PCM" should {
    "report missing coordinates and suggest fixes for issue 'a'" in {
      for {
        policyAst    <- fileutils.parse(policyPath).toFuture
        candidateAst <- fileutils.parse(candidatePath).toFuture
        policyPcm     = PCM(policyAst)
        candidatePcm  = PCM(candidateAst)
        missing        = missingCoordinatesViaDiff(policyPcm, candidatePcm, "a")
        suggestions    = suggestFromPolicy(policyPcm, missing, "a")
        _             <- finfo(s"Missing for issue 'a': ${missing.mkString(", ")}")
        _             <- finfo(s"Suggested additions: ${suggestions.mkString(", ")}")
      } yield {
        missing should contain allOf ("a0", "a2", "b2", "c1")
        missing.size should be(4)
        suggestions shouldBe missing
      }
    }
  }




  /**
   * list all order coordinate names in a PCM that explicitly reference the given issue
   *
   * @param pcm     PCM to inspect
   * @param issueId issue identifier to match on
   * @return Set[String] coordinate names linked to the issue
   */
  def orderCoordsForIssue(pcm: PCM, issueId: String): Set[String] =
    pcm.cio
      .get("Orders")
      .collect { case o: Orders => o }
      .map { orders =>
        orders.ngo.flatMap { ng =>
          ng.orderCoordinates.collect {
            case oc if oc.refs.refs.exists(_.name == issueId) => oc.name
          }
        }
      }
      .getOrElse(Set.empty)

  /**
   * Diff-based detection for order coordinates that exist in the policy but
   * not the candidate for a given issue.
   */
  def missingCoordinatesViaDiff(policySpec: PCM, candidate: PCM, issue: String): Set[String] =
    val diffTree = PCM.diff(policySpec, candidate)
    collectMissing(diffTree, issue)

  private def collectMissing(node: DiffNode[_], issue: String): Set[String] =
    val here = node match
      case DiffNode(_, Some(oc: OrderCoordinate), None, Presence.LeftOnly, _) if oc.refs.refs.exists(_.name == issue) =>
        Set(oc.name)
      case _ => Set.empty

    val childrenHits = node.children.flatMap(ch => collectMissing(ch, issue)).toSet
    here ++ childrenHits


  /**
   * compute which order coordinates required by the policy PCM for a given issue
   * are missing from the candidate PCM, it highlights policy completeness gaps
   * before merges/validation.
   *
   * @param policySpec design/baseline PCM containing the required coordinates
   * @param candidate  PCM to check against the policy
   * @param issue      issue id to filter coordinates by
   * @return Set[String] of missing coordinate names
   */
  def missingCoordinates(policySpec: PCM, candidate: PCM, issue: String): Set[String] =
    orderCoordsForIssue(policySpec, issue) diff orderCoordsForIssue(candidate, issue)


  /**
   * suggest additions by intersecting the missing names with what the policy
   * actually defines for the given issue,, this aims to  keep suggestions aligned to
   * the design/baseline PCM
   *
   * @param policySpec design/baseline PCM containing allowed coordinates
   * @param missing    names not present in the candidate
   * @param issue      issue id to filter coordinates by
   * @return Set[String] of suggested coordinate names
   */
  def suggestFromPolicy(policySpec: PCM, missing: Set[String], issue: String): Set[String] =
    missing intersect orderCoordsForIssue(policySpec, issue)

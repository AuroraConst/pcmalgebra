package org.aurora.sjsast

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.show._ // for .show

class ModuleMergeTest extends AnyFlatSpec with Matchers {

  "ModuleToPCM" should "extract correct alias mappings" in {
    // Create a module with issue named "chf"
    val sourceIssues = Issues(Set(IssueCoordinate("chf", Set.empty)), Set.empty)
    val modulePCM = PCM(Map("Issues" -> sourceIssues))

    // Show source module content
    info(s"Created source module PCM with Issues: ${modulePCM.show}")

    // Create target with alias "heart_failure" importing from "chf_drkim"
    val targetIssues = Issues(
      Set(IssueCoordinate("heart_failure", Set.empty, Some("chf_drkim"))),
      Set.empty
    )

    val modules = Map("chf_drkim" -> modulePCM)

    val aliasMappings = ModuleToPCM.extractAliasMappings(targetIssues, modules)
    info(s"Alias mappings extracted: $aliasMappings")
    aliasMappings.get("chf_drkim") should be(Some(Map("chf" -> "heart_failure")))
  }

  "ModuleToPCM" should "rewrite references when converting module" in {
    // Create module with Orders referencing "chf"
    val orderCoord = OrderCoordinate(
        "furosemide",
        Set.empty,
        QuReferences(Set(QuReference("??", "chf")))
    )
    val ngo = NGO("Treatment", Set(orderCoord), Set.empty, QuReferences(Set.empty), Set.empty)
    val orders = Orders(Set(ngo), Set.empty)
    
    val modulePCM = PCM(Map("Orders" -> orders))
    
    // Apply alias mapping
    val aliasMap = Map("chf" -> "heart_failure")
    val converted = ModuleToPCM.convertWithAliases(modulePCM, aliasMap)
    
    // Extract converted references
    val convertedOrders = converted.cio.get("Orders").get.asInstanceOf[Orders]
    val convertedNGO = convertedOrders.ngo.head
    val convertedCoord = convertedNGO.orderCoordinates.head
    val convertedRef = convertedCoord.refs.refs.head

    info(s"Original reference 'chf' rewritten as: ${convertedRef.name}")
    convertedRef.name should be("heart_failure")
    }

}

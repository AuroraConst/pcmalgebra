package org.aurora.sjsast

/**
 * Handles conversion of modules to PCMs with reference aliasing support.
 * This allows importing modules with custom variable names.
 */
object ModuleToPCM:
  
    /**
     * Convert a module to a PCM, applying alias mappings to all references.
     * 
     * @param module The module PCM to convert
     * @param aliasMap Mapping from original names to new names (e.g., "chf" -> "heart_failure")
     * @return A new PCM with all references rewritten according to the alias map
     */
    def convertWithAliases(module: PCM, aliasMap: Map[String, String]): PCM =
        if aliasMap.isEmpty then 
            module
        else
            val rewrittenCIO = module.cio.map { case (key, value) =>
            val rewritten = value match {
                case orders: Orders => rewriteOrders(orders, aliasMap)
                case clinical: Clinical => rewriteClinical(clinical, aliasMap)
                case issues: Issues => issues // Don't rewrite Issues section
            }
            key -> rewritten
            }
            PCM(rewrittenCIO.asInstanceOf[Map[String, CIO]])

    /**
     * Extract alias mappings from an Issues section.
     * 
     * For each IssueCoordinate that imports from a module, find the original
     * issue name in that module and create a mapping: original -> alias
     * 
     * @param issues The Issues section containing import statements
     * @param modules Registry of available modules
     * @return Map of original names to alias names
     */
    def extractAliasMappings(issues: Issues, modules: Map[String, PCM]): Map[String, Map[String, String]] =
        issues.ics.flatMap { ic =>
            ic.importedFrom.map { moduleName =>
            modules.get(moduleName).flatMap { modulePCM =>
                // Find the original issue name in the source module
                modulePCM.cio.get("Issues").map { issuesNode =>
                val sourceIssues = issuesNode.asInstanceOf[Issues]
                // Get the first (should be only) IssueCoordinate from source
                sourceIssues.ics.headOption.map { originalIC =>
                    moduleName -> Map(originalIC.name -> ic.name)
                }
                }.flatten
            }
            }.flatten
        }.toMap

    /**
     * Merge a PCM with imported modules, handling aliasing automatically.
     * 
     * @param targetPCM The target PCM to merge into
     * @param modules Registry of available modules by name
     * @return Merged PCM with all imports resolved and aliases applied
     */
    def mergeWithImports(targetPCM: PCM, modules: Map[String, PCM]): PCM =
        // Extract Issues section from target
        val targetIssues = targetPCM.cio.get("Issues")
            .map(_.asInstanceOf[Issues])
            .getOrElse(Issues(Set.empty))

        // Get alias mappings per module
        val moduleAliasMappings = extractAliasMappings(targetIssues, modules)

        // DEBUG: Print mappings
        println(s"Alias mappings: $moduleAliasMappings")

        // Convert and merge each imported module
        val mergedWithImports = targetIssues.ics.foldLeft(targetPCM) { (accPCM, ic) =>
            ic.importedFrom match {
            case Some(moduleName) =>
                modules.get(moduleName) match {
                case Some(modulePCM) =>
                    // Get alias mapping for this module
                    val aliasMap = moduleAliasMappings.getOrElse(moduleName, Map.empty)
                    
                    // Convert module with aliases
                    val convertedModule = convertWithAliases(modulePCM, aliasMap)
                    
                    // Merge with accumulator
                    accPCM.merge(convertedModule)
                
                case None =>
                    println(s"Warning: Module '$moduleName' not found")
                    accPCM
                }
            case None => accPCM
            }
        }

        mergedWithImports

    // Private helper methods for rewriting different node types

    private def rewriteOrders(orders: Orders, aliasMap: Map[String, String]): Orders =
        val rewrittenNGO = orders.ngo.map(ngo => rewriteNGO(ngo, aliasMap))
        orders.copy(ngo = rewrittenNGO)

    private def rewriteClinical(clinical: Clinical, aliasMap: Map[String, String]): Clinical =
        val rewrittenNGC = clinical.ngc.map(ngc => rewriteNGC(ngc, aliasMap))
        clinical.copy(ngc = rewrittenNGC)

    private def rewriteNGO(ngo: NGO, aliasMap: Map[String, String]): NGO =
        val rewrittenCoords = ngo.orderCoordinates.map(oc => rewriteOrderCoordinate(oc, aliasMap))
        val rewrittenRefs = rewriteQuReferences(ngo.quRefs, aliasMap)
        ngo.copy(orderCoordinates = rewrittenCoords, quRefs = rewrittenRefs)

    private def rewriteNGC(ngc: NGC, aliasMap: Map[String, String]): NGC =
        val rewrittenCoords = ngc.ccoords.map(ccv => rewriteClinicalCoordinateValue(ccv, aliasMap))
        val rewrittenRefs = rewriteQuReferences(ngc.quRefs, aliasMap)
        ngc.copy(ccoords = rewrittenCoords, quRefs = rewrittenRefs)

    private def rewriteOrderCoordinate(oc: OrderCoordinate, aliasMap: Map[String, String]): OrderCoordinate =
        val rewrittenRefs = rewriteQuReferences(oc.refs, aliasMap)
        oc.copy(refs = rewrittenRefs)

    private def rewriteClinicalCoordinate(cc: ClinicalCoordinate, aliasMap: Map[String, String]): ClinicalCoordinate =
        val rewrittenRefs = rewriteQuReferences(cc.refs, aliasMap)
        cc.copy(refs = rewrittenRefs)

    private def rewriteClinicalValue(cv: ClinicalValue, aliasMap: Map[String, String]): ClinicalValue =
        val rewrittenRefs = rewriteQuReferences(cv.qurefs, aliasMap)
        cv.copy(qurefs = rewrittenRefs)

    private def rewriteClinicalCoordinateValue(ccv: ClinicalCoordinateValue, aliasMap: Map[String, String]): ClinicalCoordinateValue =
        val rewrittenRefs = ccv.refs.map(rc => rewriteRefCoordinate(rc, aliasMap))
        ccv.copy(refs = rewrittenRefs)

    private def rewriteQuReferences(qrs: QuReferences, aliasMap: Map[String, String]): QuReferences =
        val rewrittenRefs = qrs.refs.map(qr => rewriteQuReference(qr, aliasMap))
        qrs.copy(refs = rewrittenRefs)

    private def rewriteQuReference(qr: QuReference, aliasMap: Map[String, String]): QuReference =
        val newName = aliasMap.getOrElse(qr.name, qr.name)
        qr.copy(name = newName)

    private def rewriteRefCoordinate(rc: RefCoordinate, aliasMap: Map[String, String]): RefCoordinate =
        val newName = aliasMap.getOrElse(rc.name, rc.name)
        rc.copy(name = newName)

end ModuleToPCM
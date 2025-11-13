package org.aurora.sjsast

case class IssueCoordinate(
  name: String, 
  narratives: Set[String],
  importedFrom: Option[String] = None  // NEW: Track which module this imports from
) extends SjsNode:

  def merge(i: IssueCoordinate): IssueCoordinate =  
    val narratives = this.narratives |+| i.narratives
    IssueCoordinate(i.name, narratives, i.importedFrom.orElse(this.importedFrom))

  override def merge(p: SjsNode): SjsNode = merge(p.asInstanceOf[IssueCoordinate])

object IssueCoordinate:
  def apply(i: GenAst.IssueCoordinate): IssueCoordinate = 
    val narratives = i.narrative.toList.map{n => n.name}.toSet
    
    // NEW: Extract the first module reference if it exists
    val importedFrom = try {
      i.mods.headOption.flatMap { modRef =>
        Option(modRef.ref).flatMap { ref =>
          ref.toOption.map(_.name)
        }
      }
    } catch {
      case _: Exception => None
    }
    
    IssueCoordinate(i.name, narratives, importedFrom)
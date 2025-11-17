package org.aurora.sjsast

import scala.concurrent.Future

import typings.auroraLangium.cliMod.{getEmptyAuroraServices, extractAstNode, getAuroraServices}
import typings.auroraLangium.cliMod.extractAstNode
import scala.scalajs.js
import typings.langium.langiumStrings.langium
import scala.scalajs.js.JSConverters._ 

class ArnoldModuleTest extends BaseAsyncTest:
  lazy val emptyServices = getAuroraServices()
  def finfo(output:String) =  Future(info(s"$output"))
  // def parse1PCM(filename:String) =  
  
  

  "ArnoldModule-0" should {
    import ShowAurora.given
    import cats.Show
    import cats.syntax.show._ 


    "be a module" in { 
      //TODO note if there is no file named ArnoldModule-0.aurora, it will automatically create one
      //SHOULD TEST FILE NAMING BE MORE DESCRIPTIVE e.g. ArnoldModule-Valid.aurora

      for {
        astPCM      <- parse(0)
        modulePCM   <- Future(ModulePCM(astPCM))
        assertName  <- Future(modulePCM.module.name should be("chf_2024"))
        _           <- finfo(s"${modulePCM.show}")
      } yield assertName
    } 
  }

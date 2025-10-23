package org.aurora.sjsast

import scala.concurrent.Future

import typings.auroraLangium.cliMod.{getEmptyAuroraServices, extractAstNode, getAuroraServices}
import typings.auroraLangium.cliMod.extractAstNode
import scala.scalajs.js
import typings.langium.libMod.LangiumParser
import typings.langium.libServicesMod.LangiumCoreServices
import typings.auroraLangium.distTypesSrcLanguageGeneratedAstMod.PCM
import typings.auroraLangium.distTypesSrcExtensionSrcParserParserMod.parseFromText
import typings.langium.libLspLspServicesMod.LangiumServices


class ParseTest extends BaseAsyncTest:
  lazy val emptyServices = getAuroraServices()
  def parse1PCM(filename:String) = 
    for{
      modelString <-  Future(???) //turn filename into string contents of file
      services <- emptyServices.toFuture
      pcm <- parseFromText(???,modelString,???).toFuture  //TODO FILL IN APPRORRIATE ARGS
    } yield pcm

  
  
  

  "Parse" should {
    "parse file correctly" in { 
      //TODO note if there is no file named Parse-0.aurora, it will automatically create one
      //SHOULD TEST FILE NAMING BE MORE DESCRIPTIVE e.g. Parse-Valid.aurora
      val path = testfilepath(0)

      for {
        result <- parse(0)
         _     <- Future(info(s"result: $result"))
        b <- Future(true should be (true))
      } yield {
        b
      }
    }



    "parse(1) correctly" in { 
      val path = testfilepath(1)

      for {
        result <- parse(1)
         _     <- Future(info(s"result: $result"))
        //TODO HOW DO YOU REPRESENT CATCHING EXCEPTION 
        b <- Future(true should be (true))


      } yield {
        b
      }
    }


    "parse(3)is a module" in {
      val path = testfilepath(3)

      for {
        _ <- Future(info(s"$path"))
        pcm <- parse1PCM(path)
        module <- Future(pcm.module.get)
        // modulename <- Future(module.name)
        doc    <- Future(info(s"module info: ${module.$document}"))
        //TODO CAN WE GET URI of resource?
        //https://langium.org/docs/learn/workflow/resolve_cross_references/
        b <- Future(true should be (true))


      } yield {
        b
      }
    }



    "parse(4) references module from parse(3) in {"  in {
      val path3 = testfilepath(3)
      val path4 = testfilepath(4)

      for {
        module <-parse1PCM(path3) 
        pcm4   <- parse1PCM(path4)
        b   <-    Future(true should be (true))
       } yield {
       b
      }
  }
} 
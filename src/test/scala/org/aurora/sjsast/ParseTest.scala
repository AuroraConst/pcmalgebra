package org.aurora.sjsast

import scala.concurrent.Future

import typings.auroraLangium.cliMod.{getEmptyAuroraServices, extractAstNode, getAuroraServices}
import typings.auroraLangium.cliMod.extractAstNode
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.Thenable.Implicits.*
import typings.langium.langiumStrings.langium

import org.aurora.sjsast.GenAst


class ParseTest extends BaseAsyncTest:
  lazy val emptyServices = getAuroraServices()
  private val simpleFixturePath =
    s"${fileutils.testResourcesPath}${fileutils.separator}fixtures${fileutils.separator}simple-valid.aurora"
  // def parse1PCM(filename:String) =  
  
  

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

    "pcm+pcm" in {
      val path0 = testfilepath(0)
      import catsgivens.given
      import cats.syntax.semigroup._ // for |+|
      import org.aurora.sjsast.ShowAurora.given
      import cats.syntax.show._ 

      for {
        langiumPCM <- org.aurora.utils.fileutils.parse(path0).toFuture
        pcm:PCM <- Future( PCM(langiumPCM))
        result   <- Future(pcm |+| pcm)
        _       <- Future(info(s"result: ${result.show}"))
        b <- Future(result should be (pcm))
      } yield {
        b
        
      }
    }

    "parse the simple fixture and expose Issues/Orders" in {
      for {
        parseResult <- fileutils.parse(simpleFixturePath).toFuture
      } yield {
        val pcmAst = parseResult.asInstanceOf[GenAst.PCM]

        pcmAst.module.toOption shouldBe empty

        val elements = pcmAst.elements.toSeq

        val issues = elements.collect {
          case element if element.$type == "Issues" => element.asInstanceOf[GenAst.Issues]
        }
        issues should have length 1

        val issueNames = issues.head.coord.toSeq.map(_.name)
        issueNames should contain theSameElementsInOrderAs Seq("A", "B")

        val maybeOrders = elements.collectFirst {
          case element if element.$type == "Orders" => element.asInstanceOf[GenAst.Orders]
        }
        maybeOrders shouldBe defined

        val namedGroups = maybeOrders.get.namedGroups.toOption.map(_.toSeq).getOrElse(Seq.empty)
        namedGroups should have length 1
        namedGroups.head.name shouldBe "GroupOne:"

        val orderNames = namedGroups.head.orders.toSeq.map(_.name)
        orderNames should contain theSameElementsInOrderAs Seq("OrderA")
      }
    }

} 

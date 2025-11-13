package org.aurora.sjsast

import scala.concurrent.Future

import typings.auroraLangium.cliMod.{getEmptyAuroraServices, getAuroraServices}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.Thenable.Implicits.*
import typings.langium.langiumStrings.langium

import org.aurora.sjsast.GenAst


class ParseTest extends BaseAsyncTest:
  lazy val emptyServices = getAuroraServices()
  // Canonical fixture used to assert we can introspect Issues/Orders without cross-doc refs.
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
      // This test walks the parsed AST directly to make sure PCM is correct & verified.
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

        val namedGroups = Option(maybeOrders.get.namedGroups).map(_.toSeq).getOrElse(Seq.empty)
        namedGroups should have length 1
        val group = namedGroups.head
        group.name shouldBe "GroupOne:"

        // Orders may contain simple coordinates or mutually-exclusive pairs; extract names from both shapes.
        val orderNames = group.orders.toSeq.flatMap { order =>
          val dynamicOrder = order.asInstanceOf[js.Dynamic]
          dynamicOrder
            .selectDynamic("$type")
            .asInstanceOf[js.UndefOr[String]]
            .toOption match
              case Some("OrderCoordinate") =>
                Seq(dynamicOrder.selectDynamic("name").asInstanceOf[String])
              case Some("MutuallyExclusive") =>
                val order1 = dynamicOrder.selectDynamic("order1").asInstanceOf[js.Dynamic]
                val order2 = dynamicOrder.selectDynamic("order2").asInstanceOf[js.Dynamic]
                Seq(
                  order1.selectDynamic("name").asInstanceOf[String],
                  order2.selectDynamic("name").asInstanceOf[String]
                )
              case _ => Seq.empty
        }
        orderNames should contain theSameElementsInOrderAs Seq("OrderA")
      }
    }

} 

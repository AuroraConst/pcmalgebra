package org.aurora.sjsast

import scala.concurrent.Future

import scala.scalajs.js
import typings.langium.langiumStrings.langium
import scala.scalajs.js.JSConverters._ 
import org.scalatest.compatible.Assertion

class ArnoldGeneralPCMTest extends BaseAsyncTest:
  import ShowAurora.given
  import cats.syntax.show._ 
  def finfo(output:String) =  Future(info(s"$output"))


  "ArnoldGeneralPCM-0" should {
    "be not be a module" in { 
      //TODO note if there is no file named ArnoldGeneralPCM-0.aurora, it will automatically create one
      //SHOULD TEST FILE NAMING BE MORE DESCRIPTIVE e.g. ArnoldModule-Valid.aurora

      for {
        tspcm0      <- parse(0)
        tspcm1     <- parse(1)
        pcm0         <- Future(PCM(tspcm0))
        pcm1     <- Future(PCM(tspcm1))
        _           <- pcm0 should be(pcm1)
        // _           <-  (pcm0 |+| pcm0) should be(pcm0)
        _           <- pcm0.module should be (None)
        a           <- (true should be(true))
        _           <- finfo(s"${pcm0.show}")
      } yield {
         a
      }
    } 
  }

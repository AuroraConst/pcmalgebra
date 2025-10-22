package org.aurora.sjsast

import scala.concurrent.Future

class ParseTest extends BaseAsyncTest:
  
  

  "First Parse" should {
    "work" in { 
      val path = testfilepath(0)
      info(s"path: $path")

      //TODO how can we see the parse erros

      for {
        result <- parse(0)
        pcm  <- Future(PCM(result))
        _    <- Future(info(s"pcm: $pcm"))
         _     <- Future(info(s"result: $result"))
        b <- Future(true should be (true))
      } yield {
        b
      }
    }

  }

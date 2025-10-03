package org.aurora.sjsast

import scala.concurrent.Future

class ParseTest extends BaseAsyncTest:
  
  

  "First Parse" should {
    "work" in { 
      val path = testfilepath(0)

      for {
        result <- parse(0)
         _     <- Future(info(s"result: $result"))
        b <- Future(true should be (true))
      } yield {
        b
      }
    }

  }

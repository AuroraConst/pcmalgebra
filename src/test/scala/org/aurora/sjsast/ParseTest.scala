package org.aurora.sjsast

import scala.concurrent.Future

class ParseTest extends BaseAsyncTest:
  
  

  "First Parse" should {
    "work" in { 
      val path = testfilepath(0)

      for {
        result <- parse(0)
        b <- Future(true should be (true))
      } yield {
        b
      }
    }

  }

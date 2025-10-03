package org.aurora.sjsast

import scala.concurrent.Future
import scala.scalajs.js


class ParseTest extends BaseAsyncTest{

  def parse(text:String) = fileutils.parse(text).toFuture.recover {
          case _: js.JavaScriptException => 
            // Handle JavaScript parsing errors
            fail("Parse failed with JavaScript error")
          case ex: Exception => 
            // Handle other exceptions
            fail(s"Parse failed: ${ex.getMessage}")
        }

  "First Parse" should {
    "work" in { 
      val text = testfile(0)
      println(text)

      for {
        result <- parse(text)
        b <- Future(true should be (true))
      } yield {
        b
      }
    }
    
    "handle invalid input gracefully" in {
      val invalidText = "definitely not valid syntax"
      
      recoverToSucceededIf[Exception] {
        fileutils.parse(invalidText).toFuture
      }
    }
  }

  
}

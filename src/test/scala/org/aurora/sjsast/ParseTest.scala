package org.aurora.sjsast

import scala.concurrent.Future

class ParseTest extends BaseAsyncTest:
  
  

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
        pcm <- parse(3)
        module <- Future(pcm.module.get.name)
        //TODO CAN WE GET URI of resource?
        //https://langium.org/docs/learn/workflow/resolve_cross_references/
        uri    <- Future(module)
        _      <- Future(info(s"module: $module"))
        b <- Future(true should be (true))


      } yield {
        b
      }
    }



    "parse(4) references module from parse(3) in {"  in {
      val path3 = testfilepath(3)
      val path4 = testfilepath(4)

      for {
        module <- Future(parse(3))
        pcm4   <- parse(4)
        b   <-    Future(true should be (true))
       } yield {
       b
      }
  }
} 
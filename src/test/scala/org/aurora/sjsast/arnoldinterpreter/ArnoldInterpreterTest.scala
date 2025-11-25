package org.aurora.sjsast.arnoldinterpreter

import org.aurora.sjsast._

class ArnoldInterpreterTest extends BaseAsyncTest:

  "evaluate" should {
    "work" in { 
      for {
        result     <- Future(SimpleInterpreter.evaluate(Add(Number(5), Number(3))))
      } yield result should be(IntValue(8))
    } 
  }
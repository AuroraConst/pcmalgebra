package org.aurora.sjsast.arnoldinterpreter

import org.aurora.sjsast._

class ArnoldInterpreterTest extends BaseAsyncTest:

  "evaluate" should {
    "add" in { 
      for {
        result     <- Future(SimpleInterpreter.evaluate(Add(Number(5), Number(3))))
      } yield result should be(IntValue(8))
    }

    "less than" in {
      for {
        result     <- Future(SimpleInterpreter.evaluate(LessThan(Number(2), Number(5))))
      } yield result should be(BoolValue(true))
    }

    "add numbers then less than" in {
      for {
        result     <- Future(SimpleInterpreter.evaluate(LessThan(Add(Number(2),Number(2)), Number(5))))
      } yield result should be(BoolValue(true))
    }
  }
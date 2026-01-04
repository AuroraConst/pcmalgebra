package org.aurora.sjsast.arnoldinterpreter
import org.aurora.sjsast.*


class ArnoldCoordinateSerializationTest extends BaseAsyncTest:
  "this" should {
    "work" in {
      import magnolia1._
      case class  A(i:Int,s:String, b:B)
      case class B(d:Double) 

      given org.aurora.sjsast.arnoldinterpreter.Show[String, B] = _.toString

      val x=  summon[org.aurora.sjsast.arnoldinterpreter.Show[String,A]] 
      val f= x.show _

      val result = f(A(42,"hello",B(3.14)))
      result should be ("A(i=42,s=hello,b=B(3.14))")
      info(result)


      
      true should be(true)
    }
  }

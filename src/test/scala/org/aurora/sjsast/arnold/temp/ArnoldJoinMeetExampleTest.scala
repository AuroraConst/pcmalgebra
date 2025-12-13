package org.aurora.sjsast.arnold.temp

import org.aurora.sjsast._


class ArnoldJoinMeetExampleTest extends BaseAsyncTest:

  "manual typeclass derivation" should {
    "look like this" in { 
      case class A(a:Set[Int])

      val a1 = A(Set(10))
      val a2 = A(Set(20))  
      val a3 = A(Set(30))

      val je:JoinMeetExample[A] = JoinMeetExample.derived
      je.join(a1, a2) should be (A(Set(10,20)))
      
    } 
  } 

  "extension implementation with using clause " should {
    "look like this" in { 
      case class A(a:Set[Int])

      val a1 = A(Set(10))
      val a2 = A(Set(20))  
      val a3 = A(Set(30))

      extension[T](a:T)
        def join(b:T)(using je:JoinMeetExample[T]):T= je.join(a,b)
        def meet(b:T)(using je:JoinMeetExample[T]):T= je.meet(a,b)
        def leftOnly(b:T)(using je:JoinMeetExample[T]):T= je.leftOnly(a,b)

      a1.join(a2) should be (A(Set(10,20))) //join
      a1.join(a2) should be(a2.join(a1))  //communativity
      a1.join(a1) should be (a1) //idempotency

      val allJoined =  (a1.join(a2).join(a3))
      a1.meet( allJoined ) should be (a1)

      a1.leftOnly( a2.join(a3) ) should be (a1)
    } 
  }


  "extension implementation with using clause while narrowing T" should {
    "look like this" in { 
      sealed trait MeetJoinAble
      case class A(a:Set[Int]) extends MeetJoinAble

      val a1 = A(Set(10))
      val a2 = A(Set(20))  
      val a3 = A(Set(30))

      //narrowing acceptable types for extension
      extension[T<:MeetJoinAble](a:T)
        def join(b:T)(using je:JoinMeetExample[T]):T= je.join(a,b)
        def meet(b:T)(using je:JoinMeetExample[T]):T= je.meet(a,b)
        def leftOnly(b:T)(using je:JoinMeetExample[T]):T= je.leftOnly(a,b)

      a1.join(a2) should be (A(Set(10,20))) //join
      a1.join(a2) should be(a2.join(a1))  //communativity
      a1.join(a1) should be (a1) //idempotency

      val allJoined =  (a1.join(a2).join(a3))
      a1.meet( allJoined ) should be (a1)


      a1.leftOnly( a2.join(a3) ) should be (a1)
    } 
  }



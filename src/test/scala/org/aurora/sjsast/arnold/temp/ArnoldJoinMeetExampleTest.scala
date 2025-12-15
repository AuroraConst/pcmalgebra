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
      extension[T ](a:T)
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

  "case class within case class" should {
    "look like this" in { 
      sealed trait MeetJoinAble
      case class A(a:Set[Int]) //not extending MeetJoinAble fits with being B being a top level case class like B is where we intend to do top level join/meet operations
      case class B(b:Set[A]) extends MeetJoinAble

      val a1 = A(Set(10))
      val a2 = A(Set(20))    
      val a3 = A(Set(30))

      val b1 = B(Set(a1))
      val b2 = B(Set(a2))
      val b3 = B(Set(a3))

      val b12 = B(Set(a1,a2))
      val b13 = B(Set(a1,a3))
      val b23 = B(Set(a2,a3))
      val b123 = B(Set(a1,a2,a3))

      //narrowing acceptable types for extension
      extension[T<:MeetJoinAble](a:T)
        def join(b:T)(using je:JoinMeetExample[T]):T= je.join(a,b)
        def meet(b:T)(using je:JoinMeetExample[T]):T= je.meet(a,b)
        def leftOnly(b:T)(using je:JoinMeetExample[T]):T= je.leftOnly(a,b)

      b1.join(b12) should be (b12) //join

      b12.join(b1) should be (b12) //join communative

      b1.join(b2) should be (B(Set(a1,a2))) //join

      b1.join(b1) should be (b1) //idempotency


      val allJoined =  (b1.join(b2).join(b3)) 
      b1.meet( allJoined ) should be (b1) //meet

      b1.leftOnly( b2.join(b3) ) should be (b1)


    } 
  }




package org.aurora.sjsast.arnold.temp
import magnolia1.*

/**
  */
trait JoinMeetExample[T]:
  def join(left: T, right: T): T
  def meet(left: T, right: T): T
  def leftOnly(left:T, right:T):T 


object JoinMeetExample extends AutoDerivation[JoinMeetExample]:
  def join[T](ctx: CaseClass[JoinMeetExample, T]): JoinMeetExample[T] =  new JoinMeetExample[T] {
    def join(left:T, right:T) = 
      val params = ctx.params.map { p => p.typeclass.join(p.deref(left), p.deref(right)) }
      ctx.rawConstruct(params) //constructs T (which represents the case class) from the joined parameters

    def meet(left:T, right:T) = 
      val params = ctx.params.map { p => p.typeclass.meet(p.deref(left), p.deref(right)) }
      ctx.rawConstruct(params)

    def leftOnly(left: T, right: T)  :T = 
      val params = ctx.params.map { p => p.typeclass.leftOnly(p.deref(left), p.deref(right)) }
      ctx.rawConstruct(params)
  } 


  override def split[T](ctx: SealedTrait[JoinMeetExample, T]): JoinMeetExample[T] = new JoinMeetExample[T] {
    def join(left:T, right:T) =
      ctx.choose(left) { sub =>
        sub.typeclass.join(sub.value, sub.cast(right))
      }

    def meet(left: T, right: T): T = 
      ctx.choose(left) { sub =>
        sub.typeclass.join(sub.value, sub.cast(right))
      }

    def leftOnly(left:T, right:T):T =  ctx.choose(left) { sub =>
        sub.typeclass.leftOnly(sub.value, sub.cast(right))
    }
  }

  given [T]: JoinMeetExample[Option[T]] = new JoinMeetExample[Option[T]] {
    def join(left: Option[T], right: Option[T]) =  (left,right) match {
      case (Some(left), Some(right)) => if(left==right) Some(left) else
        throw new Exception(s"Cannot join different values: $left and $right")
      case (None, Some(right))     => Some(right)
      case (Some(left), None)     => Some(left)
      case _                    => None   
    }

    def meet(left: Option[T], right: Option[T]) =  (left,right) match {
      case (Some(left), Some(right)) => Some(left)
      case _                    => None   
    }

    def leftOnly(left: Option[T], right: Option[T]): Option[T] = (left, right) match {
      case (Some(left), None)     => Some(left)
      case _                    => None   
    }
  }

  given [T] : JoinMeetExample[Set[T]] = new JoinMeetExample[Set[T]] {
    def join(left: Set[T], right: Set[T]) = left union right
    def meet(left: Set[T], right: Set[T]) = left intersect right
    def leftOnly(left: Set[T], right: Set[T]): Set[T] = left diff meet(left,right)
  }
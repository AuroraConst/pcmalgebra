package org.aurora.sjsast


export catsgivens.given
export cats.syntax.semigroup._ // for |+|
export org.aurora.sjsast.ShowAurora.given
export cats.syntax.show._ 

//TODO considering adding an ?Int field to allow "negatives" 
trait SjsNode :

  val name:String
  def merge(p:SjsNode):SjsNode 


extension [T<:SjsNode](s:Set[T]) 
  private def kv(o:T) = o.name -> o
  def asMap:Map[String,SjsNode] =  s.map{kv}.toMap
  def merge(s1:Set[T]):Set [T] =
    combine(s,s1)


  
def combine[T <:SjsNode](x:Set[T], y:Set[T]): Set[T] = 
  x |+| y



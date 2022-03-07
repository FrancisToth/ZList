package com.zlist

import com.zlist.ZList._
import com.zlist.Field._

sealed trait Replace[-Tag, -Rep, -In <: ZList, +Out <: ZList] {
  def replace(rep: Rep, in: In): Out
}
object Replace extends Lower0 {
  implicit val znil = new Replace[Any, Any, ZNil, ZNil] {
    override def replace(rep: Any, in: ZNil): ZNil = ZNil
  }

  implicit def intFound[Tag, Tag0, Tail <: ZList, Out <: ZList](implicit
      ev: Replace[Tag, TInt[Tag0], Tail, Out]
  ) = found[Tag, TInt[Tag0], TInt[Tag], Tail, Out]

  implicit def stringFound[Tag, Tag0, Tail <: ZList, Out <: ZList](implicit
      ev: Replace[Tag, TString[Tag0], Tail, Out]
  ) = found[Tag, TString[Tag0], TString[Tag], Tail, Out]

  private def found[Tag, Rep, Head, Tail <: ZList, Out <: ZList](implicit
      ev: Replace[Tag, Rep, Tail, Out]
  ) =
    new Replace[Tag, Rep, Head :: Tail, Rep :: Out] {
      override def replace(rep: Rep, in: Head :: Tail): Rep :: Out =
        ::(rep, ev.replace(rep, in.tail))
    }
}
trait Lower0 {

  implicit def intNotFound[Tag, Tag0, Tag1, Tail <: ZList, Out <: ZList](
      implicit ev: Replace[Tag, Any, Tail, Out]
  ): Replace[Tag, Any, TInt[Tag1] :: Tail, TInt[Tag1] :: Out] =
    notFound[Tag, Any, TInt[Tag1], Tail, Out]

  implicit def stringNotFound[Tag, Tag0, Tag1, Tail <: ZList, Out <: ZList](
      implicit ev: Replace[Tag, Any, Tail, Out]
  ): Replace[Tag, Any, TString[Tag1] :: Tail, TString[Tag1] :: Out] =
    notFound[Tag, Any, TString[Tag1], Tail, Out]

  private def notFound[Tag, Rep, Head, Tail <: ZList, Out <: ZList](implicit
      ev: Replace[Tag, Rep, Tail, Out]
  ) =
    new Replace[Tag, Rep, Head :: Tail, Head :: Out] {
      override def replace(rep: Rep, in: Head :: Tail): Head :: Out =
        ::(in.head, ev.replace(rep, in.tail))
    }
}

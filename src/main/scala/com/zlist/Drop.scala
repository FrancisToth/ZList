package com.zlist

import com.zlist.ZList._
import com.zlist.Field._

sealed trait Drop[-Tag, A <: ZList, Out <: ZList] {
  def drop(ZList: A): Out
}
object Drop extends DropLower {
  implicit val empty: Drop[Any, ZNil, ZNil] = new Drop[Any, ZNil, ZNil] {
    override def drop(ZList: ZNil): ZNil = ZList
  }

  implicit def intFound[Tag, Tail <: ZList]
      : Drop[Tag, TInt[Tag] :: Tail, Tail] =
    found[Tag, Int, Tail]
  implicit def stringFound[Tag, Tail <: ZList]
      : Drop[Tag, TString[Tag] :: Tail, Tail] =
    found[Tag, String, Tail]

  private def found[Tag, Head, Tail <: ZList] =
    new Drop[Tag, TField[Head, Tag] :: Tail, Tail] {
      override def drop(ZList: TField[Head, Tag] :: Tail): Tail =
        ZList.tail
    }
}
trait DropLower {

  implicit def intNotFound[Tag, A, Tail <: ZList, Out <: ZList](implicit
      ev: Drop[Tag, Tail, Out]
  ): Drop[Tag, TInt[A] :: Tail, TInt[A] :: Out] =
    notFound[Tag, A, Int, Tail, Out]

  implicit def stringNotFound[Tag, A, Tail <: ZList, Out <: ZList](implicit
      ev: Drop[Tag, Tail, Out]
  ): Drop[Tag, TString[A] :: Tail, TString[A] :: Out] =
    notFound[Tag, A, String, Tail, Out]

  def notFound[Tag, A, Head, Tail <: ZList, Out0 <: ZList](implicit
      ev: Drop[Tag, Tail, Out0]
  ) = new Drop[Tag, TField[Head, A] :: Tail, TField[Head, A] :: Out0] {
    override def drop(
        ZList: TField[Head, A] :: Tail
    ): TField[Head, A] :: Out0 =
      ::[TField[Head, A], Out0](ZList.head, ev.drop(ZList.tail))
  }
}

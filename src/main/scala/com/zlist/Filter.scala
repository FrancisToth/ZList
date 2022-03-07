package com.zlist

import ZList._

sealed trait Filter[-Lookup, -In] {
  type Out <: ZList
  def apply(in: In): Out
}

object Filter extends FilterImplicits0 {
  type Aux[Lookup, In, Out0] = Filter[Lookup, In] { type Out = Out0 }

  implicit case object Zero extends Filter[Any, ZNil] {
    type Out = ZNil
    def apply(in: ZNil): ZNil = ZNil
  }

  implicit def found[A, B <: ZList](implicit
      f: Filter[A, B]
  ): Filter.Aux[A, A :: B, A :: f.Out] =
    new Filter[A, A :: B] {
      type Out = A :: f.Out
      def apply(in: A :: B): A :: f.Out =
        ZList.::(in.head, f(in.tail))
    }
}

sealed trait FilterImplicits0 {

  implicit def notFound[A0, A, B <: ZList](implicit
      f: Filter[A0, B]
  ): Filter.Aux[A0, A :: B, f.Out] = {
    new Filter[A0, A :: B] {
      type Out = f.Out
      def apply(in: A :: B): Out =
        f(in.tail)
    }
  }
}

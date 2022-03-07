package com.zlist

import com.zlist.ZList._
import com.zlist.Natural._

sealed trait Take[N <: Natural, A <: ZList] {
  type Out <: ZList
  def apply(a: A): Out
}
object Take {

  type Aux[N <: Natural, A <: ZList, Out0] = Take[N, A] { type Out = Out0 }

  implicit def zero[A <: ZList]: Take.Aux[_0, A, ZNil] =
    new Take[_0, A] {
      type Out = ZNil
      def apply(a: A): ZNil = ZNil
    }

  implicit def many[A, B <: ZList, N <: Natural](implicit
      t: Take[N, B]
  ): Take.Aux[Succ[N], A :: B, A :: t.Out] =
    new Take[Succ[N], A :: B] {
      type Out = A :: t.Out
      def apply(a: A :: B): Out =
        ZList.::(a.head, t(a.tail))
    }
}

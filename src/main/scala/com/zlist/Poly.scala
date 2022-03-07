package com.zlist

import com.zlist.ZList._

sealed trait Poly[A, B] { self =>
  def apply(value: A): B
}
object Poly {

  def make[A, B](f: A => B): Poly[A, B] =
    new Poly[A, B] {
      def apply(value: A): B =
        f(value)
    }

  implicit def zero: Poly[ZNil, ZNil] = make(identity)

  implicit def many[A, A0, B <: ZList, B0 <: ZList](implicit
      ev: Poly[A, A0],
      ev0: Poly[B, B0]
  ): Poly[A :: B, A0 :: B0] =
    make(value => ZList.::(ev(value.head), ev0(value.tail)))
}

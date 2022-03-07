package com.zlist

sealed trait Natural {
  def value: Int
}
object Natural {
  case object _0 extends Natural { val value = 0 }
  final class Succ[A <: Natural](val value: Int) extends Natural

  type _0 = _0.type
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]

  implicit val _1: _1 = new Succ(0)
  implicit val _2: _2 = new Succ(1)
  implicit val _3: _3 = new Succ(2)
  implicit val _4: _4 = new Succ(3)
}

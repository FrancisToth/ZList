package com.zlist

import com.zlist.ZList._
import com.zlist.Field._
import com.zlist.Field
import com.zlist.FieldType._
import com.zlist.Natural._

object Example {
  val bool = true :: ZNil
  val string = "" :: ZNil
  val int = 1 :: ZNil

  val boolAndFloat: Boolean :: Double :: ZNil = bool :+ 3.14
  val all: Boolean :: String :: Int :: ZNil = bool ++ string ++ int

  // **** MAP ****
  implicit val case1 = Poly.make((_: Boolean).toString())
  implicit val case2 = Poly.make((_: Int).longValue())
  implicit val case3 = Poly.make((_: String).length)

  val boolAsString: String :: ZNil = bool.map
  val all0: String :: Int :: Long :: ZNil = all.map

  // **** TAKE ****
  val zero: ZNil = all.take(_0)
  val one: Boolean :: ZNil = all.take(_1)
  val two: Boolean :: String :: ZNil = all.take(_2)
  val three: Boolean :: String :: Int :: ZNil = all.take(_3)
  // val four = all.take(_4) // does not compile as expected

  // **** Filter ****
  val zero0: ZNil = all.filter(DoubleField)
  val bools: Boolean :: ZNil = all.filter(BooleanField)
  val ints: Int :: ZNil = all.filter(IntField)
  val strings: String :: ZNil = all.filter(StringField)

  val allBools: Boolean :: Boolean :: ZNil = (all ++ all).filter(BooleanField)
  val allInts: Int :: Int :: ZNil = (all ++ all).filter(IntField)
  val allStrings: String :: String :: ZNil = (all ++ all).filter(StringField)

  // Tagged Fields
  val a = Field.int("a")
  val b = Field.string("b")
  val c = Field.string("c")
  val d = Field.int("d")

  val y
      : TInt[a.Tag] :: TString[b.Tag] :: TString[c.Tag] :: TInt[a.Tag] :: ZNil =
    a :: b :: c :: a :: ZNil

  val y0 = y.drop(a)

  // TODO : Add more tests
  y.replaceAll(a, d)
}

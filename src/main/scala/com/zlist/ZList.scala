package com.zlist

import Field._

sealed trait ZList {
  type Append[That] <: ZList
  def append[That](that: That): Append[That]

  type Concat[That <: ZList] <: ZList
  def ++[That <: ZList](that: That): Concat[That] = concat(that)
  def concat[That <: ZList](that: That): Concat[That]
}
object ZList {

  type ZNil = ZNil.type

  final case object ZNil extends ZList {
    type Append[That] = That :: ZNil
    type Concat[That <: ZList] = That

    def concat[That <: ZList](that: That): That =
      that

    def ::[A](that: A): A :: ZNil =
      ZList.::(that, ZNil)

    def append[That](that: That): ZList.::[That, ZNil] =
      that :: ZNil
  }

  final case class ::[Head, Tail <: ZList](head: Head, tail: Tail)
      extends ZList { self =>
    type Append[That] = Head :: tail.Append[That]
    type Concat[That <: ZList] = Head :: tail.Concat[That]

    def concat[That <: ZList](that: That): Concat[That] =
      ZList.::(head, tail.concat(that))

    def ::[C](that: C): C :: Head :: Tail = ZList.::(that, self)

    def append[That](that: That): Head :: tail.Append[That] =
      ZList.::(head, tail.append(that))

    def :+[That](that: That): Head :: tail.Append[That] =
      append(that)

    def map[C](implicit p: Poly[Head :: Tail, C]): C =
      p(ZList.::(head, tail))

    def take[N <: Natural, T <: ZList](
        n: N
    )(implicit t: Take.Aux[N, Head :: Tail, T]): T = {
      val _ = n
      t(self)
    }

    def filter[C, O](
        tt: FieldType[C]
    )(implicit f: Filter.Aux[C, Head :: Tail, O]): O = {
      val _ = tt
      f(self)
    }

    def drop[Tag, Out <: ZList, A](field: TField[A, Tag])(implicit
        ev: Drop[Tag, Head :: Tail, Out]
    ): Out = {
      val _ = field
      ev.drop(self)
    }

    def replaceAll[Tag, Tag0, Out <: ZList, A](
      field: TField[A, Tag],
      repl: TField[A, Tag0]
    )(implicit
        ev: Replace[Tag, TField[A, Tag0], Head :: Tail, Out]
    ): Out = {
      val _ = field
      ev.replace(repl, self)
    }
  }
}

sealed trait FieldType[A]
object FieldType {
  implicit case object IntField extends FieldType[Int]
  implicit case object StringField extends FieldType[String]
  implicit case object BooleanField extends FieldType[Boolean]
  implicit case object DoubleField extends FieldType[Double]
}

sealed trait Field[A] {
  type Tag
  val fieldType: FieldType[A]
  val name: String
}
object Field {
  def apply[A: FieldType](name: String): Field[A] =
    Source[A, name.type](name)

  def int(name: String): TInt[name.type] = Source[Int, name.type](name)
  def string(name: String): TString[name.type] = Source[String, name.type](name)

  final case class Source[A: FieldType, Tag0](name: String) extends Field[A] {
    type Tag = Tag0
    val fieldType = implicitly[FieldType[A]]
  }

  type TField[A, Tag0] = Field[A] { type Tag = Tag0 }
  type TInt[Tag] = TField[Int, Tag]
  type TString[Tag] = TField[String, Tag]
  type TDouble[Tag] = TField[Double, Tag]
  type TBoolean[Tag] = TField[Boolean, Tag]
}

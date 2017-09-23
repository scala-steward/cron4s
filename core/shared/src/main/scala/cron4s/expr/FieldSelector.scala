/*
 * Copyright 2017 Antonio Alonso Dominguez
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cron4s.expr

import cron4s.CronField

import shapeless.{Generic, HList, Lazy}
import shapeless.ops.hlist.Selector

import scala.annotation.implicitNotFound

/**
  * Created by alonsodomin on 10/02/2017.
  */

trait FieldBinding[F <: CronField] {
  type NodeType
}
object FieldBinding {
  import CronField._

  type Aux[F <: CronField, N] = FieldBinding[F] { type NodeType = N }

  implicit object SecondsBinding extends FieldBinding[Second] {
    type NodeType = SecondsNode
  }
  implicit object MinutesBinding extends FieldBinding[Minute] {
    type NodeType = MinutesNode
  }
  implicit object HoursBinding extends FieldBinding[Hour] {
    type NodeType = HoursNode
  }
  implicit object DaysOfMonthBinding extends FieldBinding[DayOfMonth] {
    type NodeType = DaysOfMonthNode
  }
  implicit object MonthsBinding extends FieldBinding[Month] {
    type NodeType = MonthsNode
  }
  implicit object DaysOfWeekBinding extends FieldBinding[DayOfWeek] {
    type NodeType = DaysOfWeekNode
  }
}

@implicitNotFound("Field ${F} is not a member of expression ${E}")
sealed trait NodeExtractor[E, F <: CronField] {
  type N
  def extractFrom(expr: E): N
}

object NodeExtractor {

  type Aux[E, F, N0] = NodeExtractor[E, F] { type N = N0 }

  implicit def deriveExtractor[E, F <: CronField, Rw <: HList, N0](
    implicit
    generic: Generic.Aux[E, Rw],
    fieldBinding: FieldBinding.Aux[F, N0],
    selector: Selector[Rw, N0]
  ): NodeExtractor[E, F] = new NodeExtractor[E, F] {
    type N = N0

    def extractFrom(expr: E) =
      selector(generic.to(expr))

  }

}

@implicitNotFound("Field ${F} is not a member of expression ${A}")
sealed trait FieldSelector[A, F <: CronField] {
  type Raw <: HList
  type Out

  protected implicit def hlistSelect: Lazy[Selector[Raw, Out]]

  def selectFrom(expr: A): Out

}

object FieldSelector {
  type Aux[A, F <: CronField, O] = FieldSelector[A, F] { type Out = O }

  import CronField._

  def apply[A, F <: CronField](implicit ev: FieldSelector[A, F]): FieldSelector[A, F] = ev

  implicit def deriveSelector[F <: CronField, E, N](implicit binding: FieldBinding.Aux[F, N]): FieldSelector.Aux[E, F, N] = ???

  implicit val SecondsFromCronExpr: FieldSelector[CronExpr, Second] = new FullCronFieldNodeSelector[Second] {
    implicit val hlistSelect: Lazy[Selector[RawCronExpr, FieldNode[Second]]] = Selector[RawCronExpr, FieldNode[Second]]
  }
  implicit val SecondsFromTimeExpr: FieldSelector[TimeCronExpr, Second] = new TimeCronFieldNodeSelector[Second] {
    implicit val hlistSelect: Lazy[Selector[RawTimeCronExpr, FieldNode[Second]]] = Selector[RawTimeCronExpr, FieldNode[Second]]
  }

  implicit val MinutesFromCronExpr: FieldSelector[CronExpr, Minute] = new FullCronFieldNodeSelector[Minute] {
    implicit val hlistSelect: Lazy[Selector[RawCronExpr, FieldNode[Minute]]] = Selector[RawCronExpr, FieldNode[Minute]]
  }
  implicit val MinutesFromTimeExpr: FieldSelector[TimeCronExpr, Minute] = new TimeCronFieldNodeSelector[Minute] {
    implicit val hlistSelect: Lazy[Selector[RawTimeCronExpr, FieldNode[Minute]]] = Selector[RawTimeCronExpr, FieldNode[Minute]]
  }

  implicit val HoursFromCronExpr: FieldSelector[CronExpr, Hour] = new FullCronFieldNodeSelector[Hour] {
    implicit val hlistSelect: Lazy[Selector[RawCronExpr, FieldNode[Hour]]] = Selector[RawCronExpr, FieldNode[Hour]]
  }
  implicit val HoursFromTimeExpr: FieldSelector[TimeCronExpr, Hour] = new TimeCronFieldNodeSelector[Hour] {
    implicit val hlistSelect: Lazy[Selector[RawTimeCronExpr, FieldNode[Hour]]] = Selector[RawTimeCronExpr, FieldNode[Hour]]
  }

  implicit val DayOfMonthFromCronExpr: FieldSelector[CronExpr, DayOfMonth] = new FullCronFieldNodeWithAnySelector[DayOfMonth] {
    implicit val hlistSelect: Lazy[Selector[RawCronExpr, FieldNodeWithAny[DayOfMonth]]] = Selector[RawCronExpr, FieldNodeWithAny[DayOfMonth]]
  }
  implicit val DayOfMonthFromDateExpr: FieldSelector[DateCronExpr, DayOfMonth] = new DateCronFieldNodeWithAnySelector[DayOfMonth] {
    implicit val hlistSelect: Lazy[Selector[RawDateCronExpr, FieldNodeWithAny[DayOfMonth]]] = Selector[RawDateCronExpr, FieldNodeWithAny[DayOfMonth]]
  }

  implicit val MonthFromCronExpr: FieldSelector[CronExpr, Month] = new FullCronFieldNodeSelector[Month] {
    implicit val hlistSelect: Lazy[Selector[RawCronExpr, FieldNode[Month]]] = Selector[RawCronExpr, FieldNode[Month]]
  }
  implicit val MonthFromDateExpr: FieldSelector[DateCronExpr, Month] = new DateCronFieldNodeSelector[Month] {
    implicit val hlistSelect: Lazy[Selector[RawDateCronExpr, FieldNode[Month]]] = Selector[RawDateCronExpr, FieldNode[Month]]
  }

  implicit val DayOfWeekFromCronExpr: FieldSelector[CronExpr, DayOfWeek] = new FullCronFieldNodeWithAnySelector[DayOfWeek] {
    implicit val hlistSelect: Lazy[Selector[RawCronExpr, FieldNodeWithAny[DayOfWeek]]] = Selector[RawCronExpr, FieldNodeWithAny[DayOfWeek]]
  }
  implicit val DayOfWeekFromDateExpr: FieldSelector[DateCronExpr, DayOfWeek] = new DateCronFieldNodeWithAnySelector[DayOfWeek] {
    implicit val hlistSelect: Lazy[Selector[RawDateCronExpr, FieldNodeWithAny[DayOfWeek]]] = Selector[RawDateCronExpr, FieldNodeWithAny[DayOfWeek]]
  }

  // Base classes adding type refinements for the typeclass instances

  private[this] abstract class FieldNodeSelector[A, F <: CronField] extends FieldSelector[A, F] {
    type Out = FieldNode[F]
  }
  private[this] abstract class FullCronFieldNodeSelector[F <: CronField] extends FieldNodeSelector[CronExpr, F] {
    type Raw = RawCronExpr

    def selectFrom(expr: CronExpr): FieldNode[F] = hlistSelect.value(expr.raw)
  }
  private[this] abstract class TimeCronFieldNodeSelector[F <: CronField] extends FieldNodeSelector[TimeCronExpr, F] {
    type Raw = RawTimeCronExpr

    def selectFrom(expr: TimeCronExpr): FieldNode[F] = hlistSelect.value(expr.raw)
  }
  private[this] abstract class DateCronFieldNodeSelector[F <: CronField] extends FieldNodeSelector[DateCronExpr, F] {
    type Raw = RawDateCronExpr

    def selectFrom(expr: DateCronExpr): FieldNode[F] = hlistSelect.value(expr.raw)
  }

  private[this] abstract class FieldNodeWithAnySelector[A, F <: CronField] extends FieldSelector[A, F] {
    type Out = FieldNodeWithAny[F]
  }
  private[this] abstract class FullCronFieldNodeWithAnySelector[F <: CronField] extends FieldNodeWithAnySelector[CronExpr, F] {
    type Raw = RawCronExpr

    def selectFrom(expr: CronExpr): FieldNodeWithAny[F] = hlistSelect.value(expr.raw)
  }
  private[this] abstract class DateCronFieldNodeWithAnySelector[F <: CronField] extends FieldNodeWithAnySelector[DateCronExpr, F] {
    type Raw = RawDateCronExpr

    def selectFrom(expr: DateCronExpr): FieldNodeWithAny[F] = hlistSelect.value(expr.raw)
  }

}

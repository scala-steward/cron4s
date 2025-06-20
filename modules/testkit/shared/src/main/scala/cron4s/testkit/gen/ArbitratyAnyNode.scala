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

package cron4s.testkit.gen
import cron4s.expr.AnyNode

import cron4s.CronField._

import org.scalacheck.Arbitrary

/**
  * Created by alonsodomin on 10/02/2017.
  */
trait ArbitratyAnyNode extends NodeGenerators {
  implicit lazy val arbitraryAnySecond: Arbitrary[AnyNode[Second]] = Arbitrary(anyGen[Second])
  implicit lazy val arbitraryAnyMinute: Arbitrary[AnyNode[Minute]] = Arbitrary(anyGen[Minute])
  implicit lazy val arbitraryAnyHour: Arbitrary[AnyNode[Hour]]     = Arbitrary(anyGen[Hour])
  implicit lazy val arbitraryAnyDayOfMonth: Arbitrary[AnyNode[DayOfMonth]] = Arbitrary(
    anyGen[DayOfMonth]
  )
  implicit lazy val arbitraryAnyMonth: Arbitrary[AnyNode[Month]]         = Arbitrary(anyGen[Month])
  implicit lazy val arbitraryAnyDayOfWeek: Arbitrary[AnyNode[DayOfWeek]] = Arbitrary(
    anyGen[DayOfWeek]
  )
}

/*
 * Copyright 2009 org.http4s
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

package org.http4s.internal.parboiled2.support

import org.http4s.internal.parboiled2.{Rule, Rule0, RuleX}
import org.http4s.internal.parboiled2.support.{ActionOps => ActionOpsTc}

object TypeOps {
  type LifterIn[I <: HList] <: HList = I match {
    case HNil => HNil
    case _ => I
  }

  type LifterOut[M[_], I <: HList, O <: HList] <: HList = O match {
    case HNil => HNil
    case t :: HNil => M[t] :: HNil
    case I => I
  }

    type Reverse0[Acc <: HList, L <: HList] <: HList = L match {
    case HNil => Acc
    case ::[h, t] => Reverse0[h :: Acc, t]
  }

  type Reverse[L <: HList] <: HList = L match {
    case HNil => HNil
    case ::[h, t] => Reverse0[h :: HNil, t]
  }

  type Prepend[A <: HList, B <: HList] <: HList = A match {
    case HNil => B
    case ::[h, t] => ::[h, Prepend[t, B]]
  }

  type TailSwitch0[L <: HList, LI <: HList, T <: HList, TI <: HList, R <: HList, RI <: HList] <: HList = TI match {
    case L => R
    case _ => LI match {
      case T => Prepend[Reverse[RI], R]
      case HNil => TI match {
        case ::[_, t] => TailSwitch0[L, HNil, T, t, R, RI]
      }
      case ::[h, t] => TI match {
        case HNil => TailSwitch0[L, t, T, HNil, R, h :: RI]
        case ::[_, tt] => TailSwitch0[L, t, T, tt, R, h :: RI]
      }
    }
  }

  type TailSwitch[L <: HList, T <: HList, R <: HList] = TailSwitch0[L, L, T, T, R, HNil]

  type HListable[T] <: HList = T match {
    case Unit => HNil
    case HNil => HNil
    case h :: t => h :: t
    case _ => T :: HNil
  }

  type JoinIn[I <: HList, O <: HList, R] <: HList = R match {
    case Rule[i, o] => TailSwitch[i, O, I]
    case _ => I
  }

  type JoinOut[I <: HList, O <: HList, R] <: HList = R match {
    case h :: t => Prepend[O, h :: t]
    case Rule[i, o] => TailSwitch[O, i, o]
    case HNil => O
    case Unit => O
    case _ => JoinOut[I, O, R :: HNil]
  }

  import scala.runtime.AbstractFunction1

  type RunResult[T] <: Rule[Nothing, Any] = T match {
    case AbstractFunction1[z, r] => Rule[JoinIn[z :: HNil, HNil, r], JoinOut[z :: HNil, HNil, r]]
    case Rule[i, o] => Rule[i, o]
    case (z => r) => Rule[JoinIn[z :: HNil, HNil, r], JoinOut[z :: HNil, HNil, r]]
    case ((y, z) => r) => Rule[JoinIn[y :: z :: HNil, HNil, r], JoinOut[y :: z :: HNil, HNil, r]]
    case ((x, y, z) => r) => Rule[JoinIn[x :: y :: z :: HNil, HNil, r], JoinOut[x :: y :: z :: HNil, HNil, r]]
    case ((w, x, y, z) => r) => Rule[JoinIn[w :: x :: y :: z :: HNil, HNil, r], JoinOut[w :: x :: y :: z :: HNil, HNil, r]]
    case ((v, w, x, y, z) => r) => Rule[JoinIn[v :: w :: x :: y :: z :: HNil, HNil, r], JoinOut[v :: w :: x :: y :: z :: HNil, HNil, r]]
    case _ => Rule0
  }

  type ActionOps[I <: HList, O <: HList] = O match {
    case HNil => ActionOpsTc.Ops0[I]
    case a :: HNil => ActionOpsTc.Ops1[I, a]
    case a :: b :: HNil => ActionOpsTc.Ops2[I, a, b]
  }

  type Head[A <: HList] = A match {
    case h :: t => h
  }
}

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

package org.http4s.internal.parboiled2

import scala.annotation.compileTimeOnly
import org.http4s.internal.parboiled2.support._

private[http4s] trait ScalaVersionSpecificRuleDSLCombinators {
  /**
    * Runs its inner rule and succeeds even if the inner rule doesn't.
    * Resulting rule type is
    *   Rule0             if r == Rule0
    *   Rule1[Option[T]]  if r == Rule1[T]
    *   Rule[I, O]        if r == Rule[I, O <: I] // so called "reduction", which leaves the value stack unchanged on a type level
    */
  @compileTimeOnly("Calls to `optional` must be inside `rule` macro")
  def optional[I <: HList, O <: HList](r: Rule[I, O])(implicit l: Lifter[Option, I, O]): Rule[l.In, l.OptionalOut] =
    `n/a`

  /**
    * Runs its inner rule until it fails, always succeeds.
    * Resulting rule type is
    *   Rule0          if r == Rule0
    *   Rule1[Seq[T]]  if r == Rule1[T]
    *   Rule[I, O]     if r == Rule[I, O <: I] // so called "reduction", which leaves the value stack unchanged on a type level
    */
  @compileTimeOnly("Calls to `zeroOrMore` must be inside `rule` macro")
  def zeroOrMore[I <: HList, O <: HList](r: Rule[I, O])(
      implicit l: Lifter[immutable.Seq, I, O]
  ): Rule[l.In, l.OptionalOut] with Repeated = `n/a`

  /**
    * Runs its inner rule until it fails, succeeds if its inner rule succeeded at least once.
    * Resulting rule type is
    *   Rule0          if r == Rule0
    *   Rule1[Seq[T]]  if r == Rule1[T]
    *   Rule[I, O]     if r == Rule[I, O <: I] // so called "reduction", which leaves the value stack unchanged on a type level
    */
  @compileTimeOnly("Calls to `oneOrMore` must be inside `rule` macro")
  def oneOrMore[I <: HList, O <: HList](r: Rule[I, O])(
      implicit l: Lifter[immutable.Seq, I, O]
  ): Rule[l.In, l.StrictOut] with Repeated = `n/a`
  
  sealed trait NTimes {

    /**
      * Repeats the given sub rule `r` the given number of times.
      * Both bounds of the range must be positive and the upper bound must be >= the lower bound.
      * If the upper bound is zero the rule is equivalent to `MATCH`.
      *
      * Resulting rule type is
      *   Rule0          if r == Rule0
      *   Rule1[Seq[T]]  if r == Rule1[T]
      *   Rule[I, O]     if r == Rule[I, O <: I] // so called "reduction", which leaves the value stack unchanged on a type level
      */
    @compileTimeOnly("Calls to `times` must be inside `rule` macro")
    def times[I <: HList, O <: HList](r: Rule[I, O])(
        implicit s: Lifter[immutable.Seq, I, O]
    ): Rule[s.In, s.StrictOut] with Repeated
  }
}
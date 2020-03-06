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
import scala.collection.immutable
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
  def optional[I <: HList, O <: HList](r: Rule[I, O])(implicit l: Lifter[Option, I, O]): Rule[TypeOps.LifterIn[I], TypeOps.LifterOut[Option, I, O]] =
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
  ): Rule[TypeOps.LifterIn[I], TypeOps.LifterOut[immutable.Seq, I, O]] with Repeated = `n/a`

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
  ): Rule[TypeOps.LifterIn[I], TypeOps.LifterOut[immutable.Seq, I, O]] with Repeated = `n/a`

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
    ): Rule[TypeOps.LifterIn[I], TypeOps.LifterOut[immutable.Seq, I, O]] with Repeated
  }
}

private[http4s] trait ScalaVersionSpecificRuleSyntax {

  implicit class ActionOps2[A, B](r: Rule2[A, B]) {
    def ~>[Z](f: (A, B) => Z): Rule[TypeOps.JoinIn[HNil, HNil, Z], TypeOps.JoinOut[HNil, HNil, Z]] = `n/a`
  }

  implicit class ActionOps1[A](r: Rule1[A]) {
    def ~>[Z](f: A => Z): Rule[TypeOps.JoinIn[HNil, HNil, Z], TypeOps.JoinOut[HNil, HNil, Z]] = `n/a`
  }
  //@compileTimeOnly("Calls to `rule2ActionOperator` must be inside `rule` macro")
  //implicit def rule2ActionOperator[A](r: Rule[HNil, A :: HNil]): ActionOperator[A] = `n/a`
}

private[http4s] trait ScalaVersionSpecificRuleDSLBasics {

}

private[http4s] trait ScalaVersionSpecificRuleDSLActions {
  /**
    * Runs the given block / expression / action function.
    * A `run` rule can have several shapes, depending on its argument type. If the `arg` evaluates to
    *
    * - a rule (i.e. has type `R <: Rule[_, _]`) the result type of `run` is this rule's type (i.e. `R`) and the
    *   produced rule is immediately executed.
    *
    * - a function with 1 to 5 parameters these parameters are mapped against the top of the value stack, popped
    *   and the function executed. Thereby the function behaves just like an action function for the `~>` operator,
    *   i.e. if it produces a Unit value this result is simply dropped. HList results are pushed onto the value stack
    *   (all their elements individually), rule results are immediately executed and other result values are pushed
    *   onto the value stack as a single element.
    *
    * - a function with one HList parameter the behavior is similar to the previous case with the difference that the
    *   elements of this parameter HList are mapped against the value stack top. This allows for consumption of an
    *   arbitrary number of value stack elements. (Note: This feature of ``run`` is not yet currently implemented.)
    *
    * - any other value the result type of `run` is an always succeeding `Rule0`.
    *
    * NOTE: Even though the block is not a call-by-name parameter it will be executed
    * for every rule application anew! (Since the expression is directly transplanted
    * into the rule method by the `rule` macro.
    */
  @compileTimeOnly("Calls to `run` must be inside `rule` macro")
  def run[T](arg: T)(implicit rr: RunResult[T]): TypeOps.RunResult[T] = `n/a`

  /**
    * Pushes the input text matched by its inner rule onto the value stack
    * after its inner rule has been run successfully (and only then).
    */
  @compileTimeOnly("Calls to `capture` must be inside `rule` macro")
  def capture[I <: HList, O <: HList](r: Rule[I, O]): Rule[I, TypeOps.Prepend[O, String :: HNil]] = `n/a`

  /**
    * Pushes the given value onto the value stack.
    * - if `T` is `Unit` nothing is pushed (i.e. `push` with a block/expression evaluating to `Unit` is identical to `run`)
    * - if `T <: HList` all values of the HList is pushed as individual elements
    * - otherwise a single value of type `T` is pushed.
    */
  @compileTimeOnly("Calls to `push` must be inside `rule` macro")
  def push[T](value: T)(implicit h: HListable[T]): RuleN[TypeOps.HListable[T]] = `n/a`

  /**
    * Drops one or more values from the top of the value stack.
    * E.g. `drop[Int]` will drop the top ``Int`` value and `drop[Int :: String :: HNil]` will drop the top two values,
    * which must be an ``Int`` underneath a ``String`` (the string being the top stack element).
    */
  @compileTimeOnly("Calls to `drop` must be inside `rule` macro")
  def drop[T](implicit h: HListable[T]): PopRule[TypeOps.HListable[T]] = `n/a`
}
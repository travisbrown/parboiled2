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

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.compileTimeOnly
import org.http4s.internal.parboiled2.support._

/**
  * The general model of a parser rule.
  * It is characterized by consuming a certain number of elements from the value stack (whose types are captured by the
  * HList type parameter `I` for "Input") and itself pushing a certain number of elements onto the value stack (whose
  * types are captured by the HList type parameter `O` for "Output").
  *
  * At runtime there are only two instances of this class which signal whether the rule has matched (or mismatched)
  * at the current point in the input.
  */
private[http4s] trait ScalaVersionSpecificRuleMethods[-I <: HList, +O <: HList] {
  // Note: we could model `Rule` as a value class, however, tests have shown that this doesn't result in any measurable
  // performance benefit and, in addition, comes with other drawbacks (like generated bridge methods)

  /**
    * Concatenates this rule with the given other one.
    * The resulting rule type is computed on a type-level.
    * Here is an illustration (using an abbreviated HList notation):
    *   Rule[, A] ~ Rule[, B] = Rule[, A:B]
    *   Rule[A:B:C, D:E:F] ~ Rule[F, G:H] = Rule[A:B:C, D:E:G:H]
    *   Rule[A, B:C] ~ Rule[D:B:C, E:F] = Rule[D:A, E:F]
    */
  @compileTimeOnly("Calls to `~` must be inside `rule` macro")
  def ~[I2 <: HList, O2 <: HList](that: Rule[I2, O2]): Rule[
    TypeOps.TailSwitch[I2, O @uncheckedVariance, I @uncheckedVariance],
    TypeOps.TailSwitch[O @uncheckedVariance, I2, O2]
  ] = `n/a`

  /**
    * Same as `~` but with "cut" semantics, meaning that the parser will never backtrack across this boundary.
    * If the rule being concatenated doesn't match a parse error will be triggered immediately.
    */
  @compileTimeOnly("Calls to `~!~` must be inside `rule` macro")
  def ~!~[I2 <: HList, O2 <: HList](that: Rule[I2, O2]): Rule[
    TypeOps.TailSwitch[I2, O @uncheckedVariance, I @uncheckedVariance],
    TypeOps.TailSwitch[O @uncheckedVariance, I2, O2]
  ] = `n/a`

  /**
    * Creates a "negative syntactic predicate", i.e. a rule that matches only if this rule mismatches and vice versa.
    * The resulting rule doesn't cause the parser to make any progress (i.e. match any input) and also clears out all
    * effects that the underlying rule might have had on the value stack.
    */
  @compileTimeOnly("Calls to `unary_!` must be inside `rule` macro")
  def unary_! : Rule0 = `n/a`

  //def ~>[II <: I, A, Z](f: A => Z)(using ev: O <:< A :: HNil): Rule[TypeOps.JoinIn[II, HNil, Z], TypeOps.JoinOut[II, HNil, Z]] = `n/a`
  //def ~>[A, B, Z](f: (A, B) => Z)(using ev: O <:< A :: B :: HNil): Rule[TypeOps.JoinIn[I, HNil, Z], TypeOps.JoinOut[I, HNil, Z]] = `n/a`
}
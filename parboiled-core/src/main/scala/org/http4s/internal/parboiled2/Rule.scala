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
import scala.collection.immutable
import org.http4s.internal.parboiled2.support._

//private[http4s]
sealed trait RuleX

/**
  * The general model of a parser rule.
  * It is characterized by consuming a certain number of elements from the value stack (whose types are captured by the
  * HList type parameter `I` for "Input") and itself pushing a certain number of elements onto the value stack (whose
  * types are captured by the HList type parameter `O` for "Output").
  *
  * At runtime there are only two instances of this class which signal whether the rule has matched (or mismatched)
  * at the current point in the input.
  */
//private[http4s]
sealed class Rule[-I <: HList, +O <: HList] extends RuleX with ScalaVersionSpecificRuleMethods[I, O] {
  // Note: we could model `Rule` as a value class, however, tests have shown that this doesn't result in any measurable
  // performance benefit and, in addition, comes with other drawbacks (like generated bridge methods)

  /**
    * Combines this rule with the given other one in a way that the resulting rule matches if this rule matches
    * or the other one matches. If this rule doesn't match the parser is reset and the given alternative tried.
    * This operators therefore implements the "ordered choice' PEG combinator.
    */
  @compileTimeOnly("Calls to `|` must be inside `rule` macro")
  def |[I2 <: I, O2 >: O <: HList](that: Rule[I2, O2]): Rule[I2, O2] = `n/a`

  /**
    * Attaches the given explicit name to this rule.
    */
  @compileTimeOnly("Calls to `named` must be inside `rule` macro")
  def named(name: String): this.type = `n/a`

  /**
    * Postfix shortcut for `optional`.
    */
  @compileTimeOnly("Calls to `.?` must be inside `rule` macro")
  def ?(implicit l: Lifter[Option, I @uncheckedVariance, O @uncheckedVariance]): Rule[l.In, l.OptionalOut] = `n/a`

  /**
    * Postfix shortcut for `zeroOrMore`.
    */
  @compileTimeOnly("Calls to `.*` must be inside `rule` macro")
  def *(
      implicit l: Lifter[immutable.Seq, I @uncheckedVariance, O @uncheckedVariance]
  ): Rule[l.In, l.OptionalOut] with Repeated = `n/a`

  /**
    * Postfix shortcut for `zeroOrMore(...).separatedBy(...)`.
    */
  @compileTimeOnly("Calls to `.*` must be inside `rule` macro")
  def *(separator: Rule0)(
      implicit l: Lifter[immutable.Seq, I @uncheckedVariance, O @uncheckedVariance]
  ): Rule[l.In, l.OptionalOut] = `n/a`

  /**
    * Postfix shortcut for `oneOrMore`.
    */
  @compileTimeOnly("Calls to `.+` must be inside `rule` macro")
  def +(
      implicit l: Lifter[immutable.Seq, I @uncheckedVariance, O @uncheckedVariance]
  ): Rule[l.In, l.StrictOut] with Repeated = `n/a`

  /**
    * Postfix shortcut for `oneOrMore(...).separatedBy(...)`.
    */
  @compileTimeOnly("Calls to `.+` must be inside `rule` macro")
  def +(separator: Rule0)(
      implicit l: Lifter[immutable.Seq, I @uncheckedVariance, O @uncheckedVariance]
  ): Rule[l.In, l.StrictOut] = `n/a`
}

/**
  * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
  */
//private[http4s]
object Rule extends Rule0 with RuleRunnable with ScalaVersionSpecificRuleSyntax

private[http4s] abstract class RuleDSL extends RuleDSLBasics with RuleDSLCombinators with RuleDSLActions

// phantom type for WithSeparatedBy pimp
private[http4s] trait Repeated

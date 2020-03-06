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
import org.http4s.internal.parboiled2.support.HList.Prepend

private[http4s] trait RuleDSLActions extends ScalaVersionSpecificRuleDSLActions {

  /**
    * Implements a semantic predicate. If the argument expression evaluates to `true` the created
    * rule matches otherwise it doesn't.
    */
  @compileTimeOnly("Calls to `test` must be inside `rule` macro")
  def test(condition: Boolean): Rule0 = `n/a`
}

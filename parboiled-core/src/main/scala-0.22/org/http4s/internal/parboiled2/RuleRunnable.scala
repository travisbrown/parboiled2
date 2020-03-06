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

import org.http4s.internal.parboiled2.support.HList

/**
  * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
  */
//private[http4s]
trait RuleRunnable {

  /**
    * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
    */
  implicit class Runnable[L <: HList](rule: => RuleN[L]) {
    inline def run()(implicit scheme: Parser.DeliveryScheme[L]) <: Any =
      ${ParserMacros.runImpl[L]('rule, 'scheme)}
  }
}

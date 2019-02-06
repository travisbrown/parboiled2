package org.http4s
package internal
package parboiled2

import org.specs2.mutable.Specification

class DependencySpec extends Specification {
  "Shapeless" should {
    "not be on the classpath" in {
      Class.forName("shapeless.HList") should throwAn[ClassNotFoundException]
    }
  }
}

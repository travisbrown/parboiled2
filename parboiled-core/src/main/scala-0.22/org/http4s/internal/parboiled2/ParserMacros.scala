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

import org.http4s.internal.parboiled2.support.{HList, OpTreeContext}
import scala.language.experimental.macros
import scala.quoted._
import scala.tasty._

//private[http4s]
trait ParserMacroMethods { self: Parser =>
  /**
    * Converts a compile-time only rule definition into the corresponding rule method implementation.
    */
  inline def rule[I <: HList, O <: HList](inline r: Rule[I, O]): Rule[I, O] = ${ParserMacros.ruleImpl[I, O]('self)('r)}

  /**
    * Converts a compile-time only rule definition into the corresponding rule method implementation
    * with an explicitly given name.
    */
  inline def namedRule[I <: HList, O <: HList](name: String)(inline r: Rule[I, O]): Rule[I, O] =
    ${ParserMacros.namedRuleImpl[I, O]('self)('name)('r)}
}

//private[http4s]
object ParserMacros {
  def runImpl[L <: HList: Type](rule: Expr[RuleN[L]], scheme: Expr[Parser.DeliveryScheme[L]])(using qctx: QuoteContext): Expr[Any] = {
    import qctx.tasty.{_, given _}
    import util._
    val runCall = rule.unseal.underlyingArgument match {
      case Select(Apply(_, List(Select(_p, r))), _) => _p.seal match {
        case '{($__p: $t)} if t.unseal.tpe <:< typeOf[Parser] =>
          '{val p: $t = $__p; p.asInstanceOf[Parser].__run[L](${Select.unique('p.unseal, r).seal.cast[RuleN[L]]})($scheme)}
      }

           //Select(Apply(TypeApply(_, _), List(Select(Apply(Select(New(TypeIdent("Calculator")), "<init>"), List(Apply(Ident("apply"), _))), "xyz"))), "rule")
      //case '{Rule.Runnable($pa).rule} => qctx.throwError(pa.show)
      //case q"parboiled2.this.Rule.Runnable[$l]($ruleExpr)" =>


      //case any => println(any.show); throw new RuntimeException(any.show)
      //case '{($_p: Parser).$x => '{val p = $_p; p.__run[$l](p.$r)($scheme)}
      //case '{($_p: Parser).$r(${ExprSeq(args)}: _*)} => '{val p = $_p; p.__run[$l](p.$r(args))($scheme)}
          //case '{($_p: Parser).$r[$t]} => '{val p = $_p; p.__run[$l](p.$r[$t])($scheme)}
          //case '{($_p: RuleX).$r[$t]} => '{__run[$l]($ruleExpr)($scheme)}
          //case x                                           => c.abort(x.pos, "Illegal `.run()` call base: " + x)
        

      //case x => c.abort(x.pos, "Illegal `Runnable.apply` call: " + x)
    }
    //println(runCall.show)
    runCall
  }

  def ruleImpl[I <: HList: Type, O <: HList: Type](self: Expr[Parser])(r: Expr[Rule[I, O]])(using qctx: QuoteContext): Expr[Rule[I, O]] = {
    import qctx.tasty.{rootContext, given _}
    namedRuleImpl(self)(Expr(rootContext.owner.owner.name))(r)
  }

  def namedRuleImpl[I <: HList: Type, O <: HList: Type](self: Expr[Parser])(name: Expr[String])(r: Expr[Rule[I, O]])(using qctx: QuoteContext): Expr[Rule[I, O]] = {
    import qctx.tasty.{_, given _}
    val opTreeCtx = new OpTreeContext[I, O, qctx.type] { implicit val c: qctx.type = qctx; val parser: Expr[Parser] = self }
    val opTree    = opTreeCtx.RuleCall(Left(opTreeCtx.OpTree(r.unseal)), name)
    val expr = '{
      def wrapped: Boolean = ${opTree.render(wrapped = true)}
      val matched =
        if ($self.__inErrorAnalysis) wrapped
        else ${opTree.render(wrapped = false)}
      if (matched) Rule.asInstanceOf[Rule[I, O]] else null
    }

    //println(expr.show)
    expr
    /*import ctx.universe._
    val ruleTree = q"""
      def wrapped: Boolean = ${opTree.render(wrapped = true)}
      val matched =
        if (__inErrorAnalysis) wrapped
        else ${opTree.render(wrapped = false)}
      if (matched) org.http4s.internal.parboiled2.Rule else null""" // we encode the "matched" boolean as 'ruleResult ne null'

    reify { ctx.Expr[RuleX](ruleTree).splice.asInstanceOf[Rule[I, O]] }*/
  }
}

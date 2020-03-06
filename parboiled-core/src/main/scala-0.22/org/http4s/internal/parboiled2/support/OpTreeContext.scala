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

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import scala.quoted._
import scala.quoted.matching.Const
import org.http4s.internal.parboiled2.{CharPredicate, Parser, Rule, RuleTrace}

import scala.annotation.unchecked.uncheckedVariance

private[http4s] trait OpTreeContext[I: Type, O: Type, OpTreeCtx <: QuoteContext] {
  implicit val c: OpTreeCtx
  val parser: Expr[Parser]
  val P: Expr[Parser] = parser
  import c.tasty.{_, given _}

  sealed trait OpTree {
    // renders a Boolean Tree
    def render(wrapped: Boolean): Expr[Boolean]
  }

  sealed abstract class NonTerminalOpTree extends OpTree {
    def bubbleUp(e: Expr[Parser#TracingBubbleException], start: Expr[Int]): Expr[Nothing]

    // renders a Boolean Tree
    def render(wrapped: Boolean): Expr[Boolean] =
      if (wrapped) '{
        val start = $parser.cursor
        try ${renderInner(wrapped, 'start)}
        catch { case e: Parser#TracingBubbleException => ${bubbleUp('e, 'start)} }
      }
      else renderInner(wrapped, null)

    // renders a Boolean Tree
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean]
  }

  sealed abstract class DefaultNonTerminalOpTree extends NonTerminalOpTree {
    def bubbleUp(e: Expr[Parser#TracingBubbleException], start: Expr[Int]): Expr[Nothing] =
      '{$e.bubbleUp($ruleTraceNonTerminalKey, $start)}
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey]
  }

  sealed abstract class TerminalOpTree extends OpTree {
    def bubbleUp: Expr[Nothing] =
      '{$parser.__bubbleUp($ruleTraceTerminal)}

    // renders a Boolean Tree
    def render(wrapped: Boolean): Expr[Boolean] =
      if (wrapped) '{
        try ${renderInner(wrapped, null)} catch {
          case Parser.StartTracingException => $bubbleUp
        }
      } else renderInner(wrapped, null)

    def ruleTraceTerminal: Expr[RuleTrace.Terminal]

    // renders a Boolean Tree
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean]
  }

  sealed abstract class PotentiallyNamedTerminalOpTree(arg: Term) extends TerminalOpTree {
    override def bubbleUp: Expr[Nothing] = callName(arg) match {
      case Some(name) =>
        '{$parser.__bubbleUp(RuleTrace.NonTerminal(RuleTrace.Named(${Expr(name)}), 0) :: Nil, $ruleTraceTerminal)}
      case None       => super.bubbleUp
    }
      '{$parser.__bubbleUp($ruleTraceTerminal)}

    def ruleTraceTerminal: Expr[RuleTrace.Terminal]
  }

  def collector(lifterTree: Expr[_]): Collector =
    lifterTree.unseal.underlyingArgument match {
      case TypeApply(Ident("forRule0"), _) => rule0Collector
      case TypeApply(Ident("forRule1"), _) => rule1Collector
      case TypeApply(Ident("forReduction"), _) => rule0Collector
      //case TypeApply(Ident("forRule1"), _) => rule0Collector
      //case '{($a: Lifter.type).forRule0}             => rule0Collector
      //case '{($a: Lifter.type).forRule1}         => rule1Collector
      case x                                           => c.throwError(s"Unexpected Lifter: ${x.showExtractors}", lifterTree)
    }

  val opTreePF: PartialFunction[Expr[_], OpTree] = {
    case '{($x: Parser).ch(($c: Char))}                                 => CharMatch(c)
    case '{($x: Parser).str(($s: String))}                                 => StringMatch(s)
    case '{($x: Parser).valueMap(($m: Map[String, _]))}                  => MapMatch(m, '{false})
    case '{($x: Parser).valueMap(($m: Map[String, _]), ($ic: Boolean))}             => MapMatch(m, ic)
    case '{($x: Parser).ignoreCase(($c: Char))}                         => IgnoreCaseChar(c)
    case '{($x: Parser).ignoreCase(($s: String))}                         => IgnoreCaseString(s)
    case '{($x: Parser).predicate($p)}                          => CharPredicateMatch(p)
    case '{($x: Parser).anyOf($s)}                              => AnyOf(s)
    case '{($x: Parser).noneOf($s)}                             => NoneOf(s)
    case '{($x: Parser).ANY}                                    => ANY
    case '{($x: Parser).&($arg)}                                => AndPredicate(OpTree(arg.unseal))
    case '{($x: Parser).fail($m)}                                => Fail(m)
    case '{($x: Parser).failX($m)}                                => Fail(m)
    case other => other.unseal.underlyingArgument match {
      case Apply(Apply(TypeApply(Select(_, "optional"), _), List(arg)), List(l)) => Optional(OpTree(arg), collector(l.seal))
      case Apply(TypeApply(Select(lhs, "~"), _), List(rhs)) => Sequence(OpTree(lhs), OpTree(rhs))
      case Apply(TypeApply(Select(lhs, "~|~"), _), List(rhs)) => Cut(OpTree(lhs), OpTree(rhs))
      case Apply(TypeApply(Select(lhs, "|"), _), List(rhs)) => FirstOf(OpTree(lhs), OpTree(rhs))
      case Apply(Select(base, "?"), List(l)) => Optional(OpTree(base), collector(l.seal))

      case Apply(Apply(TypeApply(Select(_, "zeroOrMore"), _), List(arg)), List(l)) => ZeroOrMore(OpTree(arg), collector(l.seal))
      case Apply(Select(base, "*"), List(l)) => ZeroOrMore(OpTree(base), collector(l.seal))
      case Apply(Apply(Select(base, "*"), List(sep)), List(l)) => ZeroOrMore(OpTree(base), collector(l.seal), Separator(OpTree(sep)))

      case Apply(Apply(TypeApply(Select(_, "oneOrMore"), _), List(arg)), List(l)) => OneOrMore(OpTree(arg), collector(l.seal))
      case Apply(Select(base, "+"), List(l)) => OneOrMore(OpTree(base), collector(l.seal))
      case Apply(Apply(Select(base, "+"), List(sep)), List(l)) => OneOrMore(OpTree(base), collector(l.seal), Separator(OpTree(sep)))
      case Apply(Apply(TypeApply(Select(base, "times"), _), List(r)), List(l)) => Times(base, OpTree(r), collector(l.seal))
      case Select(base, "unary_!") => NotPredicate(OpTree(base))
      case Apply(TypeApply(Select(_, "atomic"), _), List(arg)) => Atomic(OpTree(arg))
      case Apply(TypeApply(Select(_, "quiet"), _), List(arg)) => Quiet(OpTree(arg))
      case Apply(Select(_, "test"), List(flag)) => SemanticPredicate(flag.seal.cast[Boolean])
      case Apply(TypeApply(Select(_, "capture"), _), List(arg)) => Capture(OpTree(arg))
      case Apply(TypeApply(Select(_, "failX"), _), List(m)) => Fail(m.seal.cast[String])
      case Apply(Select(base, "named"), List(name)) => Named(OpTree(base), name.seal.cast[String])
      case Apply(Apply(TypeApply(Select(_, "push"), _), List(arg)), List(h)) => PushAction(arg, h)
      case Apply(TypeApply(Select(_, "drop"), _), List(arg)) => DropAction(arg)
      case Apply(Apply(TypeApply(Select(_, "run"), _), List(arg)), List(Apply(TypeApply(Ident("fromAux"), _), List(rr)))) => RunAction(arg, rr)
      case Apply(Select(Apply(Select(_, "str2CharRangeSupport"), List(l)), "-"), List(r)) => CharRange(l, r)
      case Apply(TypeApply(Select(Apply(TypeApply(Ident("ActionOps1"), _), List(r)), "~>"), List(_)), List(f)) => Sequence(OpTree(r), Action(f))
      case Apply(TypeApply(Select(Apply(TypeApply(Ident("ActionOps2"), _), List(r)), "~>"), List(_)), List(f)) => Sequence(OpTree(r), Action(f))
      case call @ (Apply(_, _) | Select(_, _) | Ident(_) | TypeApply(_, _)) =>
        RuleCall(Right(call), Expr(callName(call).getOrElse(c.throwError(s"Illegal rule call: $call", call.seal))))
      case other => println(other.showExtractors); ???
    }
    /*
    case q"$a.this.runSubParser[$b, $c]($f)"               => RunSubParser(f)
    case q"$a.this.charAndValue[$t]($b.ArrowAssoc[$t1]($c).->[$t2]($v))($hl)" =>
      Sequence(CharMatch(c), PushAction(v, hl))
    case q"$a.this.stringAndValue[$t]($b.ArrowAssoc[$t1]($s).->[$t2]($v))($hl)" =>
      Sequence(StringMatch(s), PushAction(v, hl))
    case q"$a.this.rule2ActionOperator[$b1, $b2]($r)($o).~>.apply[..$e]($f)($g, support.this.FCapture.apply[$ts])" =>
      Sequence(OpTree(r), Action(f, ts))
    case x @ q"$a.this.rule2WithSeparatedBy[$b1, $b2]($base).separatedBy($sep)" =>
      OpTree(base) match {
        case x: WithSeparator => x.withSeparator(Separator(OpTree(sep)))
        case _                => c.abort(x.pos, "Illegal `separatedBy` base: " + base)
      }*/
  }

  def OpTree(tree: Term): OpTree =
    opTreePF.applyOrElse(tree.seal, (t: Expr[_]) => c.throwError(s"Invalid rule definition: ${t.show}*", t))

  def Sequence(lhs: OpTree, rhs: OpTree): Sequence =
    (lhs, rhs) match {
      case (Sequence(lops), Sequence(rops)) => Sequence(lops ++ rops)
      case (Sequence(lops), _)              => Sequence(lops :+ rhs)
      case (_, Sequence(ops))               => Sequence(lhs +: ops)
      case (_, _)                           => Sequence(Seq(lhs, rhs))
    }

  case class Sequence(ops: Seq[OpTree]) extends DefaultNonTerminalOpTree {
    require(ops.size >= 2)
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.Sequence}

    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] =
      ops
        .map(_.render(wrapped))
        .reduceLeft((_l, _r) => '{val l = $_l; if (l) $_r else false}) // work-around for https://issues.scala-lang.org/browse/SI-8657"
  }

  case class Cut(lhs: OpTree, rhs: OpTree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.Cut}
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = '{
      var matched = ${lhs.render(wrapped)}
      if (matched) {
        matched = ${rhs.render(wrapped)}
        if (!matched) throw org.http4s.internal.parboiled2.Parser.CutError
        true
      } else false // work-around for https://issues.scala-lang.org/browse/SI-8657
    }
  }

  def FirstOf(lhs: OpTree, rhs: OpTree): FirstOf =
    lhs -> rhs match {
      case (FirstOf(lops), FirstOf(rops)) => FirstOf(lops ++ rops)
      case (FirstOf(lops), _)             => FirstOf(lops :+ rhs)
      case (_, FirstOf(ops))              => FirstOf(lhs +: ops)
      case (_, _)                         => FirstOf(Seq(lhs, rhs))
    }

  case class FirstOf(ops: Seq[OpTree]) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.FirstOf}
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = '{
      val mark = $parser.__saveState; ${ops
        .map(_.render(wrapped))
        .reduceLeft(
          (_l, _r) =>
            '{val l = $_l; if (!l) { $parser.__restoreState(mark); $_r } else true} // work-around for https://issues.scala-lang.org/browse/SI-8657"
        )
      }
    }
  }

  case class CharMatch(charTree: Expr[Char]) extends TerminalOpTree {
    def ruleTraceTerminal: Expr[RuleTrace.Terminal] = '{RuleTrace.CharMatch($charTree)}

    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = if (wrapped) '{
      val unwrappedTree = $parser.cursorChar == $charTree && $parser.__advance()
      unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch()
    } else '{
      $parser.cursorChar == $charTree && $parser.__advance()
    }
  }

  case class StringMatch(stringTree: Expr[String]) extends OpTree {
    final private val autoExpandMaxStringLength = 8

    def render(wrapped: Boolean): Expr[Boolean] = {
      def unrollUnwrapped(s: String, ix: Int = 0): Expr[Boolean] =
        if (ix < s.length) '{
          if ($parser.cursorChar == ${Expr(s.charAt(ix))}) {
            $parser.__advance()
            ${unrollUnwrapped(s, ix + 1)}
          } else false
        } else '{true}

      def unrollWrapped(s: String, ix: Int = 0): Expr[Boolean] =
        if (ix < s.length) {
          val ch = s.charAt(ix)
          '{
            if ($parser.cursorChar == ${Expr(ch)}) {
              $parser.__advance()
              $parser.__updateMaxCursor()
              ${unrollWrapped(s, ix + 1)}
            } else {
              try $parser.__registerMismatch() catch {
                case Parser.StartTracingException =>
                  $parser.__bubbleUp(RuleTrace.NonTerminal(RuleTrace.StringMatch($stringTree), ${Expr(ix)}) :: Nil, RuleTrace.CharMatch(${Expr(ch)}))
              }
            }
          }
        } else '{true}

      stringTree match {
        case Const(s: String) if s.length <= autoExpandMaxStringLength =>
          if (s.isEmpty) '{true} else if (wrapped) unrollWrapped(s) else unrollUnwrapped(s)
        case _ =>
          if (wrapped) '{$parser.__matchStringWrapped($stringTree)}
          else '{$parser.__matchString($stringTree)}
      }
    }
  }

  case class MapMatch(mapTree: Expr[Map[String, Any]], ignoreCaseTree: Expr[Boolean]) extends OpTree {

    def render(wrapped: Boolean): Expr[Boolean] =
      if (wrapped) '{$parser.__matchMapWrapped($mapTree, $ignoreCaseTree)}
      else '{$parser.__matchMap($mapTree, $ignoreCaseTree)}
  }

  case class IgnoreCaseChar(charTree: Expr[Char]) extends TerminalOpTree {
    def ruleTraceTerminal: Expr[RuleTrace.Terminal] = '{RuleTrace.IgnoreCaseChar($charTree)}

    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = if (wrapped) '{
      val unwrappedTree = Character.toLowerCase($parser.cursorChar) == $charTree && $parser.__advance()
      unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch()
    } else '{
      Character.toLowerCase($parser.cursorChar) == $charTree && $parser.__advance()
    }
  }

  case class IgnoreCaseString(stringTree: Expr[String]) extends OpTree {
    final private val autoExpandMaxStringLength = 8

    def render(wrapped: Boolean): Expr[Boolean] = {
      def unrollUnwrapped(s: String, ix: Int = 0): Expr[Boolean] =
        if (ix < s.length) '{
          if (Character.toLowerCase($parser.cursorChar) == ${Expr(s.charAt(ix))}) {
            $parser.__advance()
            ${unrollUnwrapped(s, ix + 1)}
          } else false
        } else '{true}

      def unrollWrapped(s: String, ix: Int = 0): Expr[Boolean] =
        if (ix < s.length) {
          val ch = s.charAt(ix)
          '{
            if (Character.toLowerCase($parser.cursorChar) == ${Expr(ch)}) {
              $parser.__advance()
              $parser.__updateMaxCursor()
              ${unrollWrapped(s, ix + 1)}
            } else {
              try $parser.__registerMismatch() catch {
                case Parser.StartTracingException =>
                  $parser.__bubbleUp(RuleTrace.NonTerminal(RuleTrace.IgnoreCaseString($stringTree), ${Expr(ix)}) :: Nil, RuleTrace.IgnoreCaseChar(${Expr(ch)}))
              }
            }
          }
        } else '{true}

      stringTree match {
        case Const(s: String) if s.length <= autoExpandMaxStringLength =>
          if (s.isEmpty) '{true} else if (wrapped) unrollWrapped(s) else unrollUnwrapped(s)
        case _ =>
          if (wrapped) '{$parser.__matchIgnoreCaseStringWrapped($stringTree)}
          else '{$parser.__matchIgnoreCaseString($stringTree)}
      }
    }
  }

  case class CharPredicateMatch(predicateTree: Expr[CharPredicate]) extends PotentiallyNamedTerminalOpTree(predicateTree.unseal) {
    def ruleTraceTerminal: Expr[RuleTrace.Terminal] = '{RuleTrace.CharPredicateMatch($predicateTree)}
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = {
      val unwrappedTree = '{$predicateTree($parser.cursorChar) && $parser.__advance()}
      if (wrapped) '{$unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch()} else unwrappedTree
    }
  }

  case class AnyOf(stringTree: Expr[String]) extends TerminalOpTree {
    def ruleTraceTerminal: Expr[RuleTrace.Terminal] = '{RuleTrace.AnyOf($stringTree)}
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = {
      val unwrappedTree = '{$parser.__matchAnyOf($stringTree)}
      if (wrapped) '{$unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch()} else unwrappedTree
    }
  }

  case class NoneOf(stringTree: Expr[String]) extends TerminalOpTree {
    def ruleTraceTerminal: Expr[RuleTrace.Terminal] = '{RuleTrace.NoneOf($stringTree)}
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = {
      val unwrappedTree = '{$parser.__matchNoneOf($stringTree)}
      if (wrapped) '{$unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch()} else unwrappedTree
    }
  }

  case object ANY extends TerminalOpTree {
    def ruleTraceTerminal: Expr[RuleTrace.Terminal] = '{RuleTrace.ANY}
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = {
      val unwrappedTree = '{$parser.cursorChar != $parser.EOI && $parser.__advance()}
      if (wrapped) '{$unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch()} else unwrappedTree
    }
  }

  case class Optional(op: OpTree, collector: Collector) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.Optional}
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = '{
      val mark = $parser.__saveState
      val matched = ${op.render(wrapped)}
      if (matched) {
        ${collector.pushSomePop}
      } else {
        $parser.__restoreState(mark)
        ${collector.pushNone}
      }
      true
    }
  }

  sealed abstract class WithSeparator extends DefaultNonTerminalOpTree {
    def withSeparator(sep: Separator): OpTree
  }

  case class ZeroOrMore(op: OpTree, collector: Collector, separator: Separator = null) extends WithSeparator {
    def withSeparator(sep: Separator) = copy(separator = sep)
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.ZeroOrMore}

    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = '{
      val builder = ${collector.valBuilder}

      @tailrec def rec(mark: Parser.Mark): Parser.Mark = {
        val matched = ${op.render(wrapped)}
        if (matched) {
          ${collector.popToBuilder('builder)}
          ${
            if (separator.eq(null)) '{rec($parser.__saveState)}
            else '{val m = $parser.__saveState; if (${separator(wrapped)}) rec(m) else m}
          }
        } else mark
      }

      $parser.__restoreState(rec($parser.__saveState))
      ${collector.pushBuilderResult('builder)}
    }
  }

  case class OneOrMore(op: OpTree, collector: Collector, separator: Separator = null) extends WithSeparator {
    def withSeparator(sep: Separator) = copy(separator = sep)
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.OneOrMore}

    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = '{
      val firstMark = $parser.__saveState
      val builder = ${collector.valBuilder}

      @tailrec def rec(mark: Parser.Mark): Parser.Mark = {
        val matched = ${op.render(wrapped)}
        if (matched) {
          ${collector.popToBuilder('builder)}
          ${
            if (separator.eq(null)) '{rec($parser.__saveState)}
            else '{val m = $parser.__saveState; if (${separator(wrapped)}) rec(m) else m}
          }
        } else mark
      }

      val mark = rec(firstMark)
      mark != firstMark && {
        $parser.__restoreState(mark)
        ${collector.pushBuilderResult('builder)}
      }
    }
  }

  def Times(base: Term, rule: OpTree, collector: Collector, separator: Separator = null): OpTree =
    base match {
      case Apply(Select(_, "int2NTimes"), List(n)) =>
        n match {
          case Literal(Constant(i: Int)) =>
            if (i <= 0) c.throwError("`x` in `x.times` must be positive", base.seal)
            else if (i == 1) rule
            else Times(rule, Expr(i), identity, collector, separator)
          case x @ (Ident(_) | Select(_, _)) => Times(rule, n.seal.cast[Int], identity, collector, separator)
          case _                             => c.throwError(s"Invalid int base expression for `.times(...)`: $n", n.seal)
        }
      case Apply(Select(_, "range2NTimes"), List(r)) =>
        r match {
          case Apply(Select(Apply(Ident("intWrapper"), List(mn)), "to"), List(mx)) =>
            mn match {
              case Literal(Constant(min: Int)) =>
                if (min <= 0) c.throwError("`min` in `(min to max).times` must be positive", mn.seal)
              case (Ident(_) | Select(_, _)) => ()
              case _                         => c.throwError(s"Invalid int range expression for `min` in `.times(...)`: $r", r.seal)
            }
            mx match {
              case Literal(Constant(max: Int)) =>
                if (max <= 0) c.throwError("`max` in `(min to max).times` must be positive", mx.seal)
              case (Ident(_) | Select(_, _)) => ()
              case _                         => c.throwError(s"Invalid int range expression for `max` in `.times(...)`: $r", r.seal)
            }
            (mn, mx) match {
              case (Literal(Constant(min: Int)), Literal(Constant(max: Int))) =>
                if (max < min) c.throwError("`max` in `(min to max).times` must be >= `min`", mx.seal)
              case _ => ()
            }
            Times(rule, mn.seal.cast[Int], _ => mx.seal.cast[Int], collector, separator)
          case x @ (Ident(_) | Select(_, _)) =>
            // TODO: Don't include the range twice.
            Times(rule, '{${r.seal.cast[Range]}.start}, _ => '{${r.seal.cast[Range]}.end}, collector, separator)
          case _ => c.throwError(s"Invalid range base expression for `.times(...)`: $r", r.seal)
        }
      case _ => c.throwError(s"Invalid base expression for `.times(...)`: $base", base.seal)
    }

  case class Times(op: OpTree, min: Expr[Int], max: Expr[Int] => Expr[Int], collector: Collector, separator: Separator) extends WithSeparator {
    def withSeparator(sep: Separator) = copy(separator = sep)
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.Times($min, ${max(min)})}

    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = '{
      val builder = ${collector.valBuilder}
      val _min = $min
      val _max = ${max(min)}
      require(_min <= _max, "`max` in `(min to max).times` must be >= `min`")

      @tailrec def rec(count: Int, mark: Parser.Mark): Boolean = {
        val matched = ${op.render(wrapped)}
        if (matched) {
          ${collector.popToBuilder('builder)}
          if (count < _max) ${
            if (separator.eq(null)) '{rec(count + 1, $parser.__saveState)}
            else '{
              val m = $parser.__saveState
              if (${separator(wrapped)}) rec(count + 1, m)
                else (count >= _min) && { $parser.__restoreState(m); true }
            }
          } else true
        } else (count > _min) && { $parser.__restoreState(mark); true }
      }


      (_max <= 0) || rec(1, $parser.__saveState) && ${collector.pushBuilderResult('builder)}
    }
  }

  case class AndPredicate(op: OpTree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.AndPredicate}
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = '{
      val mark = $parser.__saveState
      val matched = ${op.render(wrapped)}
      $parser.__restoreState(mark)
      matched
    }
  }

  case class NotPredicate(op: OpTree) extends OpTree {
    def base: Expr[RuleTrace.NotPredicate.Base] = op match {
      case x: TerminalOpTree   => '{RuleTrace.NotPredicate.Terminal(${x.ruleTraceTerminal})}
      case x: RuleCall         => '{RuleTrace.NotPredicate.RuleCall(${x.calleeNameTree})}
      case x: StringMatch      => '{RuleTrace.NotPredicate.Named("\"" + ${x.stringTree} + '"')}
      case x: IgnoreCaseString => '{RuleTrace.NotPredicate.Named("\"" + ${x.stringTree} + '"')}
      case x: Named            => '{RuleTrace.NotPredicate.Named(${x.stringTree})}
      case _                   =>' {RuleTrace.NotPredicate.Anonymous}
    }

    def render(wrapped: Boolean): Expr[Boolean] = if (wrapped) '{
      var matchEnd = 0
      try {
        {
          val mark = $parser.__saveState
          val saved = $parser.__enterNotPredicate()
          val matched = ${op.render(wrapped)}
          $parser.__exitNotPredicate(saved)
          ${if (wrapped) '{matchEnd = $parser.cursor} else '{()}}
          $parser.__restoreState(mark)
          !matched
        } || $parser.__registerMismatch()
      } catch {
        case Parser.StartTracingException => $parser.__bubbleUp {
          RuleTrace.NotPredicate($base, matchEnd - $parser.cursor)
        }
      }
    } else '{
      var matchEnd = 0
      val mark = $parser.__saveState
      val saved = $parser.__enterNotPredicate()
      val matched = ${op.render(wrapped)}
      $parser.__exitNotPredicate(saved)
      ${if (wrapped) '{matchEnd = $parser.cursor} else '{()}}
      $parser.__restoreState(mark)
      !matched
    }
  }

  case class Atomic(op: OpTree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.Atomic}
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] =
      if (wrapped) '{
        val saved = $parser.__enterAtomic($start)
        val matched = ${op.render(wrapped)}
        $parser.__exitAtomic(saved)
        matched
      } else op.render(wrapped)
  }

  case class Quiet(op: OpTree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.Atomic}
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] =
      if (wrapped) '{
        val saved = $parser.__enterQuiet()
        val matched = ${op.render(wrapped)}
        $parser.__exitQuiet(saved)
        matched
      } else op.render(wrapped)
  }


  case class SemanticPredicate(flagTree: Expr[Boolean]) extends TerminalOpTree {
    def ruleTraceTerminal: Expr[RuleTrace.Terminal] = '{RuleTrace.SemanticPredicate}

    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] =
      if (wrapped) '{$flagTree || $parser.__registerMismatch()} else flagTree
  }

  case class Capture(op: OpTree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.Capture}
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] =
      if (wrapped) '{
        val matched = ${op.render(wrapped)}
        if (matched) {
          $parser.valueStack.push($parser.input.sliceString($start, $parser.cursor))
          true
        } else false
      } else '{
        val start = $parser.cursor
        val matched = ${op.render(wrapped)}
        if (matched) {
          $parser.valueStack.push($parser.input.sliceString(start, $parser.cursor))
          true
        } else false
      }
  }

  case class RunAction(argTree: Term, rrTree: Term) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.Run}
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = {
      def renderFunctionAction(resultTypeTree: TypeTree, argTypeTrees: TypeTree*): Expr[Boolean] = {
        def actionBody(tree: Term): Expr[Boolean] =
          tree match {
            case Block(List(DefDef(_, Nil, List(args), _, Some(body))), _) =>
              def rewrite(tree: Term): Term =
                tree match {
                  case Block(statements, res)                         => Block(statements, rewrite(res))
                  case x if resultTypeTree.tpe <:< typeOf[Rule[_, _]] => expand(x, wrapped)
                  case x                                              => '{$parser.__push(${x.seal})}.unseal
                }
              val valDefs = args
                .zip(argTypeTrees)
                .map { case (d @ ValDef(name, tpe, None), t) => ValDef.copy(d)(name, tpe, Some('{$parser.valueStack.pop()}.unseal)) }
                .reverse
              Block(valDefs, rewrite(body)).seal.cast[Boolean]
            case Block(statements, res) => Block(statements, actionBody(res).unseal).seal.cast[Boolean]
            case x => c.throwError(s"Unexpected `run` argument: ${x.showExtractors}", argTree.seal)
          }

        actionBody(argTree)
      }

      rrTree match {
        case TypeApply(Ident("forAny"), _) => '{${argTree.seal}; true}
        case TypeApply(Ident("forRule"), _) => expand(argTree, wrapped).seal.cast[Boolean]
        case Apply(TypeApply(Ident("forF1"), List(z, r, _, _)), _) => renderFunctionAction(r, z)
        case Apply(TypeApply(Ident("forF2"), List(y, z, r, _, _)), _) => renderFunctionAction(r, y, z)
        case Apply(TypeApply(Ident("forF3"), List(x, y, z, r, _, _)), _) => renderFunctionAction(r, x, y, z)
        case Apply(TypeApply(Ident("forF4"), List(w, x, y, z, r, _, _)), _) => renderFunctionAction(r, w, x, y, z)
        case Apply(TypeApply(Ident("forF5"), List(v, w, x, y, z, r, _, _)), _) => renderFunctionAction(r, v, w, x, y, z)
        case Apply(TypeApply(Ident("forF"), List(v, w, x, y, z, r, _, _)), _) => renderFunctionAction(r, v, w, x, y, z)
        case x => c.throwError(s"Unexpected RunResult.Aux: ${x.show}", rrTree.seal)
      }
    }
  }

  case class PushAction(argTree: Term, hlTree: Term) extends OpTree {
    def render(wrapped: Boolean): Expr[Boolean] = 
      hlTree.underlyingArgument match {
        case Ident("fromUnit") => '{true}
        case TypeApply(Ident("fromHList"), _) => '{$parser.valueStack.pushAll(${argTree.seal.cast[HList]}); true}
        case TypeApply(Ident("fromAnyRef"), _) => '{$parser.valueStack.push(${argTree.seal}); true}
        case x => c.throwError(s"Unexpected HListable: ${x.show}", hlTree.seal)
      }
  }

  case class DropAction(hlTree: Term) extends OpTree {
    def render(wrapped: Boolean): Expr[Boolean] =
      hlTree.underlyingArgument match {
        case Ident("fromUnit") => '{true}
        case TypeApply(Ident("fromAnyRef"), _) => '{$parser.valueStack.pop(); true}
        case TypeApply(Ident("fromHList"), List(t)) =>
          @tailrec def rec(t: Type, result: Expr[Boolean]): Expr[Boolean] = t match {
            case AppliedType(TypeRef(_, "::"), List(_, tail: Type)) =>
              rec(tail, '{$parser.valueStack.pop(); $result})
            case TypeRef(_, "HNil") => result
          }
          rec(t.tpe, '{true})
        case x => c.throwError(s"Unexpected HListable: ${x.show}", hlTree.seal)
      }
  }

  case class RuleCall(call: Either[OpTree, Term], calleeNameTree: Expr[String]) extends NonTerminalOpTree {
    def bubbleUp(e: Expr[Parser#TracingBubbleException], start: Expr[Int]): Expr[Nothing] =
      '{
        $e.prepend(RuleTrace.RuleCall, $start).bubbleUp(RuleTrace.Named($calleeNameTree), $start)
      }

    override def render(wrapped: Boolean): Expr[Boolean] =
      call match {
        case Left(_)  => super.render(wrapped)
        case Right(x) => '{${x.seal.cast[AnyRef]} ne null}
      }

    def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = {
      val Left(value) = call.asInstanceOf[Left[OpTree, Term]]
      value.render(wrapped)
    }
  }

  def CharRange(lowerTree: Term, upperTree: Term): CharacterRange = {
    val (lower, upper) = (lowerTree, upperTree) match {
      case (Literal(Constant(l: String)), Literal(Constant(u: String))) => (l, u)
      case _                                                            => c.throwError("Character ranges must be specified with string literals", lowerTree.seal)
    }
    if (lower.length != 1) c.throwError("lower bound must be a single char string", lowerTree.seal)
    if (upper.length != 1) c.throwError("upper bound must be a single char string", upperTree.seal)
    val lowerBoundChar = lower.charAt(0)
    val upperBoundChar = upper.charAt(0)
    if (lowerBoundChar > upperBoundChar) c.throwError("lower bound must not be > upper bound", lowerTree.seal)
    CharacterRange(lowerBoundChar, upperBoundChar)
  }

  case class CharacterRange(lowerBound: Char, upperBound: Char) extends TerminalOpTree {
    def ruleTraceTerminal: Expr[RuleTrace.Terminal] = '{RuleTrace.CharRange(${Expr(lowerBound)}, ${Expr(upperBound)})}

    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = {
      val unwrappedTree = '{
        val char = $parser.cursorChar
        ${Expr(lowerBound)} <= char && char <= ${Expr(upperBound)} && $parser.__advance()
      }
      if (wrapped) '{$unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch()} else unwrappedTree
    }
  }

  case class Action(actionTree: Term) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.Action}

    val actionType: List[Type] = actionTree.tpe match {
      case AppliedType(_, args) if args.nonEmpty => args.collect { case arg: Type => arg }
      case x                                     => c.throwError(s"Unexpected action type: $x" + x, actionTree.seal)
    }

    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = {
      val argTypes = actionType.dropRight(1)

      def popToVals(vals: List[ValDef]): List[ValDef] =
        vals.zip(argTypes).map {
          case (d @ ValDef(name, tpe, None), t) => ValDef.copy(d)(name, tpe, Some('{$parser.valueStack.pop()}.unseal))
        }.reverse

      def actionBody(tree: Term): Term =
        tree match {
          /*case x @ (Ident(_) | Select(_, _)) =>
            val valNames = argTypes.indices.map { i =>
              TermName("value" + i)
            }.toList
            val args = valNames map Ident.apply
            block(popToVals(valNames), q"__push($x(..$args))")*/

          case Block(List(DefDef(_, Nil, List(args), _, Some(body))), _) =>
            def rewrite(tree: Term): Term =
              tree match {
                case Block(statements, res)                         => Block(statements, rewrite(res))
                case x if actionType.last <:< typeOf[Rule[_, _]] => expand(x, wrapped)
                case x                                              => '{$parser.__push(${x.seal})}.unseal
              }
            Block(popToVals(args), rewrite(body))
          case Block(statements, res) => Block(statements, actionBody(res))
        }

      actionBody(actionTree).seal.cast[Boolean]
    }
  }
  /*

  case class RunSubParser(fTree: Tree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = reify(RuleTrace.RunSubParser).tree

    def renderInner(wrapped: Boolean): Tree = {
      def rewrite(arg: TermName, tree: Tree): Tree =
        tree match {
          case Block(statements, res) => block(statements, rewrite(arg, res))
          case q"$p.$rule"            => q"""
            val $arg = new __SubParserInput()  // TODO: avoid re-allocation by re-using a cached instance
            val __subParser = $p
            val offset = cursor
            __subParser.copyStateFrom(this, offset)
            try __subParser.$rule ne null
            finally this.copyStateFrom(__subParser, -offset)"""
          case x                      => c.abort(x.pos, "Illegal runSubParser expr: " + show(x))
        }

      val q"($arg => $body)" = c.untypecheck(fTree)
      rewrite(arg.name, body)
    }
  }
  */

  case class Fail(stringTree: Expr[String]) extends OpTree {
    def render(wrapped: Boolean): Expr[Boolean] = '{throw new Parser.Fail($stringTree)}
  }

  case class Named(op: OpTree, stringTree: Expr[String]) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey] = '{RuleTrace.Named($stringTree)}
    protected def renderInner(wrapped: Boolean, start: Expr[Int]): Expr[Boolean] = op.render(wrapped)
  }

  /////////////////////////////////// helpers ////////////////////////////////////

  class Collector(
      val valBuilder: Expr[VectorBuilder[Any]],
      val popToBuilder: Expr[VectorBuilder[Any]] => Expr[Unit],
      val pushBuilderResult: Expr[VectorBuilder[Any]] => Expr[Boolean],
      val pushSomePop: Expr[Unit],
      val pushNone: Expr[Unit]
  )

  lazy val rule0Collector = {
    val unit = '{()}
    new Collector('{null}, _ => unit, _ => '{true}, unit, unit)
  }

  lazy val rule1Collector = new Collector(
    valBuilder = '{new scala.collection.immutable.VectorBuilder[Any]},
    popToBuilder = builder => '{$builder += $parser.valueStack.pop(); ()},
    pushBuilderResult = builder => '{$parser.valueStack.push($builder.result()); true},
    pushSomePop = '{$parser.valueStack.push(Some($parser.valueStack.pop()))},
    pushNone = '{$parser.valueStack.push(None)}
  )

  type Separator = Boolean => Expr[Boolean]

  def Separator(op: OpTree): Separator = wrapped => op.render(wrapped)

  // tries to match and expand the leaves of the given Tree
  def expand(tree: Term, wrapped: Boolean): Term =
    tree match {
      case Block(statements, res)     => Block(statements, expand(res, wrapped))
      case If(cond, thenExp, elseExp) => If(cond, expand(thenExp, wrapped), expand(elseExp, wrapped))
      case Match(selector, cases)     => Match(
        selector,
        cases.map { 
          case CaseDef(pat, guard, body) => CaseDef(pat, guard, expand(body, wrapped))
        }
      )
      case x                          => opTreePF.andThen(_.render(wrapped)).applyOrElse(tree.seal, (t: Expr[_]) => '{$t.asInstanceOf[AnyRef].ne(null)}).unseal
    }

  @tailrec
  private def callName(tree: Term): Option[String] =
    tree.underlyingArgument match {
      case Ident(name)       => Some(name)
      case Select(_, name)   => Some(name)
      case Apply(fun, _)     => callName(fun)
      case TypeApply(fun, _) => callName(fun)
      case _                 => None
    }
}

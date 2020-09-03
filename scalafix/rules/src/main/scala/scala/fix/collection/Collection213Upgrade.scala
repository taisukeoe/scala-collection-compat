/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.fix.collection

import scala.meta._

import scalafix.v1._

class Collection213Upgrade extends SemanticRule("Collection213Upgrade") {

  //  == Symbols ==

  val tupleZipped = SymbolMatcher.normalized(
    "scala/runtime/Tuple2Zipped.Ops#zipped().",
    "scala/runtime/Tuple3Zipped.Ops#zipped()."
  )
  val retainMap = SymbolMatcher.normalized(
    "scala/collection/mutable/MapLike#retain()."
  )
  val retainSet = SymbolMatcher.normalized(
    "scala/collection/mutable/SetLike#retain()."
  )

  // == Rules ==

  def replaceSymbols(implicit ctx: SemanticContext): Patch = {
    Patch.replaceSymbols(
      "scala.TraversableOnce"            -> "scala.IterableOnce",
      "scala.collection.TraversableOnce" -> "scala.collection.IterableOnce"
    )
  }

  def replaceMutableSet(implicit ctx: SemanticDocument): Patch = {
    ctx.tree.collect {
      case retainSet(n: Name) =>
        Patch.replaceTree(n, "filterInPlace")
    }.asPatch
  }

  private def trailingParens(tree: Tree,
                             ctx: SemanticDocument): Option[(Token.LeftParen, Token.RightParen)] =
    for {
      end   <- tree.tokens.lastOption
      open  <- ctx.tokenList.find(end)(_.is[Token.LeftParen]).map(_.asInstanceOf[Token.LeftParen])
      close <- ctx.matchingParens.close(open)
    } yield (open, close)

  def replaceMutableMap(implicit ctx: SemanticDocument): Patch = {
    ctx.tree.collect {
      case Term.Apply(Term.Select(_, retainMap(n: Name)), List(_: Term.PartialFunction)) =>
        Patch.replaceTree(n, "filterInPlace")

      case Term.Apply(Term.Select(_, retainMap(n: Name)), List(_: Term.Function)) =>
        this
          .trailingParens(n, ctx)
          .map {
            case (open, close) =>
              Patch.replaceToken(open, "{case ") +
                Patch.replaceToken(close, "}") +
                Patch.replaceTree(n, "filterInPlace")
          }
          .asPatch
    }.asPatch
  }

  def replaceTupleZipped(implicit ctx: SemanticDocument): Patch = {
    ctx.tree.collect {
      case tupleZipped(Term.Select(Term.Tuple(args), name)) =>
        val removeTokensPatch =
          (for {
            zipped     <- name.tokens.headOption
            closeTuple <- ctx.tokenList.leading(zipped).find(_.is[Token.RightParen])
            openTuple  <- ctx.matchingParens.open(closeTuple.asInstanceOf[Token.RightParen])
            maybeDot = ctx.tokenList.slice(closeTuple, zipped).find(_.is[Token.Dot])
          } yield {
            Patch.removeToken(openTuple) +
              maybeDot.map(Patch.removeToken).asPatch +
              Patch.removeToken(zipped)
          }).asPatch

        def removeSurroundingWhiteSpaces(tk: Token) =
          (ctx.tokenList.trailing(tk).takeWhile(_.is[Token.Space]).map(Patch.removeToken) ++
            ctx.tokenList.leading(tk).takeWhile(_.is[Token.Space]).map(Patch.removeToken)).asPatch

        val commas =
          for {
            (prev, next) <- args.zip(args.tail)
            tokensBetweenArgs = ctx.tokenList.slice(prev.tokens.last, next.tokens.head)
            comma <- tokensBetweenArgs.find(_.is[Token.Comma])
          } yield comma

        val replaceCommasPatch = commas match {
          case head :: tail =>
            Patch.replaceToken(head, ".lazyZip(") +
              removeSurroundingWhiteSpaces(head) ++
              tail.map { comma =>
                Patch.replaceToken(comma, ").lazyZip(") +
                  removeSurroundingWhiteSpaces(comma)
              }
          case _ => Patch.empty
        }

        removeTokensPatch + replaceCommasPatch
    }.asPatch
  }

  override def fix(implicit ctx: SemanticDocument): Patch = {
    replaceSymbols(ctx) +
      replaceTupleZipped(ctx) +
      replaceMutableMap(ctx) +
      replaceMutableSet(ctx)
  }
}

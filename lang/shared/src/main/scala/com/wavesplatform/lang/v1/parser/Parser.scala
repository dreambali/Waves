package com.wavesplatform.lang.v1.parser

import Expressions._
import BinaryOperation._
import fastparse.{WhitespaceApi, core}
import scodec.bits.ByteVector

object Parser {

  private val Global = com.wavesplatform.lang.hacks.Global // Hack for IDEA

  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" ", "\t", "\r", "\n").rep)
  }

  import White._
  import fastparse.noApi._
  private val Base58Chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  private val keywords    = Set("let", "base58", "true", "false", "if", "then", "else")

  private val lowerChar             = CharIn('a' to 'z')
  private val upperChar             = CharIn('A' to 'Z')
  private val char                  = lowerChar | upperChar
  private val digit                 = CharIn('0' to '9')
  private val unicodeSymbolP        = P("u" ~ P(digit | char) ~ P(digit | char) ~ P(digit | char) ~ P(digit | char))
  private val escapedUnicodeSymbolP = P("\\" ~ (CharIn("\"\\bfnrt") | unicodeSymbolP))
  private val varName: P[NAME] = (char.repX(min = 1, max = 1) ~~ (digit | char).repX()).!.map { x =>
    if (keywords.contains(x)) NAME.INVALID(x, "keywords are restricted")
    else NAME.VALID(x)
  }

  private val numberP: P[CONST_LONG] = P(CharIn("+-").rep(max = 1) ~ digit.repX(min = 1)).!.map(t => CONST_LONG(t.toLong))
  private val trueP: P[TRUE.type]    = P("true").map(_ => TRUE)
  private val falseP: P[FALSE.type]  = P("false").map(_ => FALSE)
  private val bracesP: P[EXPR]       = P("(" ~ expr ~ ")")
  private val curlyBracesP: P[EXPR]  = P("{" ~ expr ~ "}")
  private val letP: P[LET]           = P("let" ~ varName ~ "=" ~ expr).map(Function.tupled(LET)).log("let")
  private val refP: P[REF]           = P(varName).map(REF)
  private val ifP: P[IF]             = P("if" ~ bracesP ~ "then" ~ expr ~ "else" ~ expr).map { case (x, y, z) => IF(x, y, z) }

  private val functionCallArgs: P[Seq[EXPR]] = expr.rep(sep = ",")

  private val functionCallP: P[EXPR] = P(varName ~~ "(" ~ functionCallArgs ~ ")").map {
    case (name, args) => FUNCTION_CALL(name, args.toList)
  }

  private val extractableAtom: P[EXPR] = P(curlyBracesP | bracesP | functionCallP | refP).log("extractableAtom")

  private val maybeGetterP: P[EXPR] = P(extractableAtom ~ ("." ~ varName).?)
    .map {
      case (e, f) => f.fold(e)(GETTER(e, _))
    }
    .log("maybeGetter")

  private val byteVectorP: P[EXPR] =
    P("base58'" ~/ Pass ~~ CharsWhileIn(Base58Chars, 0).! ~~ "'")
      .map { x =>
        if (x.isEmpty) Right(Array.emptyByteArray) else Global.base58Decode(x)
      }
      .map {
        case Left(e)   => INVALID(e, None)
        case Right(xs) => CONST_BYTEVECTOR(ByteVector(xs))
      }

  private val stringP: P[CONST_STRING] =
    P("\"" ~~ (CharsWhile(!"\"\\".contains(_: Char)) | escapedUnicodeSymbolP).rep.! ~~ "\"").map(CONST_STRING)

  private val block: P[EXPR] = P(letP ~ expr).map(Function.tupled(BLOCK)).log("block")

  private val invalid: P[INVALID] = P(AnyChars(1).! ~ expr.?).log("invalid").map {
    case (xs, next) => foldInvalid(xs, next)
  }

  private def foldInvalid(xs: String, next: Option[EXPR]): INVALID = next match {
    case Some(INVALID(nextXs, nextNext)) => foldInvalid(xs + nextXs, nextNext)
    case x                               => INVALID(xs, x)
  }

  private val atom = P(ifP | byteVectorP | stringP | numberP | trueP | falseP | block | maybeGetterP | invalid).log("atom")

  private lazy val expr = P(binaryOp(opsByPriority) | atom).log("expr")

  private def binaryOp(rest: List[(String, BinaryOperation)]): P[EXPR] = rest match {
    case Nil => atom
    case (lessPriorityOp, kind) :: restOps =>
      val operand = binaryOp(restOps)
      P(operand ~ (lessPriorityOp.!.map(_ => kind) ~ operand).rep()).map {
        case (left: EXPR, r: Seq[(BinaryOperation, EXPR)]) =>
          r.foldLeft(left) { case (acc, (currKind, currOperand)) => BINARY_OP(acc, currKind, currOperand) }
      }
  }

  def apply(str: String): core.Parsed[Seq[EXPR], Char, String] = P(Start ~ expr.rep(min = 1) ~ End).parse(str)
}

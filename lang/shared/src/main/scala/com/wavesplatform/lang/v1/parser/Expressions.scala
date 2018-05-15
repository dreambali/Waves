package com.wavesplatform.lang.v1.parser

import scodec.bits.ByteVector

object Expressions {

  sealed trait PART {
    def v: String
  }

  object PART {
    case class VALID(v: String)                    extends PART
    case class INVALID(v: String, message: String) extends PART
  }

  case class LET(name: PART, value: EXPR)

  sealed trait EXPR
  case class CONST_LONG(value: Long)                             extends EXPR
  case class GETTER(ref: EXPR, field: PART)                      extends EXPR
  case class CONST_BYTEVECTOR(value: ByteVector)                 extends EXPR
  case class CONST_STRING(value: PART)                           extends EXPR
  case class BINARY_OP(a: EXPR, kind: BinaryOperation, b: EXPR)  extends EXPR
  case class BLOCK(let: LET, body: EXPR)                         extends EXPR
  case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)         extends EXPR
  case class REF(key: PART)                                      extends EXPR
  case object TRUE                                               extends EXPR
  case object FALSE                                              extends EXPR
  case class FUNCTION_CALL(functionName: PART, args: List[EXPR]) extends EXPR
  case class INVALID(message: String, next: Option[EXPR] = None) extends EXPR
  object INVALID {
    def apply(message: String, next: EXPR): INVALID = INVALID(message, Some(next))
  }

  implicit class NameOps(val self: PART) extends AnyVal {
    def toEither: Either[String, String] = self match {
      case Expressions.PART.VALID(x)            => Right(x)
      case Expressions.PART.INVALID(x, message) => Left(s"$message: $x")
    }
  }

}

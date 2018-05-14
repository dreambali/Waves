package com.wavesplatform.lang.v1.parser

import scodec.bits.ByteVector

object Expressions {

  sealed trait NAME {
    def name: String
  }

  object NAME {
    case class VALID(name: String)                    extends NAME
    case class INVALID(name: String, message: String) extends NAME
  }

  case class LET(name: NAME, value: EXPR)

  sealed trait EXPR
  case class CONST_LONG(value: Long)                             extends EXPR
  case class GETTER(ref: EXPR, field: NAME)                      extends EXPR
  case class CONST_BYTEVECTOR(value: ByteVector)                 extends EXPR
  case class CONST_STRING(value: String)                         extends EXPR
  case class BINARY_OP(a: EXPR, kind: BinaryOperation, b: EXPR)  extends EXPR
  case class BLOCK(let: LET, body: EXPR)                         extends EXPR
  case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)         extends EXPR
  case class REF(key: NAME)                                      extends EXPR
  case object TRUE                                               extends EXPR
  case object FALSE                                              extends EXPR
  case class FUNCTION_CALL(functionName: NAME, args: List[EXPR]) extends EXPR
  case class INVALID(message: String, next: Option[EXPR] = None) extends EXPR

  implicit class NameOps(val self: NAME) extends AnyVal {
    def toEither: Either[String, String] = self match {
      case Expressions.NAME.VALID(x)            => Right(x)
      case Expressions.NAME.INVALID(x, message) => Left(s"$message: $x")
    }
  }

}

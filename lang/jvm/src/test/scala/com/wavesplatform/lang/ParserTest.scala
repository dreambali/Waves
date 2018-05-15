package com.wavesplatform.lang

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.Expressions._
import com.wavesplatform.lang.v1.parser.{BinaryOperation, Parser}
import com.wavesplatform.lang.v1.testing.ScriptGenParser
import fastparse.core.Parsed.{Failure, Success}
import org.scalacheck.Gen
import org.scalatest.exceptions.TestFailedException
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.crypto.encode.{Base58 => ScorexBase58}

class ParserTest extends PropSpec with PropertyChecks with Matchers with ScriptGenParser with NoShrink {

  private def parseOne(x: String): EXPR = Parser(x) match {
    case Success(r, _) =>
      if (r.size > 1) throw new TestFailedException(s"Expected 1 expression, but got ${r.size}: $r", 0)
      else r.head
    case e @ Failure(_, i, _) =>
      println(
        s"Can't parse (len=${x.length}): <START>\n$x\n<END>\nError: $e\nPosition ($i): '${x.slice(i, i + 1)}'\nTraced:\n${e.extra.traced.fullStack
          .mkString("\n")}")
      throw new TestFailedException("Test failed", 0)
  }

  private def parseAll(x: String): Seq[EXPR] = Parser(x) match {
    case Success(r, _) =>
      println(r)
      r
    case e @ Failure(_, i, _) =>
      println(
        s"Can't parse (len=${x.length}): <START>\n$x\n<END>\nError: $e\nPosition ($i): '${x.slice(i, i + 1)}'\nTraced:\n${e.extra.traced.fullStack
          .mkString("\n")}")
      throw new TestFailedException("Test failed", 0)
  }

  private def isParsed(x: String): Boolean = Parser(x) match {
    case r @ Success(_, _) =>
      println(s"code:\n$x\nexpr: $r")
      true
    case Failure(_, _, _) => false
  }

  private def genElementCheck(gen: Gen[EXPR]): Unit = {
    val testGen: Gen[(EXPR, String)] = for {
      expr <- gen
      str  <- toString(expr)
    } yield (expr, str)

    forAll(testGen) {
      case (expr, str) =>
        parseOne(str) shouldBe expr
    }
  }

  property("should produce INVALID on invalid input") {
    val script =
      """let C = 1
        |# /
        |true""".stripMargin
    parseOne(script) shouldBe BLOCK(
      LET(NAME.VALID("C"), CONST_LONG(1)),
      INVALID(
        "#/",
        Some(TRUE)
      )
    )
  }

  property("show parse all input including INVALID") {
    val script =
      """let C = 1
        |foo
        |#@2
        |true""".stripMargin
    parseAll(script) shouldBe Seq(
      BLOCK(
        LET(NAME.VALID("C"), CONST_LONG(1)),
        REF(NAME.VALID("foo"))
      ),
      INVALID("#@", Some(CONST_LONG(2))),
      TRUE
    )
  }

  property("all types of multiline expressions") {
    val gas = 50
    genElementCheck(CONST_LONGgen.map(_._1))
    genElementCheck(STRgen)
    genElementCheck(REFgen)
    genElementCheck(BOOLgen(gas).map(_._1))
    genElementCheck(SUMgen(gas).map(_._1))
    genElementCheck(EQ_INTgen(gas).map(_._1))
    genElementCheck(INTGen(gas).map(_._1))
    genElementCheck(GEgen(gas).map(_._1))
    genElementCheck(GTgen(gas).map(_._1))
    genElementCheck(ANDgen(gas).map(_._1))
    genElementCheck(ORgen(gas).map(_._1))
    genElementCheck(BLOCKgen(gas))
  }

  property("priority in binary expressions") {
    parseOne("1 == 0 || 3 == 2") shouldBe BINARY_OP(BINARY_OP(CONST_LONG(1), EQ_OP, CONST_LONG(0)),
                                                    OR_OP,
                                                    BINARY_OP(CONST_LONG(3), EQ_OP, CONST_LONG(2)))
    parseOne("3 + 2 > 2 + 1") shouldBe BINARY_OP(BINARY_OP(CONST_LONG(3), SUM_OP, CONST_LONG(2)),
                                                 GT_OP,
                                                 BINARY_OP(CONST_LONG(2), SUM_OP, CONST_LONG(1)))
    parseOne("1 >= 0 || 3 > 2") shouldBe BINARY_OP(BINARY_OP(CONST_LONG(1), GE_OP, CONST_LONG(0)),
                                                   OR_OP,
                                                   BINARY_OP(CONST_LONG(3), GT_OP, CONST_LONG(2)))
  }

  property("bytestr expressions") {
    parseOne("false || sigVerify(base58'333', base58'222', base58'111')") shouldBe BINARY_OP(
      FALSE,
      OR_OP,
      FUNCTION_CALL(
        NAME.VALID("sigVerify"),
        List(
          CONST_BYTEVECTOR(ByteVector(ScorexBase58.decode("333").get)),
          CONST_BYTEVECTOR(ByteVector(ScorexBase58.decode("222").get)),
          CONST_BYTEVECTOR(ByteVector(ScorexBase58.decode("111").get))
        )
      )
    )
  }

  property("base58") {
//    parseOne("base58'bQbp'") shouldBe CONST_BYTEVECTOR(ByteVector("foo".getBytes))
//    parseOne("base58''") shouldBe CONST_BYTEVECTOR(ByteVector.empty)
    isParsed("base58' bQbp'\n") shouldBe false
  }

  property("string is consumed fully") {
    parseOne(""" "   fooo    bar" """) shouldBe CONST_STRING("   fooo    bar")
  }

  property("string literal with unicode chars") {
    val stringWithUnicodeChars = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD"

    parseOne(
      s"""
         |
         | "$stringWithUnicodeChars"
         |
       """.stripMargin
    ) shouldBe CONST_STRING(stringWithUnicodeChars)
  }

  property("reserved keywords are invalid variable names") {
    List("if", "then", "else", "true", "false", "let").foreach { keyword =>
      val script = s"""let $keyword = 1
                      |true""".stripMargin
      parseOne(script) shouldBe BLOCK(
        LET(NAME.INVALID(keyword, "keywords are restricted"), CONST_LONG(1)),
        TRUE
      )
    }

    List("if", "then", "else", "let").foreach { keyword =>
      val script = s"$keyword + 1"
      parseOne(script) shouldBe BINARY_OP(REF(NAME.INVALID(keyword, "keywords are restricted")), BinaryOperation.SUM_OP, CONST_LONG(1))
    }
  }

  property("multisig sample") {
    val script =
      """
        |
        |let A = base58'PK1PK1PK1PK1PK1'
        |let B = base58'PK2PK2PK2PK2PK2'
        |let C = base58'PK3PK3PK3PK3PK3'
        |
        |let W = tx.bodyBytes
        |let P = tx.PROOF
        |let V = sigVerify(W,P,A)
        |
        |let AC = if(V) then 1 else 0
        |let BC = if(sigVerify(tx.bodyBytes,tx.PROOF,B)) then 1 else 0
        |let CC = if(sigVerify(tx.bodyBytes,tx.PROOF,C)) then 1 else 0
        |
        | AC + BC+ CC >= 2
        |
      """.stripMargin
    parseOne(script) // gets parsed, but later will fail on type check!
  }

  property("function call") {
    parseOne("FOO(1,2)".stripMargin) shouldBe FUNCTION_CALL(NAME.VALID("FOO"), List(CONST_LONG(1), CONST_LONG(2)))
    parseOne("FOO(X)".stripMargin) shouldBe FUNCTION_CALL(NAME.VALID("FOO"), List(REF(NAME.VALID("X"))))
  }

  property("isDefined/extract") {
    parseOne("isDefined(X)") shouldBe FUNCTION_CALL(NAME.VALID("isDefined"), List(REF(NAME.VALID("X"))))
    parseOne("if(isDefined(X)) then extract(X) else Y") shouldBe IF(
      FUNCTION_CALL(NAME.VALID("isDefined"), List(REF(NAME.VALID("X")))),
      FUNCTION_CALL(NAME.VALID("extract"), List(REF(NAME.VALID("X")))),
      REF(NAME.VALID("Y"))
    )
  }

  property("getter") {
    isParsed("xxx   .yyy") shouldBe true
    isParsed("xxx.  yyy") shouldBe true

    parseOne("xxx.yyy") shouldBe GETTER(REF(NAME.VALID("xxx")), NAME.VALID("yyy"))
    parseOne(
      """
        |
        | xxx.yyy
        |
      """.stripMargin
    ) shouldBe GETTER(REF(NAME.VALID("xxx")), NAME.VALID("yyy"))

    parseOne("xxx(yyy).zzz") shouldBe GETTER(FUNCTION_CALL(NAME.VALID("xxx"), List(REF(NAME.VALID("yyy")))), NAME.VALID("zzz"))
    parseOne(
      """
        |
        | xxx(yyy).zzz
        |
      """.stripMargin
    ) shouldBe GETTER(FUNCTION_CALL(NAME.VALID("xxx"), List(REF(NAME.VALID("yyy")))), NAME.VALID("zzz"))

    parseOne("(xxx(yyy)).zzz") shouldBe GETTER(FUNCTION_CALL(NAME.VALID("xxx"), List(REF(NAME.VALID("yyy")))), NAME.VALID("zzz"))
    parseOne(
      """
        |
        | (xxx(yyy)).zzz
        |
      """.stripMargin
    ) shouldBe GETTER(FUNCTION_CALL(NAME.VALID("xxx"), List(REF(NAME.VALID("yyy")))), NAME.VALID("zzz"))

    parseOne("{xxx(yyy)}.zzz") shouldBe GETTER(FUNCTION_CALL(NAME.VALID("xxx"), List(REF(NAME.VALID("yyy")))), NAME.VALID("zzz"))
    parseOne(
      """
        |
        | {
        |   xxx(yyy)
        | }.zzz
        |
      """.stripMargin
    ) shouldBe GETTER(FUNCTION_CALL(NAME.VALID("xxx"), List(REF(NAME.VALID("yyy")))), NAME.VALID("zzz"))

    parseOne(
      """
        |
        | {
        |   let yyy = aaa(bbb)
        |   xxx(yyy)
        | }.zzz
        |
      """.stripMargin
    ) shouldBe GETTER(
      BLOCK(
        LET(NAME.VALID("yyy"), FUNCTION_CALL(NAME.VALID("aaa"), List(REF(NAME.VALID("bbb"))))),
        FUNCTION_CALL(NAME.VALID("xxx"), List(REF(NAME.VALID("yyy"))))
      ),
      NAME.VALID("zzz")
    )
  }

  property("crypto functions") {
    val hashFunctions = Vector("sha256", "blake2b256", "keccak256")
    val text          = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD=test message"
    val encodedText   = ScorexBase58.encode(text.getBytes)

    for (f <- hashFunctions) {
      parseOne(s"$f(base58'$encodedText')".stripMargin) shouldBe
        FUNCTION_CALL(
          NAME.VALID(f),
          List(CONST_BYTEVECTOR(ByteVector(text.getBytes)))
        )
    }
  }
}

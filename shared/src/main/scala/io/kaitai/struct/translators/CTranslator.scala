package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.datatype._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.format.SpecialIdentifier
import io.kaitai.struct.languages.CCompiler
import io.kaitai.struct.languages.components.CppImportList

class CTranslator(provider: TypeProvider, importList: CppImportList, isInternal: Boolean) extends BaseTranslator(provider) {
  def doArrayLiteralInternal(t: DataType, value: Seq[expr]): String = {
    val size = value.size
    val args = value.map(translate).mkString(", ")
    t match {
      case CalcIntType => s"ks_array_int64_t_from_data($size, $args)"
      case CalcFloatType => s"ks_array_double_from_data($size, $args)"
      case CalcStrType => s"ks_array_string_from_data($size, $args)"
      case KaitaiStructType | CalcKaitaiStructType => s"ks_array_usertype_generic_from_data($size, $args)"
      case _ => s"Missing list type: " + t.toString()
    }
  }
  override def doArrayLiteral(t: DataType, value: Seq[expr]): String = {
    t match {
      case arr: CalcArrayType => doArrayLiteralInternal(arr.elType, value)
      case _ => doArrayLiteralInternal(t, value)
    }
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    if (arr.size == 0) {
      s"ks_bytes_from_data(0)"
    } else {
      s"ks_bytes_from_data(${arr.size}, ${arr.map(_ & 0xff).mkString(", ")})"
    }
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String =
    s"new byte[] { ${elts.map(translate).mkString(", ")} }"

  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\",

    '\u0000' -> "\\0",
    '\u0007' -> "\\a",
    '\f' -> "\\f",
    '\u000b' -> "\\v",
    '\b' -> "\\b"
  )

  override def strLiteralUnicode(code: Char): String =
    code.toString().getBytes("UTF-8").map(c => s"\\x" + Integer.toHexString(c & 0xff)).mkString("")

  override def doIntLiteral(n: BigInt): String = {
    if (n == -9223372036854775808L) {
      return s"(${n + 1}LL - 1)"
    }
    val suffix = if (n < -9223372036854775808L) {
      "" // too low, no suffix would help anyway
    } else if (n <= -2147483649L) {
      "LL" // -9223372036854775808..–2147483649
    } else if (n <= 2147483647L) {
      "" // -2147483648..2147483647
    } else if (n <= 4294967295L) {
      "UL" // 2147483648..4294967295
    } else if (n <= 9223372036854775807L) {
      "LL" // 4294967296..9223372036854775807
    } else if (n <= Utils.MAX_UINT64) {
      "ULL" // 9223372036854775808..18446744073709551615
    } else {
      "" // too high, no suffix would help anyway
    }

    s"$n$suffix"
  }

  override def strLiteralGenericCC(code: Char): String = strLiteralUnicode(code)

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"ks_mod(${translate(left)}, ${translate(right)})"
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        s"ks_div(${translate(left)}, ${translate(right)})"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def doName(s: String) =
    if (s.startsWith("_")) {
      s match {
        case Identifier.SWITCH_ON => "on"
        case Identifier.INDEX => "i"
        case Identifier.IO => "stream"
        case Identifier.PARENT => "data->_parent"
        case Identifier.ROOT => "root_data"
        case _ => s"_temp$s"
      }
    } else {
      if (isInternal) {
         s"FIELD(data, $s)"
      } else {
         s"data->$s"
      }
    }

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String = {
    val enumClass = enumTypeAbs.drop(1).mkString("_")
    s"KSX_${enumClass}_$label".toUpperCase()
  }
  override def doEnumById(enumTypeAbs: List[String], id: String): String = id

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String =
    s"(ks_bytes_compare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)"

  override def arraySubscript(container: expr, idx: expr): String =
    s"${translate(container)}->data[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"
  override def doCast(value: Ast.expr, typeName: DataType): String = {
    val suffix = typeName match {
      case t: UserType => "*"
      case _ => ""
    }
    s"((${CCompiler.kaitaiType2NativeType(typeName)}$suffix) (${translate(value)}))"
  }

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String = {
    s"ks_string_to_int(${translate(s)}, ${translate(base)})"
  }
  override def enumToInt(v: expr, et: EnumType): String =
    translate(v)
  override def floatToInt(v: expr): String =
    s"(long) (${translate(v)})"
  override def intToStr(i: expr, base: expr): String = {
    s"ks_string_from_int(${translate(i)}, ${translate(base)})"
  }
  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String =
    s"ks_string_from_bytes($bytesExpr, ${translate(encoding)})"

  override def strLength(s: expr): String =
    s"${translate(s)}->len"

  override def strReverse(s: expr): String =
    s"ks_string_reverse(${translate(s)})"

  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"ks_string_substr(${translate(s)}, ${translate(from)}, ${translate(to)})"

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    s"(ks_string_compare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)"
  }

  override def doStringLiteral(s: String): String = {
    s"ks_string_from_cstr(${super.doStringLiteral(s)})"
  }

  override def bytesFirst(b: Ast.expr): String =
    s"ks_bytes_get_at(${translate(b)}, 0)"

  override def bytesLength(b: Ast.expr): String =
    s"${translate(b)}->length"

  override def bytesLast(b: Ast.expr): String = {
    val v = translate(b)
    s"ks_bytes_get_at($v, $v->length - 1)"
  }

  override def bytesSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"ks_bytes_get_at(${translate(container)}, ${translate(idx)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}->data[0]"

  override def arrayLast(a: expr): String = {
    val v = translate(a)
    s"$v->data[$v->size - 1]"
  }

  override def arraySize(a: expr): String =
    s"${translate(a)}->size"

  override def arrayMin(a: Ast.expr): String = {
    var res = translate(a)
    var typeArray = detectType(a)
   typeArray match {
      case t : ArrayType =>
        t.elType match {
          case _ : IntType => s"ks_array_min_int(&$res->_handle)"
          case _ : FloatType => s"ks_array_min_float(&$res->_handle)"
          case _ : StrType => s"ks_array_min_string(&$res->_handle)"
          case _ : BytesType => s"ks_array_min_bytes(&$res->_handle)"
          case _ => "UNKNOWN_Min: " + t.toString()
        }
      case _ : BytesType => s"ks_bytes_min($res)"
      case _ => "UNKNOWN_Min: " + typeArray.toString()
    }
  }
  override def arrayMax(a: Ast.expr): String = {
    var res = translate(a)
    var typeArray = detectType(a)
    typeArray match {
      case t : ArrayType =>
        t.elType match {
          case _ : IntType => s"ks_array_max_int(&$res->_handle)"
          case _ : FloatType => s"ks_array_max_float(&$res->_handle)"
          case _ : StrType => s"ks_array_max_string(&$res->_handle)"
          case _ : BytesType => s"ks_array_max_bytes(&$res->_handle)"
          case _ => "UNKNOWN_Max: " + t.toString()
        }
      case _ : BytesType => s"ks_bytes_max($res)"
      case _ => "UNKNOWN_Max: " + typeArray.toString()
    }
  }
  override def anyField(value: expr, attrName: String): String = {
    if (attrName == "_io")
    {
      return s"${translate(value)}->_handle.stream"
    }
    if (attrName == "_parent" || !isInternal) {
      return s"${translate(value)}->$attrName"
    }
    val dataType = detectType(value)
    dataType match {
      case t: UserType =>
        if (t.isOpaque) {
          s"${translate(value)}->$attrName"
        } else {
          s"FIELD(${translate(value)}, $attrName)"
        }
      case _ => s"FIELD(${translate(value)}, $attrName)"
    }
  }
  override def strConcat(left: Ast.expr, right: Ast.expr): String = s"ks_string_concat(${translate(left)}, ${translate(right)})"
  override def doBoolLiteral(n: Boolean): String = if (n) "1" else "0"
  override def kaitaiStreamEof(value: Ast.expr): String =
    s"ks_stream_is_eof(${translate(value)})"
  override def kaitaiStreamSize(value: Ast.expr): String =
    s"ks_stream_get_length(${translate(value)})"
  override def kaitaiStreamPos(value: Ast.expr): String =
    s"ks_stream_get_pos(${translate(value)})"
}

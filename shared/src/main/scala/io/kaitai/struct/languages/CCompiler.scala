package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{CTranslator, TypeDetector}

class CCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with UpperCamelCaseClasses
    with ObjectOrientedLanguage
    with SingleOutputFile
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with UniversalDoc
    with FixedContentsUsingArrayByteLiteral
    with SwitchIfOps
    with NoNeedForFullClassPath {
  import CCompiler._

  val importListSrc = new CppImportList
  val importListHdr = new CppImportList

  val translator = new CTranslator(typeProvider, importListSrc)

  var outMethodHead = new StringLanguageOutputWriter(indent)
  var outMethodBody = new StringLanguageOutputWriter(indent)
  val outSrcHeader = new StringLanguageOutputWriter(indent)
  val outHdrHeader = new StringLanguageOutputWriter(indent)
  val outSrcDefs = new StringLanguageOutputWriter(indent)
  val outSrc = new StringLanguageOutputWriter(indent)
  val outHdr = new StringLanguageOutputWriter(indent)
  val outHdrRoot = new StringLanguageOutputWriter(indent)
  val outHdrDefs = new StringLanguageOutputWriter(indent)

  override def results(topClass: ClassSpec): Map[String, String] = {
    val className = topClass.nameAsStr
    Map(
      outFileNameSource(className) -> (outSrcHeader.result + importListSrc.result + outSrcDefs.result + outSrc.result),
      outFileNameHeader(className) -> (outHdrHeader.result + importListHdr.result + outHdrDefs.result + outHdrRoot.result + outHdr.result)
    )
  }

  override def outFileName(topClassName: String): String = topClassName.toLowerCase()
  def outFileNameSource(className: String): String = outFileName(className) + ".c"
  def outFileNameHeader(className: String): String = outFileName(className) + ".h"

  override def indent: String = "    "
  var rootClassCounter: Int = 0

  override def fileHeader(topClassName: String): Unit = {
    outSrcHeader.puts(s"// $headerComment")
    outSrcHeader.puts

    outHdrHeader.puts(s"// $headerComment")
    outHdrHeader.puts
    outHdrHeader.puts("#define KS_DEPEND_ON_INTERNALS")

    importListSrc.addLocal(outFileNameHeader(topClassName))

    importListHdr.addKaitai("kaitaistruct.h")
  }

  override def fileFooter(topClassName: String): Unit = {}

  override def classHeader(name: String): Unit = {
	rootClassCounter += 1
    if (rootClassCounter == 1) {
		outSrcDefs.puts
		outHdrRoot.puts
		outHdrRoot.puts(s"int ksx_read_${name}_from_stream(ks_stream* stream, struct ksx_${name}_* data);")
		outSrc.puts
		outSrc.puts(s"int ksx_read_${name}_from_stream(ks_stream* stream, ksx_$name* data)")
		outSrc.puts("{")
		outSrc.inc
		outSrc.puts(s"return ksx_read_$name(stream, data, stream, data);")
		outSrc.dec
		outSrc.puts("}")
		outHdrRoot.puts
		outHdrRoot.puts(s"typedef struct ksx_${name}_")
		outHdrRoot.puts("{")
		outHdrRoot.inc
        outHdrRoot.puts("ks_handle* _handle;")
		outHdrDefs.puts
		outHdrDefs.puts(s"struct ksx_${name}_;")
	} else {
		outHdr.puts
		outHdr.puts(s"typedef struct ksx_${name}_")
		outHdr.puts("{")
		outHdr.inc
		outHdr.puts("ks_handle* _handle;")
		outHdrDefs.puts(s"struct ksx_${name}_;")
	}
  }

  override def classFooter(name: String): Unit = {
	if (rootClassCounter == 1) {
	  outHdrRoot.dec
	  outHdrRoot.puts(s"} ksx_$name;")
	} else {
	  outHdr.dec
	  outHdr.puts(s"} ksx_$name;")
	}
	rootClassCounter -= 1
  }

  override def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
	outSrcDefs.puts(s"static int ksx_read_${name}(ks_stream* root_stream, ksx_$rootClassName* root_data, ks_stream* stream, ksx_$name* data);");
	outSrc.puts
    outSrc.puts(s"static int ksx_read_${name}(ks_stream* root_stream, ksx_$rootClassName* root_data, ks_stream* stream, ksx_$name* data)")
  }

  override def classConstructorFooter: Unit = {}

  override def runRead(name: List[String]): Unit = {}

  override def runReadCalc(): Unit = {
    out.puts
    out.puts(s"if (${privateMemberName(EndianIdentifier)} == null) {")
    out.inc
    out.puts(s"throw new ${ksErrorName(UndecidedEndiannessError)}();")
    importList.add("System")
    out.dec
    out.puts(s"} else if (${privateMemberName(EndianIdentifier)} == true) {")
    out.inc
    out.puts("_readLE();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts("_readBE();")
    out.dec
    out.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
	outSrc.puts("{")
    outMethodHead.inc
    outMethodBody.inc
  }

  override def readFooter(): Unit = {
    outSrc.add(outMethodHead)
    if (outMethodHead.result != "") {
        outSrc.puts
    }
    outSrc.add(outMethodBody)
    outSrc.puts("}")
    outMethodHead = new StringLanguageOutputWriter(indent)
    outMethodBody = new StringLanguageOutputWriter(indent)
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    if (idToStr(attrName) == "_parent" || idToStr(attrName) == "_root")
    {
      return
    }
    val prefix = attrType match {
      case t: UserType => "struct "
      case _ => ""
    }
    val suffix = attrType match {
      case t: UserType => "_*"
      case _ => ""
    }
    val isSubtypeByte = attrName match {
        case RawIdentifier(_) => true
        case _ => false
    }
    if (isSubtypeByte) {
        return
    }
	if (rootClassCounter == 1) {
		outHdrRoot.puts(s"$prefix${kaitaiType2NativeType(attrType)}$suffix ${privateMemberName(attrName)};")
	} else {
		outHdr.puts(s"$prefix${kaitaiType2NativeType(attrType)}$suffix ${privateMemberName(attrName)};")
	}
    
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def universalDoc(doc: DocSpec): Unit = {}

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts(s"if (${privateMemberName(EndianIdentifier)} == true) {")
    out.inc
    leProc()
    out.dec
    out.puts("} else {")
    out.inc
    beProc()
    out.dec
    out.puts("}")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    out.puts(s"${privateMemberName(attrName)} = $normalIO.EnsureFixedContents($contents);")

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        s"$normalIO.ProcessXor($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$normalIO.ProcessZlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$normalIO.ProcessRotateLeft($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val procClass = types2class(name)
        val procName = s"_process_${idToStr(varSrc)}"
        out.puts(s"$procClass $procName = new $procClass(${args.map(expression).mkString(", ")});")
        s"$procName.Decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val privateVarName = privateMemberName(varName)

    val ioName = s"io_$privateVarName"

    val args = rep match {
      case RepeatUntil(_) => translator.doName(Identifier.ITERATOR2)
      case _ => getRawIdExpr(varName, rep)
    }

    out.puts(s"var $ioName = new $kstreamName($args);")
    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case _ => s"$memberName[$memberName.Count - 1]"
    }
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"$kstreamName io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"long _pos = $io.Pos;")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.Seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io.Seek(_pos);")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.AlignToByte();")

  override def instanceClear(instName: InstanceIdentifier): Unit = {
    out.puts(s"${flagForInstName(instName)} = false;")
  }

  override def instanceSetCalculated(instName: InstanceIdentifier): Unit = {
    out.puts(s"${flagForInstName(instName)} = true;")
  }

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condIfFooter(expr: expr): Unit = fileFooter(null)

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw): Unit = {
    importList.add("System.Collections.Generic")

    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new List<byte[]>();")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = new List<byte[]>();")

    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2NativeType(ArrayTypeInStream(dataType))}();")
    out.puts("{")
    out.inc
    out.puts("var i = 0;")
    out.puts(s"while (!$io.IsEof) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.Add($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.puts("i++;")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: expr): Unit = {
    importList.add("System.Collections.Generic")

    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new List<byte[]>((int) (${expression(repeatExpr)}));")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = new List<byte[]>((int) (${expression(repeatExpr)}));")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2NativeType(ArrayTypeInStream(dataType))}((int) (${expression(repeatExpr)}));")
    out.puts(s"for (var i = 0; i < ${expression(repeatExpr)}; i++)")
    out.puts("{")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.Add($expr);")
  }

  override def condRepeatExprFooter: Unit = fileFooter(null)

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, untilExpr: expr): Unit = {
    importList.add("System.Collections.Generic")

    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new List<byte[]>();")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = new List<byte[]>();")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2NativeType(ArrayTypeInStream(dataType))}();")
    out.puts("{")
    out.inc
    out.puts("var i = 0;")
    out.puts(s"${kaitaiType2NativeType(dataType)} ${translator.doName("_")};")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val (typeDecl, tempVar) = if (isRaw) {
      ("byte[] ", translator.doName(Identifier.ITERATOR2))
    } else {
      ("", translator.doName(Identifier.ITERATOR))
    }
    out.puts(s"$typeDecl$tempVar = $expr;")
    out.puts(s"${privateMemberName(id)}.Add($tempVar);")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts("i++;")
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    val name = privateMemberName(id)
    val expr2 = expr.replaceAll("_NAME_", name)
    id match {
        case RawIdentifier(_) => {
            outMethodHead.puts(s"ks_bytes* _raw_$name;")
            outMethodHead.puts(s"ks_stream _io_$name;")
            outMethodBody.puts(s"/* Subtype */")
            outMethodBody.puts(s"${expr2}_raw_$name);")
        }
        case _ => outMethodBody.puts(s"${expr2}data->$name);")
    }
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"${kaitaiType2NativeType(dataType)} $id = $expr;")

  override def blockScopeHeader: Unit = {
    out.puts("{")
    out.inc
  }
  override def blockScopeFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"ks_stream_read_${t.apiCall(defEndian)}(stream, &"
      case blt: BytesLimitType =>
        s"ks_stream_read_bytes(stream, ${expression(blt.size)}, &"
      case _: BytesEosType =>
        s"$io.ReadBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.ReadBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1(bitEndian) =>
        s"ks_stream_read_bits_${bitEndian.toSuffix.toLowerCase()}(stream, 1, &"
      case BitsType(width: Int, bitEndian) =>
        s"ks_stream_read_bits_${bitEndian.toSuffix.toLowerCase()}(stream, $width, &"
      case t: UserType =>
        val name = t.name.mkString(".").toLowerCase()
        s"ks_stream_create_from_bytes(&_io__NAME_, _raw__NAME_);\n" +
            s"    data->_NAME_ = malloc(sizeof(ksx_$name));\n" +
            s"    ks_bytes_destroy(_raw__NAME_);\n" +
            s"    ksx_read_$name(root_stream, root_data, &_io__NAME_, "
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.BytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.BytesTerminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    val expr = if (assignType != dataType) {
      s"((${kaitaiType2NativeType(dataType)}) ($id))"
    } else {
      id
    }
    out.puts(s"$expr._read();")
  }

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType | _: EnumType | _: StrType => false
    case _ => true
  }

  //<editor-fold desc="switching: true version">

  val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"switch (${expression(on)}) {")

  override def switchCaseFirstStart(condition: Ast.expr): Unit = switchCaseStart(condition)

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"case ${expression(condition)}: {")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    out.puts("break;")
    out.dec
    out.puts("}")
  }

  override def switchElseStart(): Unit = {
    out.puts("default: {")
    out.inc
  }

  override def switchEnd(): Unit =
    out.puts("}")

  //</editor-fold>

  //<editor-fold desc="switching: emulation with ifs">

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"${kaitaiType2NativeType(onType)} ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
  }

  def switchCmpExpr(condition: Ast.expr): String =
    expression(
      Ast.expr.Compare(
        NAME_SWITCH_ON,
        Ast.cmpop.Eq,
        condition
      )
    )

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if (${switchCmpExpr(condition)})")
    out.puts("{")
    out.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"else if (${switchCmpExpr(condition)})")
    out.puts("{")
    out.inc
  }

  override def switchIfCaseEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  override def switchIfElseStart(): Unit = {
    out.puts("else")
    out.puts("{")
    out.inc
  }

  override def switchIfEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  //</editor-fold>

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"private bool ${flagForInstName(attrName)};")
    out.puts(s"private ${kaitaiType2NativeType(attrType)} ${privateMemberName(attrName)};")
  }

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"public ${kaitaiType2NativeType(dataType)} ${publicMemberName(instName)}")
    out.puts("{")
    out.inc
    out.puts("get")
    out.puts("{")
    out.inc
  }

  override def instanceFooter: Unit = {
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if (${flagForInstName(instName)})")
    out.inc
    instanceReturn(instName, dataType)
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: expr): Unit =
    // Perform explicit cast as unsigned integers can't be directly assigned to the default int type
    handleAssignmentSimple(instName, s"(${kaitaiType2NativeType(dataType)}) (${expression(value)})")

  def flagForInstName(ksName: Identifier) = s"f_${idToStr(ksName)}"

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    val enumClass = "KSX_" + type2class(enumName).toUpperCase()

    outHdrDefs.puts
    outHdrDefs.puts(s"typedef enum ${enumClass}_")
    outHdrDefs.puts(s"{")
    outHdrDefs.inc

    enumColl.foreach { case (id, label) =>
      outHdrDefs.puts(s"${enumClass}_${label.toUpperCase()} = $id,")
    }

    outHdrDefs.dec
    outHdrDefs.puts(s"} $enumClass;")
  }

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => name.toLowerCase()
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => name.toLowerCase()
      case RawIdentifier(innerId) => idToStr(innerId)
    }
  }

  override def publicMemberName(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => s"M${Utils.upperCamelCase(name)}"
      case NamedIdentifier(name) => Utils.upperCamelCase(name)
      case NumberedIdentifier(idx) => s"${NumberedIdentifier.TEMPLATE.capitalize}_$idx"
      case InstanceIdentifier(name) => Utils.upperCamelCase(name)
      case RawIdentifier(innerId) => s"M_Raw${publicMemberName(innerId)}"
    }
  }

  override def privateMemberName(id: Identifier): String = s"${idToStr(id)}"

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def paramName(id: Identifier): String = s"p_${idToStr(id)}"

  override def ksErrorName(err: KSError): String = CCompiler.ksErrorName(err)

  override def attrValidateExpr(
    attrId: Identifier,
    attrType: DataType,
    checkExpr: Ast.expr,
    err: KSError,
    errArgs: List[Ast.expr]
  ): Unit = {
    val errArgsStr = errArgs.map(translator.translate).mkString(", ")
    out.puts(s"if (!(${translator.translate(checkExpr)}))")
    out.puts("{")
    out.inc
    out.puts(s"throw new ${ksErrorName(err)}($errArgsStr);")
    out.dec
    out.puts("}")
  }
}

object CCompiler extends LanguageCompilerStatic
  with StreamStructNames
  with UpperCamelCaseClasses
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new CCompiler(tp, config)


  def kaitaiType2NativeType(attrType: DataType): String = {
    attrType match {
      case Int1Type(false) => "uint8_t"
      case IntMultiType(false, Width2, _) => "uint16_t"
      case IntMultiType(false, Width4, _) => "uint32_t"
      case IntMultiType(false, Width8, _) => "uint64_t"

      case Int1Type(true) => "int8_t"
      case IntMultiType(true, Width2, _) => "int16_t"
      case IntMultiType(true, Width4, _) => "int32_t"
      case IntMultiType(true, Width8, _) => "int64_t"

      case FloatMultiType(Width4, _) => "float"
      case FloatMultiType(Width8, _) => "double"

      case BitsType(_, _) => "uint64_t"

      case CalcIntType => "int"
      case CalcFloatType => "double"
      case _: BooleanType => "bool"

      case _: StrType => "char*"
      case _: BytesType => "ks_bytes*"

      case AnyType => "void*"
      case KaitaiStructType | CalcKaitaiStructType => kstructName
      case KaitaiStreamType | OwnedKaitaiStreamType => kstreamName

      case t: UserType => "ksx_" + t.name.mkString(".").toLowerCase()
      case EnumType(name, _) => "KSX_" + name.mkString(".").toUpperCase()

      case at: ArrayType => s"List<${kaitaiType2NativeType(at.elType)}>"

      case st: SwitchType => kaitaiType2NativeType(st.combinedType)
    }
  }

  def types2class(typeName: Ast.typeId): String =
    types2class(typeName.names)
  def types2class(names: Iterable[String]) = names.map(type2class).mkString(".").toLowerCase()

  override def kstructName = "KaitaiStruct"
  override def kstreamName = "KaitaiStream"
  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => "EndOfStreamException"
    case _ => err.name
  }
}

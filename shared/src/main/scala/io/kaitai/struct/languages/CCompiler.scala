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
  val outHdrEnums = new StringLanguageOutputWriter(indent)
  val outHdrArrays = new StringLanguageOutputWriter(indent)
  val outHdrDefs = new StringLanguageOutputWriter(indent)
  var outMethodBodyInstance = new StringLanguageOutputWriter(indent)

  def printdbg(s: String) : Unit = outMethodBodyInstance.puts("//" + s)

  override def results(topClass: ClassSpec): Map[String, String] = {
    val className = topClass.nameAsStr
    Map(
      outFileNameSource(className) -> (outSrcHeader.result + importListSrc.result + outSrcDefs.result + outSrc.result),
      outFileNameHeader(className) -> (outHdrHeader.result + importListHdr.result + outHdrDefs.result + outHdrEnums.result + outHdrRoot.result + outHdr.result + outHdrArrays.result)
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

    importListSrc.addLocal(outFileNameHeader(topClassName))

    importListHdr.addKaitai("kaitaistruct.h")

    outHdrDefs.puts
    outHdrDefs.puts("/* Forward declarations */")

    outHdrArrays.puts
    outHdrArrays.puts("/* Array structures */")

    outHdrEnums.puts
    outHdrEnums.puts("/* Enums */")

    outHdrRoot.puts
    outHdrRoot.puts("/* Main structures */")

    outMethodBodyInstance.inc
  }

  override def fileFooter(topClassName: String): Unit = {
    outMethodBody.dec
    outMethodBody.puts("}")
  }

  override def classHeader(name: String): Unit = {
    rootClassCounter += 1
    if (rootClassCounter == 1) {
      outSrcDefs.puts
      outHdrRoot.puts
      outHdrRoot.puts(s"int ksx_read_${name}_from_stream(ks_stream* stream, ksx_${name}* data);")
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
      outHdrRoot.puts("ks_handle _handle;")
      outHdrDefs.puts
      outHdrDefs.puts(s"typedef struct ksx_${name}_ ksx_${name};")
    } else {
      outHdrArrays.puts
      outHdrArrays.puts(s"typedef struct ksx_array_${name}_")
      outHdrArrays.puts("{")
      outHdrArrays.inc
      outHdrArrays.puts("ks_handle _handle;")
      outHdrArrays.puts("int64_t size;")
      outHdrArrays.puts(s"ksx_$name* data;")
      outHdrArrays.dec
      outHdrArrays.puts(s"} ksx_array_$name;")
      outHdr.puts
      outHdr.puts(s"typedef struct ksx_${name}_")
      outHdr.puts("{")
      outHdr.inc
      outHdr.puts("ks_handle _handle;")
      outHdrDefs.puts(s"typedef struct ksx_array_${name}_ ksx_array_${name};")
      outHdrDefs.puts(s"typedef struct ksx_${name}_ ksx_${name};")
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
    outMethodBody.puts(s"    CHECK(ks_handle_init(&data->_handle, stream, data, KS_TYPE_USERTYPE, sizeof(ksx_$name)));")
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
    outSrc.add(outMethodBodyInstance)
    outSrc.puts("}")
    outMethodHead = new StringLanguageOutputWriter(indent)
    outMethodBody = new StringLanguageOutputWriter(indent)
    outMethodBodyInstance = new StringLanguageOutputWriter(indent)
    outMethodBodyInstance.inc
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    if (idToStr(attrName) == "_parent" || idToStr(attrName) == "_root")
    {
      return
    }
    val suffix = attrType match {
      case t: UserType => "*"
      case at: ArrayType => "*"
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
		outHdrRoot.puts(s"${kaitaiType2NativeType(attrType)}$suffix ${privateMemberName(attrName)};")
	} else {
		outHdr.puts(s"${kaitaiType2NativeType(attrType)}$suffix ${privateMemberName(attrName)};")
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

  override def instanceClear(instName: InstanceIdentifier): Unit = {}

  override def instanceSetCalculated(instName: InstanceIdentifier): Unit = {}

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
    val pos = "_pos_" + privateMemberName(id)
    val len = expression(repeatExpr)
    val name = privateMemberName(RawIdentifier(id))
    val dataTypeArray = ArrayTypeInStream(dataType)
    val arrayTypeSize = getKaitaiTypeEnumAndSize(dataType)
    outMethodBody.puts("/* Array (repeat-expr) */")
    outMethodHead.puts(s"int64_t $pos;")

    outMethodBody.puts(s"data->$name = calloc(1, sizeof(${kaitaiType2NativeType(dataTypeArray)}));")
    outMethodBody.puts(s"data->$name->size = $len;")
    outMethodBody.puts(s"data->$name->data = calloc(sizeof(${kaitaiType2NativeType(dataType)}), data->$name->size);")
    outMethodBody.puts(s"CHECK(ks_handle_init(&data->$name->_handle, stream, data->$name, $arrayTypeSize));");
    outMethodBody.puts(s"for ($pos = 0; $pos < data->$name->size; $pos++)")
    outMethodBody.puts("{")
    outMethodBody.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    handleAssignmentC(id, dataTypeLast, assignTypeLast, ioLast, defEndianLast, true)
  }

  override def condRepeatExprFooter: Unit = fileFooter(null)

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, untilExpr: expr): Unit = {
    val pos = "_pos_" + privateMemberName(id)
    val name = privateMemberName(RawIdentifier(id))
    val dataTypeArray = ArrayTypeInStream(dataType)
    val arrayTypeSize = getKaitaiTypeEnumAndSize(dataType)
    outMethodBody.puts("/* Array (repeat-until) */")
    outMethodHead.puts(s"int64_t $pos;")

    outMethodBody.puts(s"data->$name = calloc(1, sizeof(${kaitaiType2NativeType(dataTypeArray)}));")
    outMethodBody.puts(s"data->$name->size = 0;")
    outMethodBody.puts(s"data->$name->data = 0;")
    outMethodBody.puts(s"CHECK(ks_handle_init(&data->$name->_handle, stream, data->$name, $arrayTypeSize));");
    outMethodBody.puts("{")
    outMethodBody.inc
    outMethodBody.puts(s"${kaitaiType2NativeType(dataType)} ${translator.doName("_")} = {0};")
    outMethodBody.puts(s"do")
    outMethodBody.puts("{")
    outMethodBody.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val name = privateMemberName(RawIdentifier(id))
    val nameTemp = translator.doName(Identifier.ITERATOR)
    val sizeof = s"sizeof(${kaitaiType2NativeType(dataTypeLast)})"
    outMethodBody.puts(s"data->$name->size++;")
    outMethodBody.puts(s"data->$name->data = realloc(data->$name->data, $sizeof * data->$name->size);")
    outMethodBody.puts(s"memset(data->$name->data + data->$name->size - 1, 0, $sizeof);")
    handleAssignmentC(id, dataTypeLast, assignTypeLast, ioLast, defEndianLast, true)
    outMethodBody.puts(s"$nameTemp = data->$name->data[_pos_$name];");
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, untilExpr: expr): Unit = {
    val pos = "_pos_" + privateMemberName(id)
    typeProvider._currentIteratorType = Some(dataType)
    outMethodBody.puts(s"$pos++;")
    outMethodBody.dec
    outMethodBody.puts(s"} while (!(${expression(untilExpr)}));")
    outMethodBody.dec
    outMethodBody.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    handleAssignmentC(id, dataTypeLast, assignTypeLast, ioLast, defEndianLast, false)
  }

  def handleAssignmentC(id: Identifier, dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian], isArray: Boolean)
  {
    // TODO: Use io!
    val name = privateMemberName(id)
    var nameTarget = name
    if (isArray) {
        nameTarget = s"$name->data[_pos_$name]"
    }
    if (lastWasInstanceValue) {
        lastWasInstanceValue = false
        return
    }
    // outMethodBody.puts("//" + dataType.toString() + " __ " + assignType.toString())
    val targetType = kaitaiType2NativeType(dataType)

    dataType match {
      case t: ReadableType =>
        outMethodHead.puts(s"$targetType $name;")
        outMethodBody.puts(s"CHECK(ks_stream_read_${t.apiCall(defEndian)}(stream, &$name));")
        outMethodBody.puts(s"data->$nameTarget = $name;")
      case blt: BytesLimitType =>
        id match {
          case RawIdentifier(_) =>
            outMethodHead.puts(s"ks_bytes _raw_$name;")
            outMethodHead.puts(s"ks_stream _io_$name;")
            outMethodBody.puts(s"/* Subtype with substream */")
            outMethodBody.puts(s"CHECK(ks_stream_read_bytes(stream, ${expression(blt.size)}, &_raw_$name));")
          case _ =>
            outMethodBody.puts(s"CHECK(ks_stream_read_bytes(stream, ${expression(blt.size)}, &data->$name));")
        }
      case _: BytesEosType =>
        // s"$io.ReadBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        // s"$io.ReadBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1(bitEndian) =>
        outMethodHead.puts(s"$targetType $name;")
        outMethodBody.puts(s"CHECK(ks_stream_read_bits_${bitEndian.toSuffix.toLowerCase()}(stream, 1, &$name));")
        outMethodBody.puts(s"data->$nameTarget = $name;")
      case BitsType(width: Int, bitEndian) =>
        outMethodHead.puts(s"$targetType $name;")
        outMethodBody.puts(s"CHECK(ks_stream_read_bits_${bitEndian.toSuffix.toLowerCase()}(stream, $width, &$name));")
        outMethodBody.puts(s"data->$nameTarget = $name;")
      case t: UserTypeFromBytes =>
        val typeName = makeName(t.name)
        outMethodBody.puts(s"CHECK(ks_stream_init_from_bytes(&_io_$name, &_raw_$name));")
        if (isArray) {
          outMethodBody.puts(s"CHECK(ksx_read_$typeName(root_stream, root_data, &_io_$name, &data->$nameTarget));")
        } else {
          outMethodBody.puts(s"data->$nameTarget = calloc(1, sizeof(ksx_$typeName));")
          outMethodBody.puts(s"CHECK(ksx_read_$typeName(root_stream, root_data, &_io_$name, data->$nameTarget));")
        }
      case t: UserTypeInstream =>
        val typeName = makeName(t.name)
        outMethodBody.puts(s"/* Subtype */")
        if (isArray) {
            outMethodBody.puts(s"CHECK(ksx_read_$typeName(root_stream, root_data, stream, &data->$nameTarget));")
        } else {
            outMethodBody.puts(s"data->$nameTarget = calloc(1, sizeof(ksx_$typeName));")
            outMethodBody.puts(s"CHECK(ksx_read_$typeName(root_stream, root_data, stream, data->$nameTarget));")
        }
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

  var dataTypeLast: DataType = null;
  var assignTypeLast: DataType = null;
  var ioLast: String = "";
  var defEndianLast: Option[FixedEndian] = null
  var lastWasInstanceValue : Boolean = false

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataTypeLast = dataType;
    assignTypeLast = assignType;
    ioLast = io;
    defEndianLast = defEndian;
    ""
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
    attributeDeclaration(attrName, attrType, isNullable)
  }

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {}

  override def instanceFooter: Unit = {}

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {}

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {}

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: expr): Unit = {
    lastWasInstanceValue = true
    val name = privateMemberName(instName)
    val expr = expression(value)
    outMethodBodyInstance.puts(s"data->$name = $expr;")
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    val enumClass = type2class(enumName).toLowerCase()
    val enumClass2 = "ksx_" + enumClass

    outHdrEnums.puts
    outHdrEnums.puts(s"typedef enum ${enumClass2}_")
    outHdrEnums.puts(s"{")
    outHdrEnums.inc

    enumColl.foreach { case (id, label) =>
      outHdrEnums.puts(s"${enumClass2.toUpperCase()}_${label.toUpperCase()} = $id,")
    }

    outHdrEnums.dec
    outHdrEnums.puts(s"} $enumClass2;")

    outHdrArrays.puts
    outHdrArrays.puts(s"typedef struct ksx_array_${enumClass}_")
    outHdrArrays.puts("{")
    outHdrArrays.inc
    outHdrArrays.puts("ks_handle* _handle;")
    outHdrArrays.puts("int64_t size;")
    outHdrArrays.puts(s"ksx_$enumClass* data;")
    outHdrArrays.dec
    outHdrArrays.puts(s"} ksx_array_$enumClass;")
    outHdrDefs.puts(s"typedef struct ksx_array_${enumClass}_ ksx_array_$enumClass;")
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

      case _: StrType => "ks_string"
      case _: BytesType => "ks_bytes"

      case AnyType => "void*"
      case KaitaiStructType | CalcKaitaiStructType => kstructName
      case KaitaiStreamType | OwnedKaitaiStreamType => kstreamName

      case t: UserType => "ksx_" + makeName(t.name)
      case EnumType(name, _) => "ksx_" + makeName(name)

      case at: ArrayType => {
        at.elType match {
          case t: UserType => s"ksx_array_${makeName(t.name)}"
          case EnumType(name, _) => s"ksx_array_${makeName(name)}"
          case _ => s"ks_array_${kaitaiType2NativeType(at.elType)}"
        }
      }

      case st: SwitchType => kaitaiType2NativeType(st.combinedType)
    }
  }

  def getKaitaiTypeEnumAndSize(attrType: DataType): String = {
    attrType match {
      case Int1Type(false) => "KS_TYPE_ARRAY_INT, 1"
      case IntMultiType(false, Width2, _) => "KS_TYPE_ARRAY_INT, 2"
      case IntMultiType(false, Width4, _) => "KS_TYPE_ARRAY_INT, 4"
      case IntMultiType(false, Width8, _) => "KS_TYPE_ARRAY_INT, 8"

      case Int1Type(true) => "KS_TYPE_ARRAY_INT, 1"
      case IntMultiType(true, Width2, _) => "KS_TYPE_ARRAY_INT, 2"
      case IntMultiType(true, Width4, _) => "KS_TYPE_ARRAY_INT, 4"
      case IntMultiType(true, Width8, _) => "KS_TYPE_ARRAY_INT, 8"

      case FloatMultiType(Width4, _) => "KS_TYPE_ARRAY_FLOAT, 4"
      case FloatMultiType(Width8, _) => "KS_TYPE_ARRAY_FLOAT, 8"

      case _: StrType => "KS_TYPE_ARRAY_STRING, 0"

      case BitsType(_, _) => "KS_TYPE_ARRAY_INT, 8"

      case t: UserType => s"KS_TYPE_ARRAY_USERTYPE, sizeof(ksx_${makeName(t.name)})"
      case EnumType(name, _) => s"KS_TYPE_ARRAY_INT, sizeof(ksx_${makeName(name)})"

      case _ => "KS_TYPE_UNKNOWN, 0"
    }
  }

  def types2class(typeName: Ast.typeId): String =
    types2class(typeName.names)
  def types2class(names: Iterable[String]) = names.map(type2class).mkString(".").toLowerCase()

  def makeName(names: Iterable[String]) = names.mkString(".").toLowerCase()

  override def kstructName = "KaitaiStruct"
  override def kstreamName = "KaitaiStream"
  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => "EndOfStreamException"
    case _ => err.name
  }
}

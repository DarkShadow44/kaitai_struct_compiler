package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{CTranslator, TypeDetector}
import scala.collection.mutable.ListBuffer

class CCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with SingleOutputFile
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with UniversalDoc
    with FixedContentsUsingArrayByteLiteral
    with SwitchIfOps {
  import CCompiler._

  val importListSrc = new CppImportList
  val importListHdr = new CppImportList

  val translator = new CTranslator(typeProvider, importListSrc)

  var outMethodHead = new StringLanguageOutputWriter(indent)
  var outMethodBody = new StringLanguageOutputWriter(indent)
  val outSrcHeader = new StringLanguageOutputWriter(indent)
  val outHdrHeader = new StringLanguageOutputWriter(indent)
  val outSrcDefs = new StringLanguageOutputWriter(indent)
  val outSrcMain = new StringLanguageOutputWriter(indent)
  val outSrcInstances = new StringLanguageOutputWriter(indent)
  val outSrcInstancesFill = new StringLanguageOutputWriter(indent)
  val outSrcInstancesFillArray : ListBuffer[StringLanguageOutputWriter] = ListBuffer()
  val outHdr = new StringLanguageOutputWriter(indent)
  val outHdrEnums = new StringLanguageOutputWriter(indent)
  val outHdrArrays = new StringLanguageOutputWriter(indent)
  val outHdrInternalStructs : ListBuffer[StringLanguageOutputWriter] = ListBuffer()
  val outHdrDefs = new StringLanguageOutputWriter(indent)
  val outHdrFinish = new StringLanguageOutputWriter(indent)
  val outSrcInstancesGet = new StringLanguageOutputWriter(indent)
  val outSrcInternalStruct =  new StringLanguageOutputWriter(indent)
  val outHdrStructs : ListBuffer[StringLanguageOutputWriter] = ListBuffer()
  val importedTypes : ListBuffer[String] = ListBuffer()
  var outMethodHasI = false

  def printdbg(s: String) : Unit = outMethodBody.puts("//" + s)

  override def results(topClass: ClassSpec): Map[String, String] = {
    val className = topClass.nameAsStr
    Map(
      outFileNameSource(className) -> (outSrcHeader.result + importListSrc.result + outSrcDefs.result + outSrcInternalStruct.result + outSrcMain.result + outSrcInstances.result + outSrcInstancesGet.result + outSrcInstancesFill.result),
      outFileNameHeader(className) -> (outHdrHeader.result + importListHdr.result + outHdrDefs.result + outHdrEnums.result + outHdr.result + outHdrArrays.result + outHdrFinish.result)
    )
  }

  override def outFileName(topClassName: String): String = topClassName.toLowerCase()
  def outFileNameSource(className: String): String = outFileName(className) + ".c"
  def outFileNameHeader(className: String): String = outFileName(className) + ".h"

  override def indent: String = "    "

  override def fileHeader(topClassName: String): Unit = {
    outSrcHeader.puts(s"/* $headerComment */")
    outSrcHeader.puts
    outSrcHeader.puts("#define KS_DEPEND_ON_INTERNALS")

    outHdrHeader.puts(s"/* $headerComment */")
    outHdrHeader.puts
    outHdrHeader.puts(s"#ifndef KAITAI_${topClassName.toUpperCase()}_H")
    outHdrHeader.puts(s"#define KAITAI_${topClassName.toUpperCase()}_H")
    outHdrHeader.puts

    importListSrc.addLocal(outFileNameHeader(topClassName))

    importListHdr.addKaitai("kaitaistruct.h")

    outHdrDefs.puts
    outHdrDefs.puts("/* Forward declarations */")

    outHdrArrays.puts
    outHdrArrays.puts("/* Array structures */")

    outHdrEnums.puts
    outHdrEnums.puts("/* Enums */")

    outHdr.puts
    outHdr.puts("/* Main structures */")

    outSrcDefs.puts
    outHdrDefs.puts
    outSrcInstances.puts

    outHdrFinish.puts
    outHdrFinish.puts("#endif")
  }

  override def fileFooter(topClassName: String): Unit = {
    outMethodBody.dec
    outMethodBody.puts("}")
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    val name = classSpec.name.head.toLowerCase();
    importedTypes.append(name)
    importListHdr.addLocal(outFileNameHeader(name))
  }

 override def importFile(file: String): Unit = {
    val name = file.toLowerCase().split("/").last
    importedTypes.append(name)
    importListHdr.addLocal(outFileNameHeader(name))
    outHdrDefs.puts(s"typedef struct ksx_${name}_ ksx_$name;")
  }

  override def classHeader(name: List[String]): Unit = {
    val className = makeName(name)
    outHdrStructs.append(new StringLanguageOutputWriter(indent))
    outHdrInternalStructs.append(new StringLanguageOutputWriter(indent))
    outSrcInstancesFillArray.append(new StringLanguageOutputWriter(indent))
    val outStruct = outHdrStructs.last
    val outInternalStruct = outHdrInternalStructs.last
    val outInstancesFill = outSrcInstancesFillArray.last
    if (outHdrStructs.length == 1) {
      outHdr.puts
      outHdr.puts(s"ksx_${className}* ksx_read_${className}_from_stream(ks_stream* stream, int* error);")
      outSrcMain.puts
      outSrcMain.puts(s"ksx_${className}* ksx_read_${className}_from_stream(ks_stream* stream, int* error)")
      outSrcMain.puts("{")
      outSrcMain.inc
      outSrcMain.puts(s"ksx_${className}_internal* data = calloc(1, sizeof(ksx_${className}_internal));");
      outSrcMain.puts(s"ksx_read_$className(stream, data, 0, stream, data);")
      outSrcMain.puts("if (error) *error = *stream->err;")
      outSrcMain.puts(s"return (ksx_${className}*)data;")
      outSrcMain.dec
      outSrcMain.puts("}")
    } else {
      outHdrArrays.puts
      outHdrArrays.puts(s"typedef struct ksx_array_${className}_")
      outHdrArrays.puts("{")
      outHdrArrays.inc
      outHdrArrays.puts("ks_handle _handle;")
      outHdrArrays.puts("int64_t size;")
      outHdrArrays.puts(s"ksx_$className** data;")
      outHdrArrays.dec
      outHdrArrays.puts(s"} ksx_array_$className;")
      outHdrDefs.puts(s"typedef struct ksx_array_${className}_ ksx_array_${className};")
    }
    outStruct.puts
    outStruct.puts(s"typedef struct ksx_${className}_")
    outStruct.puts("{")
    outStruct.inc
    outStruct.puts("ks_handle _handle;")
    outHdrDefs.puts(s"typedef struct ksx_${className}_ ksx_${className};")

    outInternalStruct.puts(s"typedef struct ksx_${className}_internal")
    outInternalStruct.puts("{")
    outInternalStruct.inc
    outInternalStruct.puts(s"ksx_${className} data;")
    outSrcDefs.puts(s"typedef struct ksx_${className}_internal ksx_${className}_internal;");

    outSrcDefs.puts(s"static void ksx_fill_${className}_instances(ksx_${className}_internal* data);")
    outInstancesFill.puts(s"static void ksx_fill_${className}_instances(ksx_${className}_internal* data)")
    outInstancesFill.puts("{")
    outInstancesFill.inc

    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        outStruct.puts(s"ks_bool ${privateMemberName(EndianIdentifier)};")
      case _ =>
        // no _is_le variable
    }
  }

  override def classFooter(name: List[String]): Unit = {
    val className = makeName(name)
    val outStruct = outHdrStructs.last
    currentClassNames.remove(currentClassNames.length -1)
    outHdrStructs.remove(outHdrStructs.length -1)
    val outInternalStruct = outHdrInternalStructs.last
    outHdrInternalStructs.remove(outHdrInternalStructs.length - 1)
    val outInstancesFill = outSrcInstancesFillArray.last
    outSrcInstancesFillArray.remove(outSrcInstancesFillArray.length - 1)
    outStruct.dec
    outStruct.puts(s"} ksx_$className;")
    outInternalStruct.dec
    outInternalStruct.puts(s"} ksx_${className}_internal;")
    outHdr.add(outStruct)
    outSrcInternalStruct.add(outInternalStruct)
    outInstancesFill.dec
    outInstancesFill.puts("}")
    outSrcInstancesFill.add(outInstancesFill)
  }

  var currentClassNames : ListBuffer[String] = ListBuffer()
  var currentRootName = ""

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val className = makeName(name)
    currentClassNames.append(className)
    val rootName = makeName(rootClassName)
    currentRootName = rootName
    val parentName = kaitaiType2NativeTypeNonInternal(parentType)
    val outStruct = outHdrStructs.last
    outSrcDefs.puts(s"static void ksx_read_${className}(ks_stream* root_stream, ksx_${rootName}_internal* root_data, void* parent_data, ks_stream* stream, ksx_${className}_internal* data);");
    outSrcMain.puts
    outSrcMain.puts(s"static void ksx_read_${className}(ks_stream* root_stream, ksx_${rootName}_internal* root_data, void* parent_data, ks_stream* stream, ksx_${className}_internal* data)")
    outSrcMain.puts("{")
    outSrcMain.inc
    outSrcMain.puts(s"CHECKV(data->data._handle = ks_handle_create(stream, data, KS_TYPE_USERTYPE, sizeof(ksx_${className}_internal)));")
    outStruct.puts(s"$parentName* _parent;")
    outSrcMain.puts(s"data->data._parent = ($parentName*)parent_data;")
    outSrcMain.puts(s"ksx_fill_${className}_instances(data);")

    outMethodHead.inc
    outMethodBody.inc
  }

  override def classConstructorFooter: Unit = {
    outSrcMain.dec
    outSrcMain.puts("}")
  }

  override def runRead(name: List[String]): Unit = {
    outSrcMain.puts(s"CHECKV(ksx_read_${currentClassNames.last}_x(root_stream, root_data, stream, data));");
  }

  override def runReadCalc(): Unit = {
    outMethodBody.puts
    outMethodBody.puts(s"if (data->data.${privateMemberName(EndianIdentifier)}) {")
    outMethodBody.inc
    outMethodBody.puts(s"CHECKV(ksx_read_${currentClassNames.last}_le(root_stream, root_data, stream, data));");
    outMethodBody.dec
    outMethodBody.puts("} else {")
    outMethodBody.inc
    outMethodBody.puts(s"CHECKV(ksx_read_${currentClassNames.last}_be(root_stream, root_data, stream, data));");
    outMethodBody.dec
    outMethodBody.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
    val suffix = endian match {
      case Some(e) => e.toSuffix
      case None => "x"
    }
    val currentClassName = currentClassNames.last
    outSrcDefs.puts(s"static void ksx_read_${currentClassName}_$suffix(ks_stream* root_stream, ksx_${currentRootName}_internal* root_data, ks_stream* stream, ksx_${currentClassName}_internal* data);");
    outSrcMain.puts(s"static void ksx_read_${currentClassName}_$suffix(ks_stream* root_stream, ksx_${currentRootName}_internal* root_data, ks_stream* stream, ksx_${currentClassName}_internal* data)");
    outSrcMain.puts("{")
  }

  def makeFooter(instance: Boolean) : Unit = {
    val outSrc = if (instance) outSrcInstances else outSrcMain
    if (outMethodHasI)
    {
      val index = translator.doName(Identifier.INDEX)
      outMethodHead.puts(s"int64_t $index;")
      outMethodHasI = false
    }
    outSrc.add(outMethodHead)
    if (outMethodHead.result != "") {
        outSrc.puts
    }
    outSrc.add(outMethodBody)
    outSrc.puts
    outSrc.puts("}")
    outMethodHead = new StringLanguageOutputWriter(indent)
    outMethodBody = new StringLanguageOutputWriter(indent)
  }

  override def readFooter(): Unit = makeFooter(false)

  def getPtrSuffix(dataType: DataType): String = {
    dataType match {
      case t: UserType => "*"
      case at: ArrayType => "*"
      case KaitaiStructType | CalcKaitaiStructType => "*"
      case AnyType => "*"
      case sw: SwitchType => getPtrSuffix(sw.combinedType)
      case _ => ""
    }
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    attributeDeclaration(attrName, attrType, isNullable, false)
  }

  def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean, isInstance: Boolean): Unit = {
    val outStruct = outHdrStructs.last
    val outInternalStruct = outHdrInternalStructs.last
    val outInstancesFill = outSrcInstancesFillArray.last
    val name = idToStr(attrName)
    if (name == "_parent" || name == "_root")
    {
      return
    }
    val suffix = getPtrSuffix(attrType)
    val isSubtypeByte = attrName match {
        case RawIdentifier(_) => true
        case _ => false
    }
    if (isSubtypeByte) {
        return
    }
    if (isNullable) {
      outStruct.puts(s"ks_bool ${nullFlagForName(attrName)};")
    }
    val currentClassName = currentClassNames.last
    val typeStr = kaitaiType2NativeType(attrType) + getPtrSuffix(attrType)
    outStruct.puts(s"${kaitaiType2NativeTypeNonInternal(attrType)}$suffix ${privateMemberName(attrName)};")

    outSrcInstancesGet.puts(s"static ${typeStr} ksx_get_${currentClassName}_${name}(ksx_${currentClassName}_internal* data)")
    outSrcInstancesGet.puts("{")
    outSrcInstancesGet.inc
    if (isInstance) {
      outSrcInstancesGet.puts(s"ksx_read_${currentClassName}_instance_${name}(ks_stream_get_root(data->data._handle.stream), data->data._handle.stream, data);")
    }
    attrType match {
      case t: UserType =>
        val cast = kaitaiType2NativeType(t)
        outSrcInstancesGet.puts(s"return (${cast}*)data->data.${name};")
      case _ =>
        outSrcInstancesGet.puts(s"return data->data.${name};")
    }
    outSrcInstancesGet.dec
    outSrcInstancesGet.puts("}")
    outSrcInstancesGet.puts

    outInternalStruct.puts(s"${typeStr} (*${funcForInstName(attrName)})(ksx_${currentClassName}_internal* data);")
    outInstancesFill.puts(s"data->${funcForInstName(attrName)} = ksx_get_${currentClassName}_${name};")
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def universalDoc(doc: DocSpec): Unit = {}

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    outMethodBody.puts(s"if (data->data.${privateMemberName(EndianIdentifier)}) {")
    outMethodBody.inc
    leProc()
    outMethodBody.dec
    outMethodBody.puts("} else {")
    outMethodBody.inc
    beProc()
    outMethodBody.dec
    outMethodBody.puts("}")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    outMethodBody.puts(s"${privateMemberName(attrName)} = $normalIO.EnsureFixedContents($contents);")

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = "_raw_" + getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val t = translator.detectType(xorValue)
        t match {
          case t1: BytesType =>
            outMethodBody.puts(s"$srcExpr = ks_bytes_process_xor_bytes($srcExpr, ${expression(xorValue)});")
          case Int1Type(_) =>
            outMethodBody.puts(s"$srcExpr = ks_bytes_process_xor_int($srcExpr, ${expression(xorValue)}, 1);")
          case _ =>
            outMethodBody.puts("Unknown xor type: " + t.toString())
        }
      case ProcessZlib =>
        outMethodBody.puts(s"$srcExpr = stream->config.inflate($srcExpr);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        outMethodBody.puts(s"$srcExpr = ks_bytes_process_rotate_left($srcExpr, $expr);")
      case ProcessCustom(typename, args) =>
        val name2 = idToStr(varSrc)
        val procClass = typename.last
        importListHdr.addLocal(outFileNameHeader(typename.last))
        outMethodHead.puts(s"ks_custom_decoder _decoder_$name2;")
        outMethodBody.puts(s"_decoder_$name2 = ${procClass}_create(${args.map(expression).mkString(", ")});")
        outMethodBody.puts(s"$srcExpr = _decoder_$name2.decode(_decoder_$name2.userdata, $srcExpr);")
    }
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val privateVarName = privateMemberName(varName)

    val ioName = s"_io_$privateVarName"

    val args = getRawIdExpr(varName, rep)

    outMethodHead.puts(s"ks_stream* $ioName;")
    outMethodBody.puts(s"/* Subtype with substream */")
    outMethodBody.puts(s"$ioName = ks_stream_create_from_bytes(_raw_$args);")
    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case _ => s"$memberName"
    }
  }

  override def useIO(ioEx: expr): String = {
    outMethodBody.puts(s"$kstreamName* io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit = {
    // outMethodBody.puts(s"long _pos = $io.Pos;")
  }

  override def seek(io: String, pos: Ast.expr): Unit =
    outMethodBody.puts(s"CHECKV(ks_stream_seek(stream, ${expression(pos)}));")

  override def popPos(io: String): Unit = {
    // outMethodBody.puts(s"$io.Seek(_pos);")
  }

  override def alignToByte(io: String): Unit = {
    val io_new = makeIO(io)
    outMethodBody.puts(s"CHECKV(ks_stream_align_to_byte($io_new));")
  }

  override def instanceClear(instName: InstanceIdentifier): Unit = {}

  override def instanceSetCalculated(instName: InstanceIdentifier): Unit = {
    outMethodBody.puts(s"data->${flagForInstName(instName)} = 1;")
  }

  override def condIfHeader(expr: expr): Unit = {
    outMethodBody.puts(s"if (${expression(expr)}) {")
    outMethodBody.inc
  }

  def nullFlagForName(ksName: Identifier) =
    s"_is_valid_${idToStr(ksName)}"

  override def condIfSetNull(instName: Identifier): Unit =
    outMethodBody.puts(s"data->data.${nullFlagForName(instName)} = 0;")

  override def condIfSetNonNull(instName: Identifier): Unit =
    outMethodBody.puts(s"data->data.${nullFlagForName(instName)} = 1;")

  override def condIfFooter(expr: expr): Unit = {
    outMethodBody.dec
    outMethodBody.puts("}")
  }

  var repeatWasUsertype = false

  def instanceRepeatStart(name: String, dataType: DataType) : Unit = {
     repeatWasUsertype = dataType match {
      case t: UserType => true
      case st: SwitchType =>
         st.cases.values.exists((t) => t.isInstanceOf[UserType])
      case _ => false
    }
    if (repeatWasUsertype) {
      outMethodBody.puts(s"for (i = 0; i < data->data.$name->size; i++)")
      outMethodBody.puts("{")
      outMethodBody.inc
    }
  }

  def instanceRepeatEnd() : Unit = {
    if (repeatWasUsertype) {
      outMethodBody.dec
      outMethodBody.puts("}")
    }
    repeatWasUsertype = false
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw): Unit = {
    val name = privateMemberName(RawIdentifier(id))
    val pos = translator.doName(Identifier.INDEX)
    val dataTypeArray = ArrayTypeInStream(dataType)
    val arrayTypeSize = getKaitaiTypeEnumAndSize(dataType)
    val io_new = makeIO(io)
    outMethodHasI = true
    outMethodBody.puts("/* Array (repeat-eos) */")
    outMethodBody.puts(s"data->data.$name = calloc(1, sizeof(${kaitaiType2NativeType(dataTypeArray)}));")
    outMethodBody.puts(s"data->data.$name->size = 0;")
    outMethodBody.puts(s"data->data.$name->data = 0;")
    outMethodBody.puts(s"CHECKV(data->data.$name->_handle = ks_handle_create(stream, data->data.$name, $arrayTypeSize));");
    outMethodBody.puts("{")
    outMethodBody.inc
    outMethodBody.puts(s"while (!ks_stream_is_eof($io_new)) {")
    outMethodBody.puts(s"$pos = data->data.$name->size;");
    outMethodBody.inc

    instanceRepeatStart(name, dataType)
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    val name = privateMemberName(RawIdentifier(id))
    val ptr = getPtrSuffix(dataTypeLast)
    val sizeof = s"sizeof(${kaitaiType2NativeType(dataTypeLast)}$ptr)"
    outMethodBody.puts(s"data->data.$name->size++;")
    outMethodBody.puts(s"data->data.$name->data = realloc(data->data.$name->data, $sizeof * data->data.$name->size);")
    outMethodBody.puts(s"memset(data->data.$name->data + data->data.$name->size - 1, 0, $sizeof);")
    handleAssignmentC(id, dataTypeLast, assignTypeLast, ioLast, defEndianLast, expr, true)
  }

  override def condRepeatEosFooter: Unit = {
    outMethodBody.dec
    outMethodBody.puts("}")
    outMethodBody.dec
    outMethodBody.puts("}")

    instanceRepeatEnd()
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: expr): Unit = {
    val pos = translator.doName(Identifier.INDEX)
    val len = expression(repeatExpr)
    val name = privateMemberName(RawIdentifier(id))
    val dataTypeArray = ArrayTypeInStream(dataType)
    val arrayTypeSize = getKaitaiTypeEnumAndSize(dataType)
    val ptr = getPtrSuffix(dataTypeLast)
    outMethodHasI = true
    outMethodBody.puts("/* Array (repeat-expr) */")
    outMethodBody.puts(s"data->data.$name = calloc(1, sizeof(${kaitaiType2NativeType(dataTypeArray)}));")
    outMethodBody.puts(s"data->data.$name->size = $len;")
    outMethodBody.puts(s"data->data.$name->data = calloc(sizeof(${kaitaiType2NativeType(dataType)}$ptr), data->data.$name->size);")
    outMethodBody.puts(s"CHECKV(data->data.$name->_handle = ks_handle_create(stream, data->data.$name, $arrayTypeSize));");
    outMethodBody.puts(s"for ($pos = 0; $pos < data->data.$name->size; $pos++)")
    outMethodBody.puts("{")
    outMethodBody.inc

    instanceRepeatStart(name, dataType)
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    handleAssignmentC(id, dataTypeLast, assignTypeLast, ioLast, defEndianLast, expr, true)
  }

  override def condRepeatExprFooter: Unit = {
    fileFooter(null)

    instanceRepeatEnd()
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, untilExpr: expr): Unit = {
    val pos = translator.doName(Identifier.INDEX)
    val name = privateMemberName(RawIdentifier(id))
    val dataTypeArray = ArrayTypeInStream(dataType)
    val arrayTypeSize = getKaitaiTypeEnumAndSize(dataType)
    val ptr = getPtrSuffix(dataType)
    outMethodHasI = true
    outMethodBody.puts("/* Array (repeat-until) */")
    outMethodBody.puts(s"data->data.$name = calloc(1, sizeof(${kaitaiType2NativeType(dataTypeArray)}));")
    outMethodBody.puts(s"data->data.$name->size = 0;")
    outMethodBody.puts(s"data->data.$name->data = 0;")
    outMethodBody.puts(s"CHECKV(data->data.$name->_handle = ks_handle_create(stream, data->data.$name, $arrayTypeSize));");
    outMethodBody.puts("{")
    outMethodBody.inc
    outMethodBody.puts("i = 0;")
    outMethodBody.puts(s"${kaitaiType2NativeType(dataType)}$ptr ${translator.doName("_")} = {0};")
    outMethodBody.puts(s"(void)${translator.doName("_")};")
    outMethodBody.puts(s"do")
    outMethodBody.puts("{")
    outMethodBody.inc

    instanceRepeatStart(name, dataType)
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val name = privateMemberName(RawIdentifier(id))
    val nameTemp = translator.doName(Identifier.ITERATOR)
    val ptr = getPtrSuffix(dataTypeLast)
    val sizeof = s"sizeof(${kaitaiType2NativeType(dataTypeLast)}$ptr)"
    outMethodBody.puts(s"data->data.$name->size++;")
    outMethodBody.puts(s"data->data.$name->data = realloc(data->data.$name->data, $sizeof * data->data.$name->size);")
    outMethodBody.puts(s"memset(data->data.$name->data + data->data.$name->size - 1, 0, $sizeof);")
    handleAssignmentC(id, dataTypeLast, assignTypeLast, ioLast, defEndianLast, expr, true)
    val pos = translator.doName(Identifier.INDEX)
    outMethodBody.puts(s"$nameTemp = data->data.$name->data[$pos];");
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, untilExpr: expr): Unit = {
    val pos = translator.doName(Identifier.INDEX)
    typeProvider._currentIteratorType = Some(dataType)
    outMethodBody.puts(s"$pos++;")
    outMethodBody.dec
    outMethodBody.puts(s"} while (!(${expression(untilExpr)}));")
    outMethodBody.dec
    outMethodBody.puts("}")

    instanceRepeatEnd()
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    handleAssignmentC(id, dataTypeLast, assignTypeLast, ioLast, defEndianLast, expr, false)
  }

  def handleAssignmentC(id: Identifier, dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian], expr: String, isArray: Boolean)
  {
    // TODO: Use io!
    val name = privateMemberName(id)
    var nameTarget = name
    if (isArray) {
        val pos = translator.doName(Identifier.INDEX)
        nameTarget = s"$name->data[$pos]"
    }
    if (lastWasInstanceValue) {
        lastWasInstanceValue = false
        return
    }
    val io_new = makeIO(io)
    // outMethodBody.puts(s"/* $io -> ${dataType.toString()} __ ${assignType.toString()} */")
    val targetType = kaitaiType2NativeType(dataType)

    dataType match {
      case t: ReadableType =>
        outMethodBody.puts(s"CHECKV(data->data.$nameTarget = ks_stream_read_${t.apiCall(defEndian)}($io_new));")
      case blt: BytesLimitType =>
        outMethodHead.puts(s"ks_bytes* _raw_$name;")
        outMethodBody.puts(s"CHECKV(_raw_$name = ks_stream_read_bytes($io_new, ${expression(blt.size)}));")
        id match {
          case RawIdentifier(_) =>
          case _ =>
            val expr2 = expr.replace("__EXPR__", s"_raw_$name")
            outMethodBody.puts(s"data->data.$nameTarget = $expr2;")
        }
      case _: BytesEosType =>
        outMethodHead.puts(s"ks_bytes* _raw_$name;")
        outMethodBody.puts(s"CHECKV(_raw_$name = ks_stream_read_bytes_full($io_new));")
        id match {
          case RawIdentifier(_) =>
          case _ =>
            val expr2 = expr.replace("__EXPR__", s"_raw_$name")
            outMethodBody.puts(s"data->data.$nameTarget = $expr2;")
        }
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        val include2 = if (include) 1 else 0
        val consume2 = if (consume) 1 else 0
        val eosError2 = if (eosError) 1 else 0
        outMethodHead.puts(s"ks_bytes *_raw_$name;")
        outMethodBody.puts(s"CHECKV(_raw_$name = ks_stream_read_bytes_term($io_new, $terminator, $include2, $consume2, $eosError2));")
        val expr2 = expr.replace("__EXPR__", s"_raw_$name")
        outMethodBody.puts(s"data->data.$nameTarget = $expr2;")
      case BitsType1(bitEndian) =>
        outMethodBody.puts(s"CHECKV(data->data.$nameTarget = ks_stream_read_bits_${bitEndian.toSuffix.toLowerCase()}($io_new, 1));")
      case BitsType(width: Int, bitEndian) =>
        outMethodBody.puts(s"CHECKV(data->data.$nameTarget = ks_stream_read_bits_${bitEndian.toSuffix.toLowerCase()}($io_new, $width));")
      case t: UserTypeFromBytes =>
        val parent = t.forcedParent match {
          case Some(USER_TYPE_NO_PARENT) => "0"
          case Some(fp) => translator.translate(fp)
          case None => "data"
        }
        val typeName = makeName(t.classSpec.get.name)
        if (importedTypes.contains(typeName)) {
          outMethodBody.puts(s"data->data.$nameTarget = calloc(1, sizeof(ksx_${typeName}));")
          outMethodBody.puts(s"CHECKV(data->data.$nameTarget = ksx_read_${typeName}_from_stream(_io_$name, 0);")
        } else {
          outMethodBody.puts(s"data->data.$nameTarget = calloc(1, sizeof(ksx_${typeName}_internal));")
          outMethodBody.puts(s"CHECKV(ksx_read_$typeName(root_stream, root_data, $parent, _io_$name, (ksx_${typeName}_internal*)data->data.$nameTarget));")
        }
        // outMethodBody.puts(s"ks_stream_destroy(_io_$name);")
      case t: UserTypeInstream =>
        val parent = t.forcedParent match {
          case Some(USER_TYPE_NO_PARENT) => "0"
          case Some(fp) => translator.translate(fp)
          case None => "data"
        }
        val typeName = makeName(t.classSpec.get.name)
        outMethodBody.puts(s"/* Subtype */")
        if (importedTypes.contains(typeName)) {
          outMethodBody.puts(s"data->data.$nameTarget = calloc(1, sizeof(ksx_${typeName}));")
          outMethodBody.puts(s"CHECKV(data->data.$nameTarget = ksx_read_${typeName}_from_stream($io_new, 0));")
        } else {
          outMethodBody.puts(s"data->data.$nameTarget = calloc(1, sizeof(ksx_${typeName}_internal));")
          outMethodBody.puts(s"CHECKV(ksx_read_$typeName(root_stream, root_data, $parent, $io_new, (ksx_${typeName}_internal*)data->data.$nameTarget));")
        }
      case _ =>
        outMethodBody.puts("Missing expression type: " + dataType.toString())
    }
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit = {}

  override def blockScopeHeader: Unit = {
    outMethodBody.puts("{")
    outMethodBody.inc
  }
  override def blockScopeFooter: Unit = {
    outMethodBody.dec
    outMethodBody.puts("}")
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
    "__EXPR__"
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"ks_bytes_strip_right($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"ks_bytes_terminate($expr1, $term, ${if (include) 1 else 0})"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {}

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType | _: EnumType => false
    case _ => true
  }

  //<editor-fold desc="switching: true version">

  val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  var outMethodHeadOldSwitch: StringLanguageOutputWriter = null
  var outMethodBodyOldSwitch: StringLanguageOutputWriter = null

  def switchOverrideStart() : Unit = {
    outMethodHeadOldSwitch = outMethodHead
    outMethodBodyOldSwitch = outMethodBody
    outMethodHead = new StringLanguageOutputWriter(indent)
    outMethodBody = new StringLanguageOutputWriter(indent)
    outMethodHead.indentLevel = outMethodBodyOldSwitch.indentLevel
    outMethodBody.indentLevel = outMethodBodyOldSwitch.indentLevel
  }

  def switchOverrideEnd() : Unit = {
    outMethodBodyOldSwitch.add(outMethodHead)
    if (outMethodHead.result != "") {
        outMethodBodyOldSwitch.puts
    }
    outMethodBodyOldSwitch.add(outMethodBody)

    outMethodHead = outMethodHeadOldSwitch
    outMethodBody = outMethodBodyOldSwitch
    outMethodHeadOldSwitch = null;
    outMethodBodyOldSwitch = null
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    outMethodBody.puts(s"switch (${expression(on)}) {")
    if (repeatWasUsertype) {
      outMethodBody.puts(s"switch (${expression(on)}) {")
    }
  }

  override def switchCaseFirstStart(condition: Ast.expr): Unit = switchCaseStart(condition)

  override def switchCaseStart(condition: Ast.expr): Unit = {
    outMethodBody.puts(s"case ${expression(condition)}: {")
    outMethodBody.inc
    switchOverrideStart()
    if (repeatWasUsertype) {
      outMethodBody.puts(s"case ${expression(condition)}: {")
      outMethodBody.inc
    }
  }

  override def switchCaseEnd(): Unit = {
    switchOverrideEnd()
    outMethodBody.puts("break;")
    outMethodBody.dec
    outMethodBody.puts("}")
    if (repeatWasUsertype) {
      outMethodBody.puts("break;")
      outMethodBody.dec
      outMethodBody.puts("}")
    }
  }

  override def switchElseStart(): Unit = {
    outMethodBody.puts("default: {")
    outMethodBody.inc
    switchOverrideStart()
    if (repeatWasUsertype) {
      outMethodBody.puts("default: {")
      outMethodBody.inc
    }
  }

  override def switchEnd(): Unit = {
    outMethodBody.puts("}")
    if (repeatWasUsertype) {
      outMethodBody.puts("}")
    }
  }

  //</editor-fold>

  //<editor-fold desc="switching: emulation with ifs">

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    outMethodBody.puts("{")
    outMethodBody.inc
    outMethodBody.puts(s"${kaitaiType2NativeType(onType)} ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
    if (repeatWasUsertype) {
      outMethodBody.puts("{")
      outMethodBody.inc
      outMethodBody.puts(s"${kaitaiType2NativeType(onType)} ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
    }
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
    outMethodBody.puts(s"if (${switchCmpExpr(condition)})")
    outMethodBody.puts("{")
    outMethodBody.inc
    switchOverrideStart()
    if (repeatWasUsertype) {
      outMethodBody.puts(s"if (${switchCmpExpr(condition)})")
      outMethodBody.puts("{")
      outMethodBody.inc
    }
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    outMethodBody.puts(s"else if (${switchCmpExpr(condition)})")
    outMethodBody.puts("{")
    outMethodBody.inc
    switchOverrideStart()
    if (repeatWasUsertype) {
      outMethodBody.puts(s"else if (${switchCmpExpr(condition)})")
      outMethodBody.puts("{")
      outMethodBody.inc
    }
  }

  override def switchIfCaseEnd(): Unit = {
    switchOverrideEnd()
    outMethodBody.dec
    outMethodBody.puts("}")
    if (repeatWasUsertype) {
      outMethodBody.dec
      outMethodBody.puts("}")
    }
  }

  override def switchIfElseStart(): Unit = {
    outMethodBody.puts("else")
    outMethodBody.puts("{")
    outMethodBody.inc
    switchOverrideStart()
    if (repeatWasUsertype) {
      outMethodBody.puts("else")
      outMethodBody.puts("{")
      outMethodBody.inc
    }
  }

  override def switchIfEnd(): Unit = {
    outMethodBody.dec
    outMethodBody.puts("}")
    if (repeatWasUsertype) {
      outMethodBody.dec
      outMethodBody.puts("}")
    }
  }

  //</editor-fold>
  def flagForInstName(ksName: Identifier) = s"_flag_${idToStr(ksName)}"
  def funcForInstName(ksName: Identifier) = s"_get_${idToStr(ksName)}"

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    val outInternalStruct = outHdrInternalStructs.last
    outInternalStruct.puts(s"ks_bool ${flagForInstName(attrName)};")
    attributeDeclaration(attrName, attrType, isNullable, true)
  }

  override def instanceHeader(classNameList: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    val className = makeName(classNameList)
    val name = privateMemberName(instName)

    outMethodHead.puts(s"static void ksx_read_${className}_instance_${name}(ks_stream* root_stream, ks_stream* stream, ksx_${className}_internal* data)")
    outMethodHead.puts("{")
    outMethodHead.inc
    outMethodBody.inc
  }

  override def instanceFooter: Unit = makeFooter(true)

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    outMethodBody.puts(s"if (data->${flagForInstName(instName)})")
    outMethodBody.inc
    outMethodBody.puts("return;")
    outMethodBody.dec
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {}

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: expr): Unit = {
    lastWasInstanceValue = true
    val name = privateMemberName(instName)
    val ex = expression(value)
    outMethodBody.puts(s"data->data.$name = $ex;")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    val enumPath : ListBuffer[String] = ListBuffer()
    enumPath.appendAll(curClass)
    enumPath.append(enumName)
    val enumClass = makeName(enumPath)
    val enumClass2 = "ksx_" + enumClass

    outHdrEnums.puts
    outHdrEnums.puts(s"typedef enum ${enumClass2}_")
    outHdrEnums.puts(s"{")
    outHdrEnums.inc

    enumColl.foreach { case (id, label) =>
      val value = translator.doIntLiteral(id)
      outHdrEnums.puts(s"${enumClass2.toUpperCase()}_${label.name.toUpperCase()} = $value,")
    }

    outHdrEnums.dec
    outHdrEnums.puts(s"} $enumClass2;")

    outHdrArrays.puts
    outHdrArrays.puts(s"typedef struct ksx_array_${enumClass}_")
    outHdrArrays.puts("{")
    outHdrArrays.inc
    outHdrArrays.puts("ks_handle _handle;")
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
    val typeStr = kaitaiType2NativeType(attrType)
    val ptr = getPtrSuffix(attrType)
    val name = privateMemberName(attrId)
    outMethodBody.puts("{")
    outMethodBody.inc
    outMethodBody.puts(s"$typeStr$ptr _temp_ = data->data.$name;")
    outMethodBody.puts(s"(void)_temp_;")
    val errArgsStr = errArgs.map(translator.translate).mkString(", ")
    outMethodBody.puts(s"if (!(${translator.translate(checkExpr)}))")
    outMethodBody.puts("{")
    outMethodBody.inc
    //outMethodBody.puts(s"throw new ${ksErrorName(err)}($errArgsStr);")
    outMethodBody.puts(s"*stream->err = 1;")
    outMethodBody.puts(s"return;")
    outMethodBody.dec
    outMethodBody.puts("}")
    outMethodBody.dec
    outMethodBody.puts("}")
  }
  override def type2class(className: String): String =
    className
}

object CCompiler extends LanguageCompilerStatic
  with StreamStructNames
  with UpperCamelCaseClasses
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new CCompiler(tp, config)

  def kaitaiType2NativeTypeArray(attrType: DataType): String = {
    attrType match {
      case t: UserType => s"ksx_array_${makeName(t.classSpec.get.name)}"
      case t: EnumType => s"ksx_array_${makeName(t.enumSpec.get.name)}"
      case _: StrType => s"ks_array_string"
      case _: BytesType => s"ks_array_bytes"
      case KaitaiStructType | CalcKaitaiStructType => s"ks_array_usertype_generic"
      case AnyType => "ks_array_any"
      case st: SwitchType => kaitaiType2NativeTypeArray(st.combinedType)
      case _ => s"ks_array_${kaitaiType2NativeType(attrType)}"
    }
  }

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

      case CalcIntType => "int64_t"
      case CalcFloatType => "double"
      case _: BooleanType => "ks_bool"

      case _: StrType => "ks_string*"
      case _: BytesType => "ks_bytes*"

      case AnyType => "void"
      case KaitaiStructType | CalcKaitaiStructType => kstructName
      case KaitaiStreamType | OwnedKaitaiStreamType => kstreamName

      case t: UserType =>
        if (t.isOpaque) {
          "ksx_" + makeName(t.classSpec.get.name)
        } else {
          "ksx_" + makeName(t.classSpec.get.name) + "_internal"
        }
      case t: EnumType => "ksx_" + makeName(t.enumSpec.get.name)

      case at: ArrayType => kaitaiType2NativeTypeArray(at.elType)

      case st: SwitchType => kaitaiType2NativeType(st.combinedType)
    }
  }

  def kaitaiType2NativeTypeNonInternal(attrType: DataType): String = {
    attrType match {
      case t: UserType => "ksx_" + makeName(t.classSpec.get.name)

      case st: SwitchType => kaitaiType2NativeTypeNonInternal(st.combinedType)
      case _ => kaitaiType2NativeType(attrType)
    }
  }

  def getKaitaiTypeEnumAndSize(attrType: DataType): String = {
    attrType match {
      case Int1Type(false) => "KS_TYPE_ARRAY_UINT, 1"
      case IntMultiType(false, Width2, _) => "KS_TYPE_ARRAY_UINT, 2"
      case IntMultiType(false, Width4, _) => "KS_TYPE_ARRAY_UINT, 4"
      case IntMultiType(false, Width8, _) => "KS_TYPE_ARRAY_UINT, 8"

      case Int1Type(true) => "KS_TYPE_ARRAY_INT, 1"
      case IntMultiType(true, Width2, _) => "KS_TYPE_ARRAY_INT, 2"
      case IntMultiType(true, Width4, _) => "KS_TYPE_ARRAY_INT, 4"
      case IntMultiType(true, Width8, _) => "KS_TYPE_ARRAY_INT, 8"

      case FloatMultiType(Width4, _) => "KS_TYPE_ARRAY_FLOAT, 4"
      case FloatMultiType(Width8, _) => "KS_TYPE_ARRAY_FLOAT, 8"

      case _: StrType => "KS_TYPE_ARRAY_STRING, sizeof(ks_string*)"

      case BitsType(_, _) => "KS_TYPE_ARRAY_UINT, 8"
      case _: BytesType => s"KS_TYPE_ARRAY_BYTES, sizeof(ks_bytes*)"

      case t: UserType => s"KS_TYPE_ARRAY_USERTYPE, sizeof(ksx_${makeName(t.classSpec.get.name)}*)"
      case t: EnumType => getKaitaiTypeEnumAndSize(t.basedOn)

      case _ => "KS_TYPE_UNKNOWN, 0"
    }
  }

  def types2class(typeName: Ast.typeId): String =
    types2class(typeName.names)
  def types2class(names: Iterable[String]) = names.map(type2class).mkString(".").toLowerCase()

  def makeName(names: Iterable[String]) = {
    val arr = names.toList
    if (arr.length == 1)
      arr.mkString("_").toLowerCase()
    else
      arr.drop(1).mkString("_").toLowerCase()
  }

  def makeIO(io: String) = if (io == "_io") "stream" else io

  override def kstructName = "ks_usertype_generic"
  override def kstreamName = "ks_stream"
  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => "EndOfStreamException"
    case _ => err.name
  }
}

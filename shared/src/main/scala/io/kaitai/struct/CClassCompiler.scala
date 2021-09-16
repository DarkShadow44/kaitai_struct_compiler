package io.kaitai.struct

import scala.collection.mutable.ListBuffer
import io.kaitai.struct.datatype.DataType.{CalcIntType, KaitaiStreamType, UserTypeInstream}
import io.kaitai.struct.datatype.{BigEndian, CalcEndian, Endianness, FixedEndian, InheritedEndian, LittleEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.Ast.identifier
import io.kaitai.struct.exprlang.Ast.expr.{BoolOp, BinOp, UnaryOp, IfExp, Compare, Call, Attribute, CastToType, Subscript, Name}
import io.kaitai.struct.format._
import io.kaitai.struct.languages.CCompiler
import io.kaitai.struct.languages.components.ExtraAttrs

class CClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, CCompiler) {

  def getDependents(e: expr, list: ListBuffer[String]) : Unit = {
    e match {
      case BoolOp(_, values: Seq[expr]) =>
        for (e2  <- values) {
          getDependents(e2, list)
        }
      case BinOp(left: expr, _, right: expr) =>
        getDependents(left, list)
        getDependents(right, list)
      case UnaryOp(_, operand: expr) =>
        getDependents(operand, list)
      case IfExp(condition: expr, ifTrue: expr, ifFalse: expr) =>
        getDependents(condition, list)
        getDependents(ifTrue, list)
        getDependents(ifFalse, list)
      case Compare(left: expr, _, right: expr) =>
        getDependents(left, list)
        getDependents(right, list)
      case Call(func: expr, args: Seq[expr]) =>
        getDependents(func, list)
        for (e2  <- args) {
          getDependents(e2, list)
        }
      case Attribute(value: expr, attr: identifier) =>
        getDependents(value, list)
      case CastToType(value: expr, _) =>
        getDependents(value, list)
      case Subscript(value: expr, idx: expr) =>
        getDependents(value, list)
        getDependents(idx, list)
      case Name(id: identifier) =>
        list += id.name
      case expr.List(elts: Seq[expr]) =>
        for (e2  <- elts) {
          getDependents(e2, list)
        }
      case _ =>
    }
  }

  def getDependents(instSpec: InstanceSpec) : ListBuffer[String] = {
   val deps : ListBuffer[String] = ListBuffer()
    instSpec match {
      case vi: ValueInstanceSpec =>
        if (vi.ifExpr.nonEmpty) {
          getDependents(vi.ifExpr.get, deps)
        }
        getDependents(vi.value, deps)
      case pi: ParseInstanceSpec =>
        if (pi.pos.nonEmpty) {
          getDependents(pi.pos.get, deps)
        }
    }
    deps
  }

  def getSortedInstances(curClass: ClassSpec) : ListBuffer[InstanceIdentifier] = {
    val ret : ListBuffer[InstanceIdentifier] = ListBuffer()
    var found = true
    val allInstances = curClass.instances.map({case (k, v) => k.name}).toArray

    while (found) {
      found = false
      curClass.instances.foreach { case (instName, instSpec) =>
        if (!ret.contains(instName)) {
          val deps = getDependents(instSpec)
          val resolved = ret.map(x => x.name)
          val exists = deps.forall(x => resolved.contains(x) || !allInstances.contains(x))
          if (exists) {
            ret += instName
            found = true
          }
        }
      }
    }

    if (ret.length != curClass.instances.size) {
       curClass.instances.foreach { case (instName, instSpec) =>
         if (!ret.contains(instName)) {
           val currentDeps = ret.map(x => x.name).mkString
           throw new Exception(s"Cant resolve dependencies for instance '$instName'\n Current deps: $currentDeps")
         }
       }
    }
    ret
  }

  override def compileInstances(curClass: ClassSpec) = {
    val lang2 = lang.asInstanceOf[CCompiler]
    lang2.instanceStart(curClass.name)
    val instances = getSortedInstances(curClass)
    for (name <- instances) {
      compileInstance(curClass.name, name, curClass.instances.get(name).get, curClass.meta.endian)
    }
    lang.instanceFooter
  }

  override def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = {
    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    compileInstanceDeclaration(instName, instSpec)

    compileInstanceDoc(instName, instSpec)

    instSpec match {
      case vi: ValueInstanceSpec =>
        lang.attrParseIfHeader(instName, vi.ifExpr)
        lang.instanceCalculate(instName, dataType, vi.value)
        lang.attrParseIfFooter(vi.ifExpr)
        lang.instanceSetCalculated(instName)
      case pi: ParseInstanceSpec =>
        lang.attrParse(pi, instName, endian)
    }
  }
}

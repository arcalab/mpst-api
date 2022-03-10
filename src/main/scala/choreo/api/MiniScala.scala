package choreo.api

import choreo.api.Code


import java.util.concurrent.locks.StampedLock

// todo: needs to be rethought
object MiniScala:

  case class ScalaModule(name:String, statements:Statement) extends Code:
    def toCode(implicit i: Int): String =
      statements.toCode


  trait Statement extends Code


  case class Statements(statements:List[Statement]) extends Statement:

    def toCode(implicit i: Int): String = statements match
      case Nil => ""
      case l   => l.map(l=>l.toCode).mkString("\n\n")

  case class NoSepStatements(statements:List[Statement]) extends Statement:

    def toCode(implicit i: Int): String = statements match
      case Nil => ""
      case l   => l.map(l=>l.toCode).mkString("\n")

  case class BlockStatement(statement: Statement) extends Statement:
    def toCode(implicit i: Int): String = ind(i) + "{\n" ++ statement.toCode(i+1) ++ "\n" + ind(i) ++"}"

  case class PreCode(code:String) extends Statement with TExp:
    def toCode(implicit i: Int): String =
      val lines = code.split("\n")
      lines.map(l => ind(i)+l).mkString("\n")


  case class Import(imp:String)  extends Statement:
    def toCode(implicit i: Int): String =
      ind(i) ++ s"import $imp"

  case class Trait(name:String,typeVariables:List[String],statement:Statement) extends Statement :
    def toCode(implicit i: Int): String =
      val body = statement.toCode(i+1)
      ind(i) ++ s"trait $name${brackets(typeVariables,false)}" ++
        (if body.nonEmpty then ":\n" ++ body else "")

  case class Enum(name:String,typeVariables:List[String],enums:List[String]) extends Statement :
    def toCode(implicit i: Int): String = ind(i) ++
      s"enum $name${brackets(typeVariables,false)}:\n" ++
      ind(i+1) ++ "case " ++ enums.mkString(",")

  case class ScalaClass(
    name:String,
    typeVariables:List[(String,Option[String])],
    //typeVariablesTExp:List[String],
    parameters:List[Param],
    statements:Option[Statement],
    comment:Option[String],
    extendsWith:List[String]=Nil,
    caseClass:Boolean = true
  ) extends Statement:

    def toCode(implicit i: Int): String =
      //val tvars = typeVariables.zip(typeVariablesTExp)
      val com = if comment.isDefined then comment(comment.get)+"\n" else ""
      val c = if caseClass then "case " else ""
      val extension = if extendsWith.isEmpty then "" else s" extends ${extendsWith.mkString(",")}"
      val classDef = s"""${c}class $name${brackets(mkTVars())(i+1)}""" ++
          (if parameters.isEmpty then "" else  s"""${params(parameters.map(_.toString))(i+1)}""")
      val body = if statements.isDefined then statements.get.toCode(i+1) else ""

      if body.nonEmpty then
        com ++ ind(i) ++ classDef ++ s"$extension:\n\n" ++ body
      else
        com ++ ind(i) ++ classDef ++ extension

    def mkTVars():List[String] =
      typeVariables.map(t =>
        if t._2.isDefined then
          s"${t._1} <: ${t._2.get}"
        else t._1
      ).toList

  case class ScalaObject(
    name:String,
    statements:Option[Statement],
    comment:Option[String],
    extendsWith:List[String]=Nil,
  ) extends Statement:

    def toCode(implicit i: Int): String =
      val extension = if extendsWith.isEmpty then "" else s" extends ${extendsWith.mkString(",")}"
      val com = if comment.isDefined then comment(comment.get)+"\n" else ""
//      val com =
//        if comment.isDefined then
//          s"""/**
//             |${comment.get.split("\n").map(l=>ind(i)++" * "++l).mkString("\n")}
//             |*/
//             |
//             |""".stripMargin
//        else ""
      com ++ ind(i) ++ s"""object $name$extension""" ++
        (if statements.isDefined then
          s":\n\n" ++ statements.get.toCode(i+1)
        else "")

  case class MethodDef(
    name:String,
    typeVariables:List[String],
    parameters:List[Param],
    ev:Set[Evidence],
    outputType:Option[TExp],
    statement:Statement,
    comment:Option[String],
    annotation:Option[String] = None
  ) extends Statement:
    override def toCode(implicit i:Int):String =
      val com = if comment.isDefined then comment(comment.get)+"\n" else ""
      com ++ ind(i) ++
      (if annotation.isDefined then annotation.get ++"\n"++ind(i) else "") ++
        s"""def $name${brackets(typeVariables)(i+1)}${params(parameters.map(_.toString))(i+1)}${mkEvidence()(i+1)}${mkTExp()} =\n""" ++
        statement.toCode(i+1)

    protected def mkTExp():String =
      if outputType.isDefined then
        ": " ++ outputType.get.toString
      else ""

    protected def mkEvidence()(implicit i:Int):String =
      val evStr = ev.map(_.toString).toList
      val lg    = length(evStr)
      if ev.isEmpty then ""
      else if lg <40 then
        s"""(using ev: ${params(ev.map(_.toString).toList," | ",ln = false)(i+1)})"""
      else
        "(\n" ++ ind(i) ++
          s"""using ev: ${params(ev.map(_.toString).toList," | ")(i+1)}\n"""++
          s"${ind(Integer.max(i-1,0))})"


  // Parameter
  case class Param(name:String,typ:TExp) extends Code:
    def toCode(implicit i:Int):String =
      ind(i) ++ name ++ ":" ++ typ.toString

  //trait Exp extends Statement

  // todo: improve when possible (proper expressions)
  case class While(cond:String, block:Statement) extends Statement:

    def toCode(implicit i: Int): String =
      ind(i) ++ s"""while $cond do\n""" ++ block.toCode(i+1)

  case object UnitSt extends Statement:
    def toCode(implicit i: Int): String = ind(i) ++ "()"

  case class Asign(lhs:String,rhs:String) extends Statement:
    def toCode(implicit i: Int): String =
      ind(i) ++ s"""$lhs = $rhs"""

//  case class FunCall(
//    name:String,
//    typeVariables:List[String],
//    args:List[String]
//  ) extends Statement:
//
//    def toCode(implicit i: Int): String = ind(i) ++
//        s"""$name${brackets(typeVariables,ln=false)}${params(args,ln=false)}"""

  case class FunCall(
    name:String,
    typeVariables:List[String],
    args:List[Statement]
  ) extends Statement:

    def toCode(implicit i: Int): String = ind(i) ++
//      s"""$name${brackets(typeVariables,ln=false)}${params(args.map(_.toCode),ln=false)}"""
      s"""$name${brackets(typeVariables,ln=false)}${params(args.map(_.toString))(i+1)}"""

  case class FunCallLn(
    name:String,
    typeVariables:List[String],
    args:List[Statement]
  ) extends Statement:

    def toCode(implicit i: Int): String =
      val params = args.map(_.toCode(i+1))
      ind(i) ++
      //      s"""$name${brackets(typeVariables,ln=false)}${params(args.map(_.toCode),ln=false)}"""
        s"""$name${brackets(typeVariables,ln=false)}(\n""" +
        params.mkString(",\n") +  "\n" + ind(i) + ")"

  case class Extension(
    typeVariables:Map[String,Option[String]],
    //typeVariablesTExp:List[String],
    param:Param,
    methods:List[MethodDef]
  ) extends Statement:

    def addMethod(m:MethodDef):Extension = this.copy(methods = methods:+m)

    def toCode(implicit i: Int): String =
      //val tvars = typeVariables.zip(typeVariablesTExp)
      ind(i) ++
      s"""extension${brackets(mkTVars())(i+1)}""" ++
      s"""(${param.toString}) {\n\n""" ++ // todo: upd
      methods.map(m=>m.toCode(i+1)).mkString("\n\n") ++ ind(i) ++ s"\n${ind(i)}}"

    def mkTVars():List[String] =
      (typeVariables.map(t =>
        if t._2.isDefined then
          s"${t._1} <: ${t._2.get}"
        else t._1)
      ).toList

  // todo: if possible define a proper grammar
  case class VarDef(name:String, tExp:Option[TExp], rhs:String) extends Statement:
    def toCode(implicit i: Int): String =
      ind(i) ++ name ++ (
        if tExp.isDefined then
          ":" ++ tExp.get.toString
        else ""
        ) ++ " = " ++ rhs

  case class TypeDef(alias:TName, rhs:TExp) extends Statement:
    def toCode(implicit i: Int): String =  rhs match
      case t:TNameLn =>
        ind(i) ++ s"""type $alias =\n""" + s"""${rhs.toCode(i+1)}"""
      case _ => ind(i) ++ s"""type $alias = ${rhs.toString}"""
      //s"""type $name${brackets(typVars)} = ${tExp.toString}"""

  trait TExp extends Code:
    def size():Int = this match
      case TName(_,_) => 1
      case TTuple(l) => l.size

  case object TUnit extends TExp :
    def toCode(implicit i:Int):String = ind(i) ++ "Unit"

  case class TUnion(ors:List[String]) extends TExp:
    def toCode(implicit i:Int):String = ind(i) ++ ors.mkString(" | ")

  case class TName(name:String,typVars:Option[List[String]]=None) extends TExp:
    def toCode(implicit i:Int):String = ind(i) ++ (typVars match
      case Some(vars) => name++"["++vars.mkString(",")++"]"
      case None => name)

  case class TNameLn(name:String,typVars:List[TExp]) extends TExp:
    def toCode(implicit i: Int): String =
      val params = typVars.map(_.toCode(i+1))
      ind(i) ++
        s"""$name[\n"""  + params.mkString(",\n") +  "\n" + ind(i) + "]"

  case class TVar(name:String,typeConstraint:Option[TExp]=None) extends TExp:
    def toCode(implicit i:Int):String = ind(i) ++
      (if typeConstraint.isDefined then
        name ++ " <: " ++ typeConstraint.toString
      else name)


  case class TTuple(typs:List[TExp]) extends TExp:
    def toCode(implicit i:Int):String =
      ind(i) ++ params(typs.map(_.toString),ln=false)

  case class TTupleLn(typs:List[TExp]) extends TExp:
    def toCode(implicit i:Int):String =
      ind(i) ++ //params(typs.map(_.toCode(i+1)),ln = true)(0)
        typs.map(a=>a.toCode(i+1)).mkString(s"(\n",",\n",s"\n${ind(i)})")

  case class TFun(from:TExp,to:TExp) extends TExp:
    def toCode(implicit i: Int): String =
      ind(i) ++ from.toString ++ " => " ++ to.toString

  trait GroundTerm extends Statement

  case class Variable(name:String) extends GroundTerm:
    def toCode(implicit i:Int):String = ind(i) ++ name

  case class Tuple(elems:List[Statement]) extends GroundTerm:
    def toCode(implicit i:Int):String = ind(i) ++
      params(elems.map(_.toString))(i+1)

  case class Constructor(name:String, args:List[GroundTerm]=Nil) extends GroundTerm:
    def toCode(implicit i:Int):String = ind(i) ++
      name ++ params(args.map(_.toString))(i+1)

  case class Match(matchVars:List[String],cases:List[Case]) extends Statement:
    def toCode(implicit i: Int): String =
      ind(i) ++ s"""${params(matchVars, ln = false)} match\n""" ++
        cases.map(c=>c.toCode(i+1)).mkString("\n")

  // Case
//  case class Case(
//    pattern:List[String],
//    patternTyp:List[String],
//    output:Statement,
//    comment:Option[String] = None
//  )
  case class Case(
    pattern:List[String],//GroundTerm,
    patternTyp:List[String],//TExp],
    output:Statement,
    comment:Option[String] = None
  ) extends Statement:

//    def toCode(implicit i:Int):String =
//      ind(i) ++ s"""case ${pattern.toString}${mkPatternTExp} =>\n""" ++
//        output.toCode(i+1)
//
//    def mkPatternTExp(implicit i:Int):String =
//      if patternTyp.isDefined then ": " ++ patternTyp.get.toString else ""

      def toCode(implicit i:Int):String =
        val com = if comment.isDefined then singleComment(comment.get)+"\n" else ""
        com ++ ind(i) ++ s"""case ${params(pattern,ln = false)}${mkPatternT()} =>\n""" ++
          output.toCode(i+1)

      def mkPatternT():String =
        if patternTyp.nonEmpty then
          s":${params(patternTyp,ln = false)(0)}"
        else ""

  // Match Type
  case class MatchTyp(name:String, typVars: List[(String,Option[String])], cases:List[MatchTypCase]) extends Statement:
    def toCode(implicit i:Int):String =
      s"""${ind(i)}type $name${brackets(mkTVars())(i+1)} = ${typVars.map(_._1).mkString("(",",",")")} match
         |${cases.map(c=>c.toCode(i+1)).mkString("\n")}""".stripMargin

    def mkTVars():List[String] =
      typVars.map(t =>
        if t._2.isDefined then
          s"${t._1} <: ${t._2.get}"
        else t._1
      ).toList


  case class MatchTypCase(pattern:List[String], output:TExp,comment:Option[String] = None) extends Code:
//    def toCode(implicit i:Int):String =
//      ind(i) ++ s"""case ${pattern.toCode} => $output"""
    def toCode(implicit i:Int):String =
      val com = if comment.isDefined then singleComment(comment.get)+"\n" else ""
      com ++ ind(i) ++ s"""case ${params(pattern,ln = false)} => $output"""
//  case class MatchTypCase(pattern:List[String], output:TExp) extends Code:
//    def toCode(implicit i:Int):String =
//      ind(i) ++ s"""case ${params(pattern,ln = false)} => $output"""


  case class Evidence(evidence:Map[String,String]) extends Code:
    def toCode(implicit i: Int): String =
      if evidence.nonEmpty then
        ind(i) ++
          showEv(evidence.keys.toList) ++
          "<:<" ++ showEv(evidence.values.toList)
      else ""

    protected def showEv(args:List[String]):String =
      if args.size == 1 then args.head
      else args.mkString("(",",",")")

    def updKeysPrefix(prefix:String):Evidence =
      Evidence(evidence.map(e=>(prefix++e._1,e._2)))


  case class IfThenElse(cond:Statement,ok:Statement,ko:Option[Statement]=None) extends Statement:
    def toCode(implicit i: Int): String =
      val ifStr =
        ind(i) + "if " + cond.toString + " then\n" + ok.toCode(i+1)
      val elseStr =
        if ko.isDefined then
          "\n" + ind(i) + "else\n" + ko.get.toCode(i+1)
        else ""
      ifStr + elseStr

  case class TryCatch(trySt:Statement,catchSt:Statement) extends Statement:
    def toCode(implicit i: Int): String =
      ind(i) + "try {\n" +
        trySt.toCode(i+1) + "\n" +
        ind(i) + "} catch {\n" +
        catchSt.toCode(i+1) + "\n" +
        ind(i) + "}"

//  case class ITE(cond:Statement,ok:Statement,ko:Statement) extends Statement:
//    def toCode(implicit i: Int): String =
//        ind(i) + "ifThenElse(\n" +
//          cond.toCode(i+1) + ",\n" +
//          ok.toCode(i+1) + ",\n" +
//          ko.toCode(i+1) + "\n" +
//          ind(i+1) + ")"
//
//  case class ITEType(cond:TExp,ok:TExp,ko:TExp) extends TExp:
//    def toCode(implicit i: Int): String =
//      ind(i) + "IfThenElse[\n" +
//        cond.toCode(i+1) + ",\n" +
//        ok.toCode(i+1) + ",\n" +
//        ko.toCode(i+1) + "\n" +
//        ind(i+1) + "]"
package choreo.gen


import choreo.gen.NPom2SessionCtx.Event2Value
import choreo.api.MiniScala.*
import choreo.gen.EventsCtx.EventCtx
import choreo.syntax.Choreo.In
import choreo.gen.SessionAPI.{chanName, mkPattern, msgName, sbj, roleName}

import scala.collection.immutable.HashMap

case class LocalMethod(methodDef:MethodDef,matchTypeDef:MatchTyp):
  def getStatements():Statement = Statements(matchTypeDef::methodDef::Nil)

object LocalMethod:

  def localRecvFrom(ctx: RoleLocalCtx):LocalMethod = mkRecv(ctx)
  def localSendFrom(ctx: RoleLocalCtx):LocalMethod = mkSend(ctx)

  def mkMethodName(ctx:RoleLocalCtx)(implicit builder:MethodBuilder):String =
    s"${ctx.name}$$"+builder.name

  def mkMatchTypeName(ctx:RoleLocalCtx)(implicit builder: MethodBuilder):String =
    s"""${ctx.name}$$${builder.name.capitalize}"""

  // recipient variable
  protected lazy val rtype = "P"
  // message variable
  protected lazy val m = "m"
  protected lazy val mtype = "M"
  // state variable
  protected lazy val x = "x"
  protected lazy val xtype = "X"

  protected lazy val tVariables = xtype::rtype::mtype::Nil

  protected def mkRecv(ctx: RoleLocalCtx):LocalMethod =
    implicit val builder:MethodBuilder = RecvBuilder
    LocalMethod(mkMethod(ctx),mkMatchType(ctx))

  protected def mkSend(ctx: RoleLocalCtx):LocalMethod =
    implicit val builder:MethodBuilder = SendBuilder
    LocalMethod(mkMethod(ctx),mkMatchType(ctx))

  protected def mkMethod(ctx:RoleLocalCtx)(implicit builder:MethodBuilder):MethodDef =
    val name          = mkMethodName(ctx)
    val typeVariables = tVariables      // State / Participant / Message
    val parameters    = mkParameters
    val evidence      = Set[Evidence]() // no evidence at the local level
    val returnType    = mkType(ctx)
    val body          = mkBody(ctx)
    val comment       = mkComment(ctx)
    MethodDef(name, typeVariables, parameters, evidence, Some(returnType), body, Some(comment))

  protected def mkParameters(implicit builder:MethodBuilder): List[Param] =
    List(Param(x,TName(xtype)),Param(builder.roleParam, TName(rtype)), Param(m, TName(mtype)))

  protected def mkType(ctx: RoleLocalCtx)(implicit builder:MethodBuilder):TExp =
    TName(mkMatchTypeName(ctx),Some(tVariables))

  protected def mkBody(ctx: RoleLocalCtx)(implicit builder: MethodBuilder):Match =
    val matchElements = x::builder.roleParam::m::Nil // state / participant / message
    Match(matchElements, mkCases(ctx))

  protected def mkCases(ctx:RoleLocalCtx)(implicit builder: MethodBuilder):List[Case] =
    val cases = for pre <- builder.getEventsCtx(ctx) yield mkCase(pre,ctx)
    cases :+ mkEndCase()

  protected def mkCase(eventCtx:EventCtx,ctx:RoleLocalCtx):Case =
    val (stateValues, commValues) = mkCaseExpectedValues(eventCtx, ctx)
    // case expected pattern and type
    val casePattern = mkCasePattern(stateValues,commValues)
    val caseType    = mkCasePatternType(stateValues,commValues)
    // return expression if it matches
    val returnSt = mkCaseReturnSt(eventCtx,ctx)
    val comment  = mkCaseComment(eventCtx)
    Case(casePattern,caseType,returnSt,Some(comment))

  protected def mkCaseComment(eventCtx:EventCtx):String =
    s"""// ${eventCtx.act.toString}"""

  protected def mkCaseExpectedValues(eventCtx:EventCtx,ctx:RoleLocalCtx):(List[String],List[String]) =
    val defaultStValue  = ctx.event2Param//ctx.events.map(e=>e->"_").toMap
    val stateValues     = mkPattern(ctx.events,eventCtx.pre,defaultStValue)
    val commValues      = roleName(sbj(eventCtx.act))::msgName(eventCtx.act)::Nil
    (stateValues, commValues)

  protected def mkCasePattern(stateValues:List[String],commValues:List[String]):List[String] = //GroundTerm =
    val statePattern  = stateValues.map(v=>Variable(v))
    val sendPattern   = commValues.map(v=>Variable(v))
    val casePattern   = Tuple(statePattern)::sendPattern
    casePattern.map(_.toString)

  protected def mkCasePatternType(stateValues:List[String],commValues:List[String]):List[String] = //TExp =
    val stateType = stateValues.map(v=>TName(v))
    val commType  = commValues.map(v=>TName(v))
    val caseType  = TTuple(stateType)::commType
    caseType.map(_.toString)

//  protected def mkCase(eventCtx:EventCtx,ctx:LocalCtx):Case =
//    val default       = ctx.events.map(e=>e->"_").toMap
//    val classPattern  = mkPattern(ctx.events,eventCtx.pre,default)
//    val returnSt      = mkCaseReturnSt(eventCtx,ctx)
//    Case(
//      classPattern ++ List("_", "_"),
//      classPattern ++ List(passiveRole(eventCtx.act), msgName(eventCtx.act)),
//      returnSt
//    )

  protected def mkEndCase():Case =
    Case("_"::Nil, "Any"::Nil, FunCall("Error",Nil,Nil))

  protected def mkCaseReturnSt(eventCtx: EventCtx,ctx: RoleLocalCtx):Statement =
    val newClassArgs = mkPattern(ctx.events,eventCtx.post,ctx.event2Param)
    Tuple(newClassArgs.map(Variable(_)))

  protected def mkMatchType(ctx:RoleLocalCtx)(implicit builder: MethodBuilder):MatchTyp =
    val name              = mkMatchTypeName(ctx)
    val tVars2TypeBounds  = tVariables.map(t=>(t,None))
    val cases             = mkMTCases(ctx)
    val endCase           = MatchTypCase("Any"::Nil, TName("Error"))
    MatchTyp(name, tVars2TypeBounds, cases:+endCase)

  protected def mkMTCases(ctx:RoleLocalCtx)(implicit builder: MethodBuilder):List[MatchTypCase] =
    for evCtx <- builder.getEventsCtx(ctx) yield mkMTCase(evCtx,ctx)

  protected def mkMTCase(eventCtx:EventCtx,ctx:RoleLocalCtx):MatchTypCase =
    val (stateValues, commValues) = mkCaseExpectedValues(eventCtx, ctx)
    // case expected pattern and type
    val caseType    = mkCasePatternType(stateValues,commValues)
    // return type expression if it matches
    val returnTExp  = mkMatchTypeCaseReturnTExp(eventCtx,ctx)
    MatchTypCase(caseType,returnTExp)

  protected def mkMatchTypeCaseReturnTExp(eventCtx: EventCtx,ctx: RoleLocalCtx):TExp =
    val returnTypeNames = mkPattern(ctx.events,eventCtx.post,ctx.event2Param)
    val returnTypes = returnTypeNames.map(t=>TName(t))
    TTuple(returnTypes)

  protected def mkComment(ctx:RoleLocalCtx)(implicit builder: MethodBuilder):String =
    val participant = ctx.agent.s
    val pomset      = ctx.name
    val actions     = builder.getEventsCtx(ctx).map(r=> (r.e.toString,r.act.toString))
    val method      = if builder.name == "recv" then "Receive" else "Send"
    s"""$method method for participant $participant in Local Pomset [[${ctx.name}]]
       |Captures the behaviour of the following actions:
       |${actions.mkString(",")}""".stripMargin

  sealed trait MethodBuilder:
    def name:String
    def roleParam:String
    def getEventsCtx: RoleLocalCtx => List[EventCtx]

  object SendBuilder extends MethodBuilder:
    def name:String = "send"
    def roleParam:String = "to"
    def getEventsCtx: RoleLocalCtx => List[EventCtx] = l => l.getAllSendEventsCtx

  object RecvBuilder extends MethodBuilder:
    def name:String = "recv"
    def roleParam:String = "from"
    def getEventsCtx: RoleLocalCtx => List[EventCtx] = l => l.getAllRecvEventsCtx
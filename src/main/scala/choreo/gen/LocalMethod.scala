package choreo.gen


import choreo.api.GlobalCtx.Event2Value
import choreo.api.MiniScala.*
import choreo.gen.EventsCtx.EventCtx
import choreo.syntax.Choreo.In
import choreo.gen.Session.{mkPattern, msgName, passiveRole, roleName}

import scala.collection.immutable.HashMap

case class LocalMethod(methodDef:MethodDef,matchTypeDef:MatchTyp)

object LocalMethod:

  def localRecvFrom(ctx: LocalCtx):LocalMethod = mkRecv(ctx)
  def localSendFrom(ctx: LocalCtx):LocalMethod = mkSend(ctx)

  // participant variable
  protected lazy val p = "p"
  // message variable
  protected lazy val m = "m"

  protected def mkRecv(ctx: LocalCtx):LocalMethod =
    implicit val builder:MethodBuilder = RecvBuilder
    LocalMethod(mkMethod(ctx),mkMatchType(ctx))

  protected def mkSend(ctx: LocalCtx):LocalMethod =
    implicit val builder:MethodBuilder = SendBuilder
    LocalMethod(mkMethod(ctx),mkMatchType(ctx))

  protected def mkMethod(ctx:LocalCtx)(implicit builder:MethodBuilder):MethodDef =
    MethodDef(
      builder.name,
      List("P","M"),         // Participant/Message
      mkParameters(),
      Set(),                // no evidence at the option level
      Some(mkType(ctx)),
      mkBody(ctx),
      Some(mkComment(ctx))
    )

  protected def mkParameters(): List[Param] =
    List(Param(p, TName("P")), Param(m, TName("M")))

  protected def mkType(ctx: LocalCtx)(implicit builder:MethodBuilder):TExp =
    TName(mkMatchTypeName(ctx),Some(ctx.typeVariablesName))

  protected def mkMatchTypeName(ctx:LocalCtx)(implicit builder: MethodBuilder):String =
    s"""${ctx.name}$$${builder.name.capitalize}"""

  protected def mkBody(ctx: LocalCtx)(implicit builder: MethodBuilder):Statement =
    Match(ctx.parametersName++List(p, m), mkCases(ctx))

  protected def mkCases(ctx:LocalCtx)(implicit builder: MethodBuilder):List[Case] =
    val cases = for pre <- builder.getEventsCtx(ctx) yield mkCase(pre,ctx)
    cases :+ mkEndCase()

  protected def mkCase(eventCtx:EventCtx,ctx:LocalCtx):Case =
    val default       = ctx.events.map(e=>e->"_").toMap
    val classPattern  = mkPattern(ctx.events,eventCtx.pre,default)
    val returnSt      = mkCaseReturnSt(eventCtx,ctx)
    Case(
      classPattern ++ List("_", "_"),
      classPattern ++ List(passiveRole(eventCtx.act), msgName(eventCtx.act)),
      returnSt
    )

  protected def mkEndCase():Case =
    Case(List("_"), List("Any"), FunCall("Error",Nil,Nil))

  protected def mkCaseReturnSt(eventCtx: EventCtx,ctx: LocalCtx):Statement =
    val newClassArgs = mkPattern(ctx.events,eventCtx.post,ctx.event2Param)
    FunCall(ctx.name,Nil,newClassArgs)

  protected def mkMatchType(ctx:LocalCtx)(implicit builder: MethodBuilder):MatchTyp =
    val classTVars = ctx.typeVariables.map(t=>(t.name,Some(t.typeConstraint.toString)))
    MatchTyp(
      mkMatchTypeName(ctx),
      classTVars ++ List(("P",None),("M",None)),
      mkMTCases(ctx)
    )

  protected def mkMTCases(ctx:LocalCtx)(implicit builder: MethodBuilder):List[MatchTypCase] =
    for evCtx <- builder.getEventsCtx(ctx) yield mkMTCase(evCtx,ctx)

  protected def mkMTCase(eventCtx:EventCtx,ctx:LocalCtx):MatchTypCase =
    val default = ctx.events.map(e=>e->"_").toMap
    MatchTypCase(
      mkPattern(ctx.events,eventCtx.pre,default) ++ List("_", "_"),
      mkMatchTypeCaseReturnSt(eventCtx,ctx)
    )

  protected def mkMatchTypeCaseReturnSt(eventsCtx: EventCtx,ctx: LocalCtx):TExp =
    TName(ctx.name,Some(mkPattern(ctx.events,eventsCtx.post,ctx.event2TVar)))

  protected def mkComment(ctx:LocalCtx)(implicit builder: MethodBuilder):String =
    val participant = ctx.agent.s
    val pomset      = ctx.name
    val actions     = builder.getEventsCtx(ctx).map(r=> (r.e.toString,r.act.toString))
    val method      = if builder.name == "recv" then "Receive" else "Send"
    s"""$method method for participant $participant in Local Pomset [[${ctx.name}]]
       |Captures the behaviour of the following actions:
       |${actions.mkString(",")}""".stripMargin

  sealed trait MethodBuilder:
    def name:String
    def getEventsCtx: LocalCtx => List[EventCtx]

  object SendBuilder extends MethodBuilder:
    def name:String = "send"
    def getEventsCtx: LocalCtx => List[EventCtx] = l => l.getAllSendEventsCtx

  object RecvBuilder extends MethodBuilder:
    def name:String = "recv"
    def getEventsCtx: LocalCtx => List[EventCtx] = l => l.getAllRecvEventsCtx

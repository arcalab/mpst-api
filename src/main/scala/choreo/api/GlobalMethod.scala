package choreo.api

import choreo.api.MiniScala.*
import choreo.api.NPom2SessionCtx.{ForkInfo, forkEvent}
import choreo.api.LocalAPI.*
import choreo.api.LocalMethod.*
import choreo.api.SessionAPI.*


case class GlobalMethod(methodDef:MethodDef,methodTypeTDef:Statement):
  def getStatements():Statement = Statements(methodTypeTDef::methodDef::Nil)

object GlobalMethod:

  def apply(ctx: RoleCtx):List[GlobalMethod] =
    var res = List[GlobalMethod]()
    if getNumSends(ctx) > 0 then res :+= GlobalSend(ctx)
    if getNumReceives(ctx) > 0 then res :+= GlobalReceive(ctx)
    res

  // recipient variable
  val rtype = "_P"

  // message variable
  val m = "m"
  val mtype = "_M"

  // receive continuation function name
  val f = "f"
  // in channel name
  val ch = "ch"
  val chanRead = "c"
  val msgRead = "m"
  val sbjRead = "q"

  protected def getNumSends(ctx:RoleCtx):Int =
    ctx.localCtx.map(l=>l.getAllSendEventsCtx.size).sum

  protected def getNumReceives(ctx:RoleCtx):Int =
    ctx.localCtx.map(l=>l.getAllRecvEventsCtx.size).sum


  def mkEvidence(ctx: RoleCtx)(implicit builder:MethodBuilder):(List[Evidence],List[MatchTyp]) =
    val evidenceWithMT = for (lc,i) <- ctx.localCtx.zipWithIndex yield mkEvidence(lc,i+1)
    evidenceWithMT.unzip

  def mkEvidence(lc:RoleLocalCtx,i:Int)(implicit builder:MethodBuilder):(Evidence,MatchTyp) =
    val name          = lc.name+"$Evid$"+builder.name.capitalize
    val typeVar       = stateTVar+i
    // evidence
    val evidenceType  = TName(name,Some(typeVar::Nil))
    val evidence      = Evidence(Map(evidenceType.toString->"true"))
    // match type
    val evidenceMT    = mkEvidenceMTCases(lc)
    val matchType     = MatchTyp(name,(typeVar,None)::Nil,evidenceMT)
    (evidence,matchType)

  def mkEvidenceMTCases(lc:RoleLocalCtx)(implicit builder: MethodBuilder):List[MatchTypCase] =
    val okCases =
      for
        e <- builder.getEventsCtx(lc)
        pattern = mkPattern(lc.events,e.pre,lc.event2Param)
      yield
        MatchTypCase(pattern,TName("true"),Some(mkCaseComment(e)))
    val koCase = MatchTypCase("_"::Nil, TName("false"))
    okCases:+koCase

  def mkStateTVars(ctx:RoleCtx):List[String] =
    for i <- (1 to ctx.localCtx.size).toList yield stateTVar+i

  def mkStateVars(ctx:RoleCtx):List[String] =
    for i <- (1 to ctx.localCtx.size).toList yield stateVar+i

  def mkNextState(ctx:RoleCtx, localSts:List[Statement]):Statement =
    FunCall(ctx.name,Nil,localSts:+Variable("net"))

  def mkITE(calls:List[Statement],returnSt:Statement):Statement =
    val cond = calls.tail.foldLeft[Statement](calls.head)({
      case (acc,call) => and(acc,call)
    })
    ITE(cond,UnitSt,returnSt)

  /* helpers */

  def out(o:InOut):Statement =
    val net   = s"net.${chanName(o)}"
    val args  = net::m::Nil
    FunCall("out",Nil,args.map(Variable(_)))

  def in():Statement =
    val arg = Variable(s"$ch: _*") // hardcoded
    FunCall("in",Nil,arg::Nil)

  def ITE(cond:Statement,ok:Statement,ko:Statement):Statement =
    FunCallLn("ifThenElse",Nil,cond::ok::ko::Nil)

  def ITEType(cond:TExp,ok:TExp,ko:TExp):TExp =
    TNameLn("IfThenElse",cond::ok::ko::Nil)

  def isError(cond:Statement):Statement =
    FunCall("isError",Nil,cond::Nil)

  def isErrorType(cond:TExp):TExp =
    TName("IsError",Some(cond.toString::Nil))

  def and(left:Statement,right:Statement):Statement =
    FunCallLn("and",Nil,left::right::Nil)

  def andType(left:TExp,right:TExp):TExp =
    TNameLn("And",left::right::Nil)

  def tryCatch(st:Statement):Statement =
    TryCatch(st,PreCode("case _: ClassCastException => ()"))

  def simplifyType(list:List[TExp]):TExp =
    TNameLn("Simplify",TTupleLn(list)::Nil)

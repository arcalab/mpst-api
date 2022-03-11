package choreo.api

import choreo.api.GlobalMethod.*
import choreo.api.LocalAPI.*
import choreo.api.LocalMethod.*
import choreo.api.MiniScala.*
import choreo.api.SessionAPI.*

object GlobalSend:

  def apply(ctx: RoleCtx):GlobalMethod =
    val (methodDef, evidMT) = mkSend(ctx)
    val types:List[Statement] = evidMT:+mkSendTypeAlias(ctx)
    GlobalMethod(methodDef,Statements(types))

  /* Send Method */

  protected def mkSend(ctx:RoleCtx):(MethodDef,List[MatchTyp]) =
    val name        = "send"
    val typeVars    = rtype::mtype::Nil // participant / message
    val params      = mkSendParams("to")
    val (evid,eMT)  = mkEvidence(ctx)(SendBuilder)//Set[Evidence]() // todo
    val returnType  = mkSendType(ctx)
    val body        = mkSendBody(ctx)
    val comment     = None // todo mkSendComment(ctx)
    (MethodDef(name, typeVars, params, evid.toSet, Some(returnType), body, comment),eMT)

  protected def mkSendParams(recipient:String):List[Param] =
    Param(recipient,TName(rtype))
      :: Param(m,TName(mtype))
      :: Nil

  protected def mkSendType(ctx: RoleCtx):TExp =
    TName(mkSendTypeName(ctx),Some(mkSendTypeVars(ctx)))

  protected def mkSendBody(ctx:RoleCtx):Statement =
    val doLocalSends  = mkLocalSendCalls(ctx)
    val returnSt      = mkSendReturnSt(ctx,doLocalSends)
    val ifThenElse    = mkITE(doLocalSends.map(s=>isError(s)),returnSt)
    // use; if (send not allow) then () else actual send
    NoSepStatements(use::ifThenElse::Nil)

  protected def mkLocalSendCalls(ctx:RoleCtx):List[Statement] =
    implicit val builder:MethodBuilder = SendBuilder
    for (lc,i) <- ctx.localCtx.zipWithIndex yield
      FunCall(mkMethodName(lc),Nil,Variable(stateVar+(i+1))::Variable("to")::Variable(m)::Nil)

  protected def mkSendReturnSt(ctx:RoleCtx,localSends:List[Statement]):Statement =
    val matchVars = "to"::Nil
    val matchSt   = Match(matchVars,mkSendCases(ctx))
    val nextState = mkNextState(ctx,localSends)//FunCall(className(ctx.name),mkNextState(localSends):+Variables(net))
    BlockStatement(NoSepStatements(matchSt::nextState::Nil))
  protected def mkSendCases(ctx:RoleCtx):List[Case] =
    // get sends across all local options
    val sends:List[InOut] = ctx.localCtx.foldLeft[List[InOut]](List[InOut]())({
      case (acc, lc) => acc++lc.getAllSendEventsCtx.map(e=>e.act)
    })
    val sendsBySbj = sends.groupBy(s=>sbj(s))
    // case for each unique send
    for  (_,send) <- sendsBySbj.toList yield mkSendCase(send.head)

  protected def mkSendCase(act:InOut):Case =
  // case TO => out(net.FROMTO, m)
    Case(roleName(sbj(act))::Nil, Nil, out(act))

  protected def getNumSends(ctx:RoleCtx):Int =
    ctx.localCtx.map(l=>l.getAllSendEventsCtx.size).sum

  /* Send Match Type */

  protected def mkSendTypeAlias(ctx:RoleCtx):TypeDef =
    val name      = mkSendTypeName(ctx)
    val typeVars  = mkSendTypeVars(ctx)
    val typeSt    = mkSendTypeAliasSt(ctx)
    TypeDef(TName(name,Some(typeVars)), typeSt)

  protected def mkSendTypeName(ctx:RoleCtx):String =
    className(ctx.agent)+"$SendReturn"

  protected def mkSendTypeVars(ctx: RoleCtx):List[String] =
    val stateTVars  = mkStateTVars(ctx)
    val sendTVars   = rtype::mtype::Nil
    stateTVars++sendTVars

  protected def mkSendTypeAliasSt(ctx:RoleCtx):TExp =
    // type resturn by each local send
    val locaSendsType = mkTypeOfLocalSends(ctx)
    // condition type to check if send executes well
    val cond = locaSendsType.tail.foldLeft[TExp](isErrorType(locaSendsType.head))({
      case (acc,st) => andType(acc,isErrorType(st))
    })
    // return type if send executes well
    //    val args = locaSendsType.map(_.toString)
    val returnType = TName(ctx.name,Some(locaSendsType.map(_.toString)))
    ITEType(cond,TUnit,returnType)

  protected def mkTypeOfLocalSends(ctx:RoleCtx):List[TExp] =
    implicit val builder:MethodBuilder = SendBuilder
    for (lc,i) <- ctx.localCtx.zipWithIndex yield
      TName(mkMatchTypeName(lc),Some(stateTVar+(i+1)::rtype::mtype::Nil))


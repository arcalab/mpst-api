package choreo.gen

import choreo.api.MiniScala.*
import choreo.gen.LocalMethod.*
import choreo.gen.LocalAPI.*
import choreo.gen.SessionAPI.*


case class GlobalMethod(methodDef:MethodDef,methodTypeTDef:TypeDef):
  def getStatements():Statement = Statements(methodTypeTDef::methodDef::Nil)

object GlobalMethod:

  def apply(ctx: RoleCtx):List[GlobalMethod] =
    var res = List[GlobalMethod]()
    if getNumSends(ctx) > 0 then res :+= sendFrom(ctx)
    if getNumReceives(ctx) > 0 then res :+= recvFrom(ctx)
    res
  protected def recvFrom(ctx: RoleCtx):GlobalMethod =
    GlobalMethod(mkRecv(ctx),TypeDef(TName("Final"),mkEndType(ctx))) //todo end type
  protected def sendFrom(ctx: RoleCtx):GlobalMethod =
    GlobalMethod(mkSend(ctx),mkSendTypeAlias(ctx))


  // recipient variable
  protected val rtype = "P"

  // message variable
  protected val m = "m"
  protected val mtype = "M"

  // use instance
  protected val use = Variable("use")

  // receive continuation function name
  protected val f = "f"
  // in channel name
  protected val ch = "ch"
  protected val chanRead = "c"
  protected val msgRead = "m"
  protected val sbjRead = "q"

  /* Receive Method */

  protected def mkRecv(ctx:RoleCtx):MethodDef =
    val name        = "recv"
    val typeVars    = Nil
    val params      = Param(f,mkRecvArgType(ctx))
    val evidence    = Set[Evidence]() // todo
    val returnType  = mkEndType(ctx)
    val body        = mkRecvBody(ctx)
    val comment     = "" //todo mkRecvComment(ctx)
    MethodDef(name, typeVars, params::Nil, evidence, Some(returnType), body, Some(comment))

  protected def mkRecvArgType(ctx:RoleCtx):TExp =
    TName(mkRecvArgTypeName(ctx),Some(mkStateTVars(ctx)))

  protected def mkRecvArgTypeName(ctx:RoleCtx):String =
    className(ctx.agent)++"$RecvArgument"

//  protected def mkRecvType(ctx:RoleCtx):TExp =
//    TName(className(ctx)+"$Pom",)

  protected def mkStateTVars(ctx:RoleCtx):List[String] =
    for i <- (1 to ctx.localCtx.size).toList yield stateTVar+i

  protected def mkStateVars(ctx:RoleCtx):List[String] =
    for i <- (1 to ctx.localCtx.size).toList yield stateVar+i

  protected def mkRecvBody(ctx:RoleCtx):Statement =
    val matchSt = mkRecvMatch(ctx)
    val endSt   = mkEndInstance(ctx)
    NoSepStatements(use::matchSt::endSt::Nil)

  protected def mkEndInstance(ctx: RoleCtx):Statement =
//    PreCode(s"s.asInstanceOf[${mkEndType(ctx)}")
    // todo: if fork is different.
    mkDefaultFinalSt(ctx)

  protected def mkDefaultFinalSt(ctx:RoleCtx):Statement =
    val args =
      for lc <- ctx.localCtx yield
          Tuple(for i <- (1 to lc.eventsCtx.size).toList yield Variable("false"))
    FunCall(ctx.name, Nil, args)

  protected def mkRecvMatch(ctx:RoleCtx):Statement =
    val matchVars               = mkStateVars(ctx)
    val (casePattern, caseTExp) = mkRecvCasePattern(ctx)
    val caseReturn              = mkRecvCaseReturn(ctx)
    Match(matchVars,Case(casePattern,caseTExp,caseReturn,None)::Nil)

  protected def mkRecvCasePattern(ctx:RoleCtx):(List[String],List[String]) =
    val patternWithTExp =
      for (lc,i) <- ctx.localCtx.zipWithIndex yield
        val res =
          (for  e <- lc.eventsCtx yield
            (Variable(stateVar+(i+1)+e.param), TName("Boolean"))) ++
            (for  e <- lc.extEventsCtx yield
              (Variable(stateVar+(i+1)+e.param), TName("Int")))
//            (stateVar+i+e.param, "Boolean")
        res.unzip
    val (gt,te) = patternWithTExp.unzip
    (gt.map(l=>Tuple(l).toString),te.map(l=>TTuple(l).toString))

  protected def mkRecvCaseReturn(ctx:RoleCtx):Statement =
    val channelDef      = PreCode(s"var $ch:Seq[Channel] = Seq()")
    val channels2Read   = mkChannels2Read(ctx)
    val readMatch       = mkRecvMatchOnInput(ctx)
    val endSt           = TName(className(ctx.agent),None) // todo
    Statements(channelDef::channels2Read::readMatch::Nil)

  // if certain actions enabled => corresponding channel to read
  protected def mkChannels2Read(ctx:RoleCtx):Statement =
    val channelWithRequiredVariable =
      for
        (lc,i) <- ctx.localCtx.zipWithIndex
        recv <- lc.getAllRecvEventsCtx
      yield
        (chanName(recv.act), stateVar+(i+1)+recv.param)
    // variables that need to be true to read a channel (relaxed check)
    val variablesByChannel = channelWithRequiredVariable.groupMap(_._1)(_._2)
    // add channel to channels to read if certain variables are true
    val channels2Read =
      for
        (chan,vars) <- variablesByChannel
        condStr = vars.mkString(" || ")
      yield
        IfThenElse(PreCode(condStr),PreCode(s"$ch = $ch :+ $chan"))
    // all if then elses
    NoSepStatements(channels2Read.toList)

  protected def mkRecvMatchOnInput(ctx:RoleCtx):Match =
    val readSbjDef      = PreCode(s"var $sbjRead:Any = null")
    val instantiateSbj  = mkInstantiateReadSbj(ctx)
    val tryCatchRead    = mkTryCatchRead(ctx)
    val caseSt          = NoSepStatements(readSbjDef::instantiateSbj::tryCatchRead::Nil)
    // case (c,m)  => // c channel read, m message read
    val caseRead = Case(chanRead::msgRead::Nil,Nil,caseSt)
    Match(in().toString::Nil,caseRead::Nil)

  protected def mkInstantiateReadSbj(ctx:RoleCtx):Statement =
    val recvs = ctx.localCtx.foldLeft[List[InOut]](List[InOut]())({
        case (acc, lc) => acc++lc.getAllSendEventsCtx.map(e=>e.act)
      })
    val instantiateReadSbj =
      for
        r     <- recvs.distinct
        cond  = PreCode(s"$chanRead == net.${chanName(r)}")
        ok    = PreCode(s"$sbjRead = ${roleName(sbj(r))}")
      yield
        IfThenElse(cond,ok)
    NoSepStatements(instantiateReadSbj)

  protected def mkTryCatchRead(ctx: RoleCtx):Statement =
    val singleFun = mkTryCatchFun(ctx)
    val tupleFun  = mkTryCatchTuple(ctx)
    NoSepStatements(tryCatch(singleFun)::tryCatch(tupleFun)::Nil)

  protected def mkTryCatchFun(ctx:RoleCtx):Statement =
    val name = s"$f.asInstanceOf[Cont]"
    val nextLocalStates = mkLocalRecvCalls(ctx)
    val nextState = mkNextState(ctx,nextLocalStates)
    val args = Variable(sbjRead)::Variable(msgRead)::nextState::Nil
    val fun = FunCall(name,Nil,args)
    val asign = Asign("s",fun.toString) //todo change asign class definition
    asign

  protected def mkTryCatchTuple(ctx:RoleCtx):Statement =
    val code =
      s"""$f.asInstanceOf[Tuple]
         |  .productIterator
         |  .foreach(
         |    ${mkTryCatchFun(ctx)}
         |  )""".stripMargin
    tryCatch(PreCode(code))

  protected def mkLocalRecvCalls(ctx:RoleCtx):List[Statement] =
    implicit val builder:MethodBuilder = RecvBuilder
    for (lc,i) <- ctx.localCtx.zipWithIndex yield
      FunCall(mkMethodName(lc),Nil,Variable(stateVar+(i+1))::Variable(sbjRead)::Variable(msgRead)::Nil)

  /* Send Method */

  protected def mkSend(ctx:RoleCtx):MethodDef =
    val name        = "send"
    val typeVars    = rtype::mtype::Nil // participant / message
    val params      = mkSendParams("to")
    val evidence    = Set[Evidence]() // todo
    val returnType  = mkSendType(ctx)
    val body        = mkSendBody(ctx)
    val comment     = "" // todo mkSendComment(ctx)
    MethodDef(name, typeVars, params, evidence, Some(returnType), body, Some(comment))

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

  protected def mkNextState(ctx:RoleCtx, localSts:List[Statement]):Statement =
    FunCall(className(ctx.agent),Nil,localSts:+Variable("net"))

  protected def mkSendCases(ctx:RoleCtx):List[Case] =
    // get sends across all local options
    val sends:List[InOut] = ctx.localCtx.foldLeft[List[InOut]](List[InOut]())({
      case (acc, lc) => acc++lc.getAllSendEventsCtx.map(e=>e.act)
    })
    // case for each unique send
    for  send <- sends.distinct yield mkSendCase(send)

  protected def mkSendCase(act:InOut):Case =
    // case TO => out(net.FROMTO, m)
    Case(roleName(sbj(act))::Nil, Nil, out(act))

  protected def mkITE(calls:List[Statement],returnSt:Statement):Statement =
    val cond = calls.tail.foldLeft[Statement](calls.head)({
      case (acc,call) => and(acc,call)
    })
    ITE(cond,UnitSt,returnSt)

  protected def getNumSends(ctx:RoleCtx):Int =
    ctx.localCtx.map(l=>l.getAllSendEventsCtx.size).sum

  protected def getNumReceives(ctx:RoleCtx):Int =
    ctx.localCtx.map(l=>l.getAllRecvEventsCtx.size).sum

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
    val cond = locaSendsType.tail.foldLeft[TExp](locaSendsType.head)({
      case (acc,st) => andType(acc,isErrorType(st))
    })
    // return type if send executes well
    val returnType = TName(className(ctx.agent),Some(locaSendsType.map(_.toString)))
    ITEType(cond,TUnit,returnType)

  protected def mkTypeOfLocalSends(ctx:RoleCtx):List[TExp] =
    implicit val builder:MethodBuilder = RecvBuilder
    for (lc,i) <- ctx.localCtx.zipWithIndex yield
      TName(mkMatchTypeName(lc),Some(stateVar+(i+1)::rtype::mtype::Nil))

  /* helpers */

  protected def out(o:InOut):Statement =
    val net   = s"net.${chanName(o)}"
    val args  = net::m::Nil
    FunCall("out",Nil,args.map(Variable(_)))

  protected def in():Statement =
    val arg = Variable(s"$ch: _*") // hardcoded
    FunCall("in",Nil,arg::Nil)

  protected def ITE(cond:Statement,ok:Statement,ko:Statement):Statement =
    FunCallLn("ifThenElse",Nil,cond::ok::ko::Nil)

  protected def ITEType(cond:TExp,ok:TExp,ko:TExp):TExp =
    TNameLn("IfThenElse",cond::ok::ko::Nil)

  protected def isError(cond:Statement):Statement =
    FunCall("isError",Nil,cond::Nil)

  protected def isErrorType(cond:TExp):TExp =
    TName("isError",Some(cond.toString::Nil))

  protected def and(left:Statement,right:Statement):Statement =
    FunCallLn("and",Nil,left::right::Nil)

  protected def andType(left:TExp,right:TExp):TExp =
    TNameLn("And",left::right::Nil)

  protected def tryCatch(st:Statement):Statement =
    TryCatch(st,PreCode("case _: ClassCastException => ()"))



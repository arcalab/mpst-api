package choreo.api

import choreo.api.GlobalMethod.* //{ITEType, andType, ch, chanRead, f, in, isErrorType, mkDefaultFinalSt, mkNextState, mkRecvBody, mkRecvMatch, mkStateTVars, mkStateVars, msgRead, sbjRead, simplifyType, tryCatch}
import choreo.api.LocalAPI.{mkEndType, stateTVar, stateVar, use}
import choreo.api.LocalMethod.{MethodBuilder, RecvBuilder, mkMatchTypeName, mkMethodName, mkRecv}
import choreo.api.MiniScala.{Asign, Case, Evidence, FunCall, IfThenElse, Match, MatchTyp, MatchTypCase, MethodDef, NoSepStatements, Param, PreCode, Statement, Statements, TExp, TFun, TName, TTuple, TUnit, Tuple, TypeDef, Variable}
import choreo.api.NPom2SessionCtx.forkEvent
import choreo.api.SessionAPI.{InOut, chanName, className, mkPattern, msgName, roleName, sbj}

object GlobalReceive :

  def apply(ctx: RoleCtx):GlobalMethod =
    val (methodDef,evidMT) = mkRecv(ctx)
    val types:List[Statement] = evidMT++(mkRecvArgTypeDef(ctx)::mkRecvTypeAlias(ctx)::Nil)
    GlobalMethod(methodDef,
      Statements(types)
    )

  /* Receive Method */

  protected def mkRecv(ctx:RoleCtx):(MethodDef,List[MatchTyp]) =
    val name        = "recv"
    val typeVars    = Nil
    val params      = Param(f,mkRecvArgType(ctx))
    val (evid,eMT)  = mkEvidence(ctx)(RecvBuilder)//Set[Evidence]() // todo
    val returnType  = mkRecvType(ctx)
    val body        = mkRecvBody(ctx)
    val comment     = None //todo mkRecvComment(ctx)
    (MethodDef(name, typeVars, params::Nil, evid.toSet, Some(returnType), body, comment),eMT)

  protected def mkRecvArgType(ctx:RoleCtx):TExp =
    TName(mkRecvArgTypeName(ctx),Some(mkStateTVars(ctx)))

  protected def mkRecvArgTypeName(ctx:RoleCtx):String =
    className(ctx.agent)++"$RecvArgument"

  protected def mkRecvType(ctx:RoleCtx):TExp =
    if ctx.forkes then
      TName(ctx.name,Some(TName(mkRecvTypeName(ctx),mkRecvTypeVars(ctx)).toString::Nil))
    else
      mkEndType(ctx)

  protected def mkRecvTypeName(ctx: RoleCtx):String =
    if ctx.forkes then
      className(ctx.agent)+"$Pom1$Final"
    else
      ctx.name/*className(ctx.agent)*/+"$Final"

  protected def mkRecvTypeVars(ctx:RoleCtx):Option[List[String]] =
    if ctx.forkes then
      Some(List(stateTVar+1))
    else
      None

  protected def mkRecvTypeAlias(ctx:RoleCtx):Statement =
    if ctx.forkes then
      mkRecvTypeFinal(ctx)
    else
      val name      = mkRecvTypeName(ctx)
      val typeVars  = mkRecvTypeVars(ctx)
      val typeSt    = mkRecvTypeAliasSt(ctx)
      TypeDef(TName(name,typeVars), typeSt)

  //  protected def mkRecvTypeAliasSt(ctx:RoleCtx):TExp =
  //    val vars =
  //      for lc <- ctx.localCtx yield
  //        TTuple(lc.events.map(e=>TName("false")))
  //    TName(ctx.name,Some(vars.map(_.toString)))
  protected def mkRecvTypeAliasSt(ctx:RoleCtx):TExp =
    val vars =
      for lc <- ctx.localCtx yield
        TTuple(lc.events.map(e=>TName("false")))
    val options =
      for i <- ctx.localCtx.indices.toList yield
        for j <- ctx.localCtx.indices.toList yield
          val res:TExp = if j == i then vars(i) else TName("_")
          res
    val optionTypes = options.map(o=>TName(ctx.name,Some(o.map(t=>t.toString))))
    PreCode(optionTypes.mkString(" | "))
  //    TName(ctx.name,Some(vars.map(_.toString)))


  protected def mkRecvTypeFinal(ctx:RoleCtx):Statement =
    val lc = ctx.localCtx.head  // forks, so only one
    val forkJoin = lc.forkJoin.get
    val defaultValues = lc.event2Param
    val allFalse = lc.events.map(e=>e->"false").toMap
    val regionsIds =
      for
        f <- forkJoin.forks
        r <- f.post
      yield
        r.id.toString
    val cases =
      for
        rid <- regionsIds.distinct
        pattern = mkPattern(lc.events,Map(forkEvent->rid),defaultValues)
        out = mkPattern(lc.events,Map(forkEvent->rid),allFalse)
      yield
        MatchTypCase(pattern,TTuple(out.map(o=>TName(o))))
    val typeVars = mkStateTVars(ctx).map(t => (t,Option.empty[String]))
    val noForkCase = MatchTypCase(
      mkPattern(lc.events,Map(forkEvent->"0"),defaultValues),
      TTuple(mkPattern(lc.events,Map(forkEvent->"0"),allFalse).map(o=>TName(o)))
    )
    MatchTyp(mkRecvTypeName(ctx),typeVars,cases:+noForkCase)


  protected def mkRecvArgTypeDef(ctx:RoleCtx):TypeDef =
    val name      = mkRecvArgTypeName(ctx)
    val typeVars  = mkStateTVars(ctx)
    val typeSt    = mkRecvArgTypeSimplify(ctx)
    TypeDef(TName(name,Some(typeVars)), typeSt)

  protected def mkRecvArgTypeSimplify(ctx:RoleCtx):TExp =
    val args = mkRecvArgTypeSimplifyITEs(ctx)
    simplifyType(args)

  protected def mkRecvArgTypeSimplifyITEs(ctx:RoleCtx):List[TExp] =
    val recvs:List[InOut] = ctx.localCtx.foldLeft[List[InOut]](List[InOut]())({
      case (acc, lc) => acc++lc.getAllRecvEventsCtx.map(e=>e.act)
    })
    val recvBySbjMsg = recvs.groupBy(r=>(sbj(r),msgName(r)))
    val uniqueRecvs = recvBySbjMsg.map(r=>r._2.head)
    // types of local receives
    val recvsType = for r <- uniqueRecvs yield (r->mkTypeOfLocalRecv(r,ctx))
    // ITEs

    for (r,rt) <- recvsType.toList yield //recvs.zip(recvsType) yield
      mkRecvArgTypeSimplifyITE(r,rt,ctx)

  protected def mkRecvArgTypeSimplifyITE(r:InOut,localRecvTypes:List[TExp],ctx:RoleCtx):TExp =
    val cond = localRecvTypes.tail.foldLeft[TExp](isErrorType(localRecvTypes.head))({
      case (acc,st) => andType(acc,isErrorType(st))
    })
    ITEType(cond,TUnit,mkRecvContFunType(r,localRecvTypes,ctx))

  protected def mkTypeOfLocalRecv(r:InOut, ctx:RoleCtx):List[TExp] =
    implicit val builder:MethodBuilder = RecvBuilder
    for (lc,i) <- ctx.localCtx.zipWithIndex yield
      TName(mkMatchTypeName(lc),Some(stateTVar+(i+1)::roleName(sbj(r))::msgName(r)::Nil))

  protected def mkRecvContFunType(r:InOut,localRecvType:List[TExp],ctx:RoleCtx):TExp =
    val role  = TName(roleName(sbj(r)))
    val msg   = TName(msgName(r))
    val nextState =  TName(ctx.name,Some(localRecvType.map(_.toString)))
    TFun(TTuple(role::msg::nextState::Nil), mkRecvType(ctx))

  protected def mkRecvBody(ctx:RoleCtx):Statement =
    val matchSt = mkRecvMatch(ctx)
    NoSepStatements(use::matchSt::Nil)

  protected def mkEndInstance(ctx: RoleCtx):Statement =
    if ctx.forkes then
      PreCode(s"s.asInstanceOf[${mkRecvType(ctx)}]")
    else mkDefaultFinalSt(ctx)

  protected def mkDefaultFinalSt(ctx:RoleCtx):Statement =
    val args =
      for lc <- ctx.localCtx yield
        Tuple(for i <- (1 to lc.eventsCtx.size).toList yield Variable("false"))
    FunCall(ctx.name, args.map(_.toString), args:+Variable("net"))

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
    val funDef          = PreCode("var s:Any = null")
    val readMatch       = mkRecvMatchOnInput(ctx)
    //    val endSt           = TName(className(ctx.agent),None) // todo
    val endSt           = mkEndInstance(ctx)
    Statements(channelDef::channels2Read::funDef::readMatch::endSt::Nil)

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
        IfThenElse(PreCode(condStr),PreCode(s"$ch = $ch :+ net.$chan"))
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
    val recvs = ctx.localCtx.foldLeft[List[(String,String)]](List())({
      case (acc, lc) => acc++lc.getAllRecvEventsCtx
        .map(e=>(chanName(e.act),roleName(sbj(e.act))))
    })
    val recvsByCh =  recvs.groupMap(_._1)(_._2)
    val instantiateReadSbj =
      for
        (ch,r) <- recvsByCh.toList //recvs.distinct
        cond  = PreCode(s"$chanRead == net.$ch")
        ok    = PreCode(s"$sbjRead = ${r.head}")
      yield
        IfThenElse(cond,ok)
    NoSepStatements(instantiateReadSbj)

  protected def mkTryCatchRead(ctx: RoleCtx):Statement =
    val singleFun = mkTryCatchSingleFun(ctx)
    val tupleFun  = mkTryCatchTuple(ctx)
    NoSepStatements(tryCatch(singleFun)::tryCatch(tupleFun)::Nil)

  protected def mkTryCatchSingleFun(ctx:RoleCtx):Statement =
    val name = s"$f.asInstanceOf[Cont]"
    val nextLocalStates = mkLocalRecvCalls(ctx)
    val nextState = mkNextState(ctx,nextLocalStates)
    val args = Variable(sbjRead)::Variable(msgRead)::nextState::Nil
    val fun = FunCall(name,Nil,args)
    val asign = Asign("s",fun.toString) //todo change asign class definition
    asign

  protected def mkTryCatchFun(ctx:RoleCtx):Statement =
    val name = s"fun.asInstanceOf[Cont]"
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
         |  .foreach( fun =>
         |    ${tryCatch(mkTryCatchFun(ctx))}
         |  )""".stripMargin
    PreCode(code)

  protected def mkLocalRecvCalls(ctx:RoleCtx):List[Statement] =
    implicit val builder:MethodBuilder = RecvBuilder
    for (lc,i) <- ctx.localCtx.zipWithIndex yield
      FunCall(mkMethodName(lc),Nil,Variable(stateVar+(i+1))::Variable(sbjRead)::Variable(msgRead)::Nil)


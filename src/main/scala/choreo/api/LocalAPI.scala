package choreo.api

import choreo.api.MiniScala.*
import choreo.api.NPom2SessionCtx.{ForkInfo, JoinInfo, RegionInfo, forkEvent}
import choreo.api.SessionAPI.*

case class LocalAPI(clas:ScalaClass,co:ScalaObject)

object LocalAPI:

  def apply(ctx:RoleCtx):LocalAPI =
    val (clas, types) = mkClass(ctx)
    val co            = mkCO(ctx,types)
    LocalAPI(clas,co)

  val stateVar = "x"
  val stateTVar = "_X"
  val localVar  = "v"

  // use instance
  val use = Variable("use")

  protected def mkClass(ctx: RoleCtx):(ScalaClass,Statement) =
    val name        = ctx.name //className(ctx.agent)
    val typeVars    = mkTVars(ctx)
    val parameters  = mkParams(ctx)
    val globalMethods = GlobalMethod(ctx)
    var methods       = Statements(globalMethods.map(g=>g.methodDef))
    var objStatements   = globalMethods.map(g=>g.methodTypeTDef)
    val fork          = mkFork(ctx)
    if fork.isDefined then
      methods = Statements(methods.statements:+fork.get.methodDef)
      objStatements :+= mkEndTypeDefIfFork(ctx)
      objStatements :+= fork.get.methodTypeTDef
      objStatements ++= mkJoins(ctx)
    val comment     = mkGlobalComment(ctx)
    val extendsWith = "UseOnce"
    val init = mkInitTypeDef(ctx)
    val clas = ScalaClass(name,typeVars,parameters,Some(methods),Some(comment),extendsWith::Nil)
    (clas,Statements(init::objStatements))

  protected def mkGlobalComment(ctx:RoleCtx):String =
    val stateVariables =
      for (lc,i) <- ctx.localCtx.zipWithIndex yield
        stateTVar+(i+1)+": "+mkEventActions(lc).mkString("(",", ",")")

    s"""Main class for participant ${className(ctx.agent)}
       |State variables represent events in each optional Pomset for ${className(ctx.agent)}:
       |${stateVariables.mkString("\n")}""".stripMargin

  protected def mkEventActions(lc:RoleLocalCtx):List[String] =
    val stateVariables = for e <- lc.eventsCtx yield e.param+": "+e.act.toString
    if lc.forkJoin.isDefined then
      stateVariables:+"forkRegion"
    else stateVariables

  protected def mkInitTypeDef(ctx:RoleCtx):TypeDef =
    val name  = TName(ctx.name+"$Init")
    val typVars = mkLocalInitParams(ctx)
    TypeDef(name,TName(ctx.name,Some(typVars.map(_.toString))))

  def mkInitTypeInstance(ctx:RoleCtx):Statement =
    val name  = TName(ctx.name+"$Init")
    val values =
      for lc <- ctx.localCtx yield
        val stateVals = for e <- lc.eventsCtx yield e.initialVal
        val extraVals = for e <- lc.extEventsCtx yield e.initialVal
        Tuple((stateVals++extraVals).map(v=>Variable(v)))
    FunCall(ctx.name,Nil,values++List(Variable("net")))

  protected def mkLocalInitParams(ctx:RoleCtx):List[TExp] =
    for lc <- ctx.localCtx yield
      val stateVals = for e <- lc.eventsCtx yield e.initialVal
      val extraVals = for e <- lc.extEventsCtx yield e.initialVal
      TTuple((stateVals++extraVals).map(v=>TName(v)))

  protected def mkTVars(ctx:RoleCtx):List[(String,Option[String])] =
    for i <- (1 to ctx.localCtx.size).toList yield (stateTVar+i, Option.empty[String])

  protected def mkParams(ctx:RoleCtx):List[Param] =
    val stParams =
      for i <- (1 to ctx.localCtx.size).toList yield
        Param(stateVar+i,TName(stateTVar+i))
    stParams:+Param("net",TName("Network"))

  protected def mkCO(ctx:RoleCtx,typeDefs:Statement):ScalaObject =
    val localMethods  = mkLocalMethods(ctx)
    val statements    = Statements(typeDefs::localMethods.map(l=>l.getStatements()))
    ScalaObject(ctx.name,Some(statements),None,Nil)

  protected def mkLocalMethods(ctx:RoleCtx):List[LocalMethod] =
    ctx.localCtx.foldLeft[List[LocalMethod]](Nil)({
      case (acc,lc) => acc++mkLocalMethods(lc)
    })

  protected def mkLocalMethods(localCtx:RoleLocalCtx):List[LocalMethod] =
    LocalMethod.localSendFrom(localCtx)
      :: LocalMethod.localRecvFrom(localCtx)
      :: Nil

  protected def mkFork(ctx: RoleCtx):Option[GlobalMethod] =
    if ctx.forkes then
      val (method,mt) = mkForkMethod(ctx)
      val typeDef = mkForkTypeDef(ctx)
      Some(GlobalMethod(method,Statements(mt::typeDef::Nil)))
    else
      None

  protected def mkForkMethod(ctx:RoleCtx):(MethodDef,MatchTyp) =
    val (ev,mt) = mkForkEvidence(ctx.localCtx.head)
    val returnType = mkForkType(ctx)
    val st = mkForkSt(ctx)
    (MethodDef("fork",Nil,Nil,Set(ev),Some(returnType),st,None),mt)

  protected def mkForkSt(ctx:RoleCtx):Statement =
    val pattern = stateVar+1.toString::Nil
    val cases = mkForkCases(ctx)
    val matchSt = Match(pattern,cases)
    NoSepStatements(use::matchSt::Nil)

  protected def mkForkCases(ctx:RoleCtx):List[Case] =
    val forkJoin = ctx.localCtx.head.forkJoin.get
    for f <- forkJoin.forks yield mkForkCase(f,ctx)

  protected def mkForkCase(fork:ForkInfo,ctx:RoleCtx):Case =
    val lc = ctx.localCtx.head
    val defaultValues = lc.event2Param
    val pattern = mkPattern(lc.events,fork.pre,defaultValues)
    val outSt = Tuple(fork.post.map(b=>mkBranch(b,ctx)))
    Case(pattern,pattern,outSt,Some(mkCaseComment(fork)))

  protected def mkBranch(r:RegionInfo,ctx:RoleCtx):Statement =
    val lc = ctx.localCtx.head
    val defaultValues = lc.event2Param
    val argValues = mkPattern(lc.events,r.values,defaultValues)
    val args = Tuple(argValues.map(v=>Variable(v)))::Variable("net")::Nil
    FunCall(ctx.name, Nil, args)

  protected def mkForkType(ctx: RoleCtx):TName =
    TName(mkForkTypeName(ctx),Some(mkForkTypeVars()))

  protected def mkForkTypeName(ctx:RoleCtx):String =
    className(ctx.agent)+"$ForkReturn"

  protected def mkForkTypeVars():List[String] =
    (stateTVar+1)::Nil

  protected def mkForkTypeDef(ctx: RoleCtx):MatchTyp =
    val name = mkForkTypeName(ctx)
    val vars = mkForkTypeVars().map(v=>(v,Option.empty[String]))
    val cases = mkForkTypeCases(ctx)
    MatchTyp(name,vars,cases)

  protected def mkForkTypeCases(ctx:RoleCtx):List[MatchTypCase] =
    val forkJoin = ctx.localCtx.head.forkJoin.get
    for f <- forkJoin.forks yield mkForkTypeCase(f,ctx)

  protected def mkForkTypeCase(fork:ForkInfo,ctx:RoleCtx):MatchTypCase =
    val lc = ctx.localCtx.head
    val defaultValues = lc.event2Param
    val pattern = mkPattern(lc.events,fork.pre,defaultValues)
    val outSt = TTuple(fork.post.map(b=>mkBranchType(b,ctx)))
    MatchTypCase(pattern,outSt,Some(mkCaseComment(fork)))

  protected def mkBranchType(r:RegionInfo,ctx:RoleCtx):TName =
    val lc = ctx.localCtx.head
    val defaultValues = lc.event2Param
    val argValues = mkPattern(lc.events,r.values,defaultValues)
    val args = TTuple(argValues.map(v=>TName(v)))
    TName(ctx.name,  Some(args.toString::Nil))

  def mkForkEvidence(ctx:RoleLocalCtx):(Evidence,MatchTyp) =
    val name          = ctx.name+"$Evid$Fork"
    val typeVar       = stateTVar+1
    // evidence
    val evidenceType  = TName(name,Some(typeVar::Nil))
    val evidence      = Evidence(Map(evidenceType.toString->"true"))
    // match type
    val evidenceMT    = mkForkEvidenceMTCases(ctx)
    val matchType     = MatchTyp(name,(typeVar,None)::Nil,evidenceMT)
    (evidence,matchType)

  def mkForkEvidenceMTCases(lc:RoleLocalCtx):List[MatchTypCase] =
    val okCases =
      for
        f <- lc.forkJoin.get.forks
        pattern = mkPattern(lc.events,f.pre,lc.event2Param)
      yield
        MatchTypCase(pattern,TName("true"),Some(mkCaseComment(f)))
    val koCase = MatchTypCase("_"::Nil, TName("false"))
    okCases:+koCase

  protected def mkJoins(ctx:RoleCtx):List[MethodDef] =
    if ctx.forkes then
      val forkJoin = ctx.localCtx.head.forkJoin.get
      val joins = for j <- forkJoin.joins yield mkJoinMethod(j,ctx)
      joins
    else
      Nil

  protected def mkJoinMethod(j:JoinInfo,ctx:RoleCtx):MethodDef =
    val params      = mkJoinParams(j,ctx)
    val ev          = Set[Evidence]() // todo
    val returnType  = mkJoinType(j,ctx)
    val statement   = mkJoinSt(j,ctx)
    MethodDef("join",Nil,params,ev,Some(returnType),statement,Some(mkComment(j)))

  protected def mkJoinParams(j:JoinInfo,ctx: RoleCtx):List[Param] =
    val lc = ctx.localCtx.head
    val allFalse = lc.events.map(e=>e->"false").toMap
    val args =
      for r <- j.regions yield
        mkPattern(lc.events,Map(forkEvent->r.toString),allFalse)
    val types =
      for p <- args yield
        TName(ctx.name,Some(TTuple(p.map(a=>TName(a))).toString::Nil))
    val params =
      for (r,t) <- j.regions.zip(types) yield
        Param(s"s$r",t)
    params

  protected def mkJoinType(j:JoinInfo,ctx: RoleCtx):TName =
    val lc = ctx.localCtx.head
    val allFalse = lc.events.map(e=>e->"false").toMap
    val typeVars = Tuple(mkPattern(lc.events,j.post,allFalse).map(v=>Variable(v))).toString
    TName(ctx.name,Some(typeVars::Nil))

  protected def mkJoinSt(j:JoinInfo,ctx:RoleCtx):Statement =
    val lc = ctx.localCtx.head
    val allFalse = lc.events.map(e=>e->"false").toMap
    val stArgs = Tuple(mkPattern(lc.events,j.post,allFalse).map(v=>Variable(v)))
    val net = Variable(s"s${j.regions.head}.net")
    FunCall(ctx.name,Nil,stArgs::net::Nil)

  protected def mkDefaultFinalType(ctx:RoleCtx):TExp =
    val args =
      for lc <- ctx.localCtx yield
        TTuple(for i <- (1 to lc.eventsCtx.size).toList yield TName("false")).toString
    TName(ctx.name, Some(args))

  protected def mkEndTypeDefIfFork(ctx:RoleCtx):TypeDef =
      val lc = ctx.localCtx.head
      val defaultValues = lc.events.map(e=>e->"false").toMap
      val argValues = mkPattern(lc.events,Map(forkEvent->"0"),defaultValues)
      val args = TTuple(argValues.map(v=>TName(v)))
      val endType = TName(ctx.name,Some(args.toString::Nil))
      TypeDef(TName(ctx.name+"$Final"),endType)


  def mkEndType(ctx:RoleCtx):TExp =
    TName(ctx.name+"$Final",None)


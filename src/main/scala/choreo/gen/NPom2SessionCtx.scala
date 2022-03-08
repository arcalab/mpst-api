package choreo.gen

import choreo.npomsets.NPomset
import choreo.syntax.Agent
import choreo.gen.*
import choreo.gen.EventsCtx.*
import choreo.npomsets.NPomset.Event
import choreo.syntax.Choreo.{In,Out}
import choreo.api.MiniScala.TVar


object NPom2SessionCtx:

  type Event2Value = Map[Event,String]

  // context information for a pomset
  def apply(npom:NPomset):SessionCtx =
    // all options
    val choices = npom.refinements
    val pomsByAgent = for a <- npom.agents yield a-> choices.map(_.project(a))
    val agent2Ctx = for (a,poms) <- pomsByAgent yield a->apply(a,poms)
    SessionCtx(agent2Ctx.toMap)

  // context information for an agent
  protected def apply(agent:Agent,poms:List[NPomset]):RoleCtx =
    val options =
      if poms.size == 1 then
        apply(agent,poms.head.simplifiedFull,1)(true)::Nil
      else for
        (p,i)<-poms.zipWithIndex
        psim = p.simplifiedFull
      yield apply(agent,psim,i+1)(false)
    RoleCtx(agentClassName(agent),agent,options)

//  lazy val lastEvent = -1
  lazy val forkEvent = 0

  // context information for an option
  protected def apply(agent:Agent, pom:NPomset, index:Int)(implicit single:Boolean):RoleLocalCtx =
    val events =
      for e <- pom.events.toSet yield
        mkEventCtx(e,pom)
    val forkJoin = if single then getForkJoinInfo(pom) else None
    val extraEvents = if forkJoin.isDefined then forkEventCtx::Nil else Nil
    RoleLocalCtx(optionClassName(agent,index),agent, events.toList, extraEvents,forkJoin)

  protected val forkEventCtx = ExtEventCtx(forkEvent,"f",TVar("F",None),"0")
//    lazy val forkPoints = getForkPoints(pom)
//    val canFork         = single && forkPoints.nonEmpty
//    val name            = optionClassName(agent,index)
//    val agentCom        = mkAgentCom(pom)
//    val agentEvid       = mkAgentEvid(agent,pom)
//    val eventNames      = getEventNames(pom)
//    val paramNames      = mkParamNames(eventNames)
//    val typeVariables   = mkTVariables(eventNames)
//    val initialVals     = paramNames.map(e=>e._1->"true")
//    val res             =
//      RoleLocalCtx(
//        name,
//        agent,
//        agentCom,
//        agentEvid,
//        paramNames,
//        typeVariables,
//        initialVals,
//        shared,
//        None
//      )
//    if canFork then
//      addForkInfo(res,mkForkInfo(forkPoints, pom))
//    else res

  protected def mkEventCtx(e:Event,pom:NPomset):EventCtx =
    val pre   = mkPreEvid(e,pom)
    val post  = pre++mkPostEvid(e,pom) // pre because no we don't have explicit the variable names
    EventCtx(e,pom.actions(e).asInstanceOf[In|Out],pre,post,"v"+e,TVar("V"+e),"true")

  protected def getForkJoinInfo(pom:NPomset):Option[ForkJoin] =
    val initials = pom.minimum()
    if initials.size > 1 then
      val fork                = Fork(Many(initials),initials.map(i=>i->List(i)).toMap)
      val (forks,joins,lasts) = traverse(fork::Nil)(pom)
      val mergedJoins         = mergeJoins(joins)
      val lastJoin            = getLastJoin(lasts)
      Some(mkForkJoinInfo(forks,mergedJoins++lastJoin.toList)(pom))
    else traverseShard(initials.head)(pom)

  protected def mkForkJoinInfo(forks:List[Fork],joins:List[Join])(implicit pom:NPomset):ForkJoin =
    val forkInfo = for f <- forks yield mkForkInfo(f)
    val joinInfo = for j <- joins yield mkJoinInfo(j)
    ForkJoin(forkInfo,joinInfo)

  protected def mkForkInfo(fork:Fork)(implicit pom:NPomset):ForkInfo =
    val pre = fork.point match
      case One(e) =>
        Map(e->"false") ++ fork.branches.keySet.map(s=> s->"true")
      case Many(events) =>
        events.map(e=>e->"true").toMap
    val post  = fork.branches.map(b=>mkRegionInfo(b))
    ForkInfo(fork.point,pre,post.toList)

  protected def mkRegionInfo(regionEvents: (Int, List[Event]))(implicit pom:NPomset):RegionInfo =
    val (r,events) = regionEvents
    val others = pom.events.toSet -- events.toSet
    val trues  = events.map(e=>e->"true").toMap
    val falses = others.map(e=>e->"false").toMap
    RegionInfo(r,trues++falses+(forkEvent->r.toString))

  protected def mkJoinInfo(join: Join)(implicit pom:NPomset):JoinInfo =
    val post = join.point match
      case One(e) => pom.allSuccesors(e).map(s=>s->"true").toMap+(forkEvent->"0")
      case Many(e) => Map(forkEvent->"0")
    JoinInfo(join.regions,post)

  protected def mergeJoins(joins:List[Join]):List[Join] =
    val joinsByPoint = joins.groupBy(j=>j.point)
    for (jp,ji) <- joinsByPoint.toList yield
      Join(jp,ji.flatMap(j=>j.regions))

  protected def getLastJoin(lasts: List[(Event, Int)]):Option[Join] =
    val lastsByEvent = lasts.groupBy(_._1)
    if lastsByEvent.size <= 1 then None
    else Some(Join(Many(lasts.map(_._1).toSet),lasts.map(_._2)))

  case class ForkInfo(poin:Split,pre:Event2Value,post:List[RegionInfo])
  case class JoinInfo(regions:List[Int], post:Event2Value)
  case class RegionInfo(id:Int,values:Event2Value)
  /**
   * Information about forks
   * @param fork a map from each fork point to its branching information
   * @param join a map from each join point and its merging information
   */
  case class ForkJoin(fork:List[ForkInfo],join:List[JoinInfo])

  protected def traverseShard(e:Event)(implicit pom:NPomset):Option[ForkJoin] =
    val succ = pom.succ.getOrElse(e,Set())
    if succ.isEmpty then
      None
    else if succ.size > 1 then
      val fork = Fork(One(e),succ.map(s=>s->List(s)).toMap)
      val (forks,joins,lasts) = traverse(fork::Nil)
      val mergedJoins         = mergeJoins(joins)
      val lastJoin            = getLastJoin(lasts)
      Some(mkForkJoinInfo(forks,mergedJoins++lastJoin.toList))
    else traverseShard(succ.head)

  protected def traverse(forks:List[Fork])(implicit pom:NPomset):(List[Fork],List[Join],List[(Event,Int)]) =
    var traversedForks = List[Fork]()
    var toTraverse = List[Fork]()
    var njoins = List[Join]()
    var lasts = List[(Event,Int)]()
    for f <- forks do
      val (nf,mf,mj,l) = traverseFork(f)
      traversedForks :+= nf
      toTraverse ++= mf
      njoins ++=mj
      lasts ++= l
    if toTraverse.isEmpty then
      (traversedForks,njoins,lasts)
    else
      val (fs,js,ls) = traverse(toTraverse)
      (traversedForks++fs,njoins++js,lasts++ls)

  protected def traverseFork(fork: Fork)(implicit pomset: NPomset)
    :(Fork,List[Fork],List[Join],List[(Event,Int)]) =
    var nforks = List[Fork]()
    var njoins = List[Join]()
    var lasts = List[(Event,Int)]()
    var nfork = fork
    for (r,b) <- fork.branches do
      val (nf,nfs,njs,l) = traverseBranch(r,nfork)
      nfork = nf
      if nfs.isDefined then nforks :+= nfs.get
      if njs.isDefined then njoins :+= njs.get
      if l.isDefined  then lasts  :+= l.get

    (nfork,nforks,njoins,lasts)

  protected def traverseBranch(r:Int,fork:Fork)(implicit pom: NPomset)
    :(Fork,Option[Fork],Option[Join],Option[(Event,Int)]) =
    // starting to traverse the new branch
    val succ = pom.succ.getOrElse(r,Set())
    if succ.isEmpty then // r is the first and last element in the shard
      (fork,None,None,Some((r,r)))
    else if succ.size > 1 then // r it's the 1st element and also a fork point
      (fork,Some(Fork(One(r), succ.map(s=>s->List(s)).toMap)), None, None)
    else // exactly on successor, then traverse the branch
      traverseBranch(r,fork,succ.head)


  protected def traverseBranch(r:Int,fork:Fork,e:Event)(implicit pom:NPomset)
    : (Fork,Option[Fork],Option[Join],Option[(Event,Int)]) =
    val succ = pom.succ.getOrElse(e,Set())
    if succ.isEmpty then // e has no succesors
      val pred = pom.realPred(e)
      if pred.size <= 1 then // additionally, it doesn't join any branch
        // last element in the region and a part of a special join
        (fork.addEvent2Region(e,r),None,None,Some((e,r)))
      else // pred > 1, so is a join with no succesors
        (fork,None,Some(Join(One(e), List(r))),Some((e,r)))//Map(fork.branches(r).last,r)),None)
    else if succ.size > 1 then // fork point
      (fork,Some(Fork(One(e),succ.map(s=>s->List(s)).toMap)),None,None)
    else // has exactly one successor, it is part of the shard
      traverseBranch(r,fork.addEvent2Region(e,r),succ.head)




//  val specialInit = -1
//  val specialLast = -2
  type Forks = List[Fork]
  type Joins = List[Join]
  sealed trait Split
  case class One(e:Event)         extends Split
  case class Many(events:Set[Event]) extends Split

  case class Join(point:Split,regions:List[Int]):
    def waitFor(region:Int):Join = Join(point, regions:+region)
  case class Fork(point:Split,branches:Map[Int,List[Event]]):
    def addEvent2Region(e:Event,region:Int):Fork =
      if branches.isDefinedAt(region) then
        Fork(point, branches + (region->(branches(region):+e)))
      else Fork(point, branches + (region->List(e)))
  //
//  protected def addForkInfo(agentCtx:AgentOptCtx,forkInfo: ForkInfo):AgentOptCtx =
//    val nParams   = agentCtx.paramName+(lastEvent->"e")+(forked->"f")
//    val nTypeVar  = agentCtx.typeVariables.addVar(lastEvent,"E",Some("ET"))
//      .addVar(forked,"F",Some("TF"))
//    val ninitials = agentCtx.initialValues+
//      (lastEvent->"false")+
//      (forked->"false")
//    val npost =
//      for (e,ev) <- agentCtx.agentEvid.post yield
//        e->(ev+(lastEvent->e.toString))
//    val evid = Evid(agentCtx.agentEvid.pre,npost)
//    AgentOptCtx(
//      agentCtx.name,
//      agentCtx.agent,
//      agentCtx.agentCom,
//      evid,
//      nParams,
//      nTypeVar,
//      ninitials,
//      agentCtx.shared,
//      Some(forkInfo)
//    )
//
//
//  protected def mkForkInfo(forkPoints:Set[SplitPoint],pom:NPomset):ForkInfo =
//    val branches = for fp <- forkPoints yield fp -> mkBranchInfo(fp,pom)
//    val joins    = for jp <- getJoinPoints(pom) yield jp -> mkJoinInfo(jp,pom)
//    //println(s"branches: $branches")
//    //println(s"joins: $joins")
//    ForkInfo(branches.toMap,joins.toMap)
//
//  protected def  mkBranchInfo(forkPoint: SplitPoint,pom:NPomset):BranchInfo = forkPoint match
//    case SinglePoint(e) =>
//      val succ  = pom.succ.getOrElse(e,Set())
//      val pre   = Map(e->"false") ++ succ.map(s=> s->"true")
//      val post  = mkBranchPost(succ,pom)
//      BranchInfo(pre,post)
//    case MultPoints(events) =>
//      val pre = events.map(e=>e->"true").toMap
//      val post = mkBranchPost(events,pom)
//      BranchInfo(pre,post)
//
//  protected def mkBranchPost(events:Set[Event],pom:NPomset):List[Map[Event,String]] =
//    (for
//      e <- events
//      others = events - e
//      toFalse = others++others.flatMap(e=>pom.allSuccesors(e))
//    yield
//      toFalse.map(e=> e -> "false").toMap).toList
//
//  protected def mkJoinInfo(jp:SplitPoint,pom:NPomset):JoinInfo = jp match
//    case SinglePoint(e) =>
//      val toFalse = pom.events.toSet.map(e=>e->"false").toMap
//      val pred    = pom.realPred(e)
//      val succ    = pom.allSuccesors(e)
//      val pre     = for p <- pred yield toFalse+(lastEvent->e.toString)+(forked->"true")//+(forked->"true")
//      val post    = for s <- succ yield s->"true"
//      val post1   = (pom.allRealPred(e).map(e=> e->"false"))
//      JoinInfo(pre.toList,
//        post.toMap++post1+(lastEvent->"false")+(forked->"false"))
//    case MultPoints(es) =>
//      val toFalse = pom.events.toSet.map(e=>e->"false").toMap
//      val pre = es.map(e=>toFalse+(lastEvent->e.toString)+(forked->"true"))
//      val post = toFalse+(lastEvent->"false")+(forked->"false")//es.flatMap(e=>pom.allSuccesors(e)).map(s=>s->"true")+(forkEvent->"false")
//      JoinInfo(pre.toList,post)
//
//
//
//  protected def getForkPoints(pom:NPomset):Set[SplitPoint] =
//    val forkEvents:Set[SplitPoint] =
//      getForkEvents(pom).map(e=>SinglePoint(e))
//    val initialFork = pom.minimum()
//    if initialFork.size>1 then
//      forkEvents + MultPoints(initialFork)
//    else
//      forkEvents
//
//  protected def getJoinPoints(pom:NPomset):Set[SplitPoint] =
//    val forkEvents:Set[SplitPoint] =
//      getJoinEvents(pom).map(e=>SinglePoint(e))
//    val lastJoin = for e<- pom.events.toSet; if pom.allSuccesors(e).isEmpty yield e
//    if lastJoin.size>1 then
//      forkEvents + MultPoints(lastJoin)
//    else
//      forkEvents
//
//  protected def getForkEvents(pom:NPomset):Set[Event] =
//    for
//      e <- pom.events.toSet
//      directSucc = pom.succ.getOrElse(e,Set())
//      if directSucc.size>1
//    yield
//      e
//
//  protected def getJoinEvents(pom:NPomset):Set[Event] =
//    for
//      e <- pom.events.toSet
//      directPred = pom.realPred(e)
//      if directPred.size>1
//    yield
//      e

  //type EventNames = Map[Event,(String,Option[Int])]
//  type EventNames2 = Map[Event,String]

//  protected def mkTVariables(eventNames: EventNames2):TVarInfo =
//    val info = for (e,n) <- eventNames yield
//      e->("V"++n,Some("TF"))
//    //val types = names.view.mapValues(_=> "TF")
//    TVarInfo(info)//,types.toMap)
//
//  //protected def mkTVariables(eventNames: EventNames):TVarInfo =
//  //  val info = for (e,(n,i)) <- eventNames yield
//  //    if i.isDefined then
//  //      e->("V"++n++i.get.toString,Some("TF"))
//  //    else e->("V"++n,Some("TF"))
//  //  //val types = names.view.mapValues(_=> "TF")
//  //  TVarInfo(info)//,types.toMap)
//
//  protected def getEventNames(pom: NPomset):EventNames2 =
//    (for e<- pom.events.toSet yield e->e.toString).toMap
//  //protected def getEventNames(pom: NPomset):EventNames =
//  //  val com             = mkAgentCom(pom)
//  //  val to              = com.sends.map(s=>s._1->s._2.b.s)
//  //  val from            = com.receives.map(s=>s._1->s._2.b.s)
//  //  val groupByPassive  = (to++from).groupBy(_._2)
//  //  (for (b,m) <- groupByPassive yield
//  //    if m.size > 1 then
//  //      m.zipWithIndex.map({case ((e,b),i) => e -> (b,Some(i+1))}).toMap
//  //    else
//  //      Map[Event,(String,Option[Int])](m.head._1->(b,None))).flatten.toMap
//
//
//  protected def mkParamNames(eventNames:EventNames2):Map[Event,String] =
//    for (e,n) <- eventNames yield e->("v"++n)
//
//  //protected def mkParamNames(eventNames:EventNames):Map[Event,String] =
//  //  for (e,(n,i)) <- eventNames yield
//  //    if i.isDefined then
//  //      e->("v"++n++i.get.toString)
//  //    else e->("v"++n)
//
//  protected def mkAgentCom(pom:NPomset):Com =
//    val sends = pom.actions.collect({case (e,a@Out(_,_,_)) => e->a})
//    val recs  = pom.actions.collect({case (e,a@In(_,_,_)) => e->a})
//    Com(sends,recs)
//
//  protected def mkAgentEvid(a:Agent,pom:NPomset):Evid =
//    var pre   = Map[Event,Event2Value]()
//    var post  = Map[Event,Event2Value]()
//    for e <- pom.events.toSet do
//      pre += (e->mkPreEvid(e,pom))
//      post += (e->mkPostEvid(e))
//    Evid(pre,post)

  protected def mkPreEvid(e:Event,pom:NPomset):Event2Value =
    pom.realPred(e).map(pre => pre->"false").toMap + (e->"true")

  protected def mkPostEvid(e:Event,pom:NPomset):Event2Value =
    Map(e->"false")


  /* ---- HELPERS ---- */

  protected def optionClassName(a:Agent,i:Int): String = a.s.toUpperCase + "$Pom" +i.toString
  protected def agentClassName(a:Agent): String = a.s.toUpperCase + "$State"


//  protected def sharedActions(poms:Iterable[NPomset]):Set[Action] =
//    val actionsPerPom = poms.map(p=>p.actions.values.toSet)
//    if actionsPerPom.size > 1 then
//      actionsPerPom.tail.foldRight(actionsPerPom.head)(_.intersect(_))
//    else Set()
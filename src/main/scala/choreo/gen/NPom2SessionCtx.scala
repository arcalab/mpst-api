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

  val forkEvent = 0

  // context information for a pomset
  def apply(npom:NPomset):SessionCtx =
    // all options
    val choices = npom.refinements
    val pomsByAgent = for a <- npom.agents yield a-> choices.map(_.project(a).simplifiedFull)
    val agent2Ctx = for (a,poms) <- pomsByAgent yield a->apply(a,poms)
    SessionCtx(agent2Ctx.toMap)

  protected val forkEventCtx = ExtEventCtx(forkEvent,"f",TVar("F",None),"0")

  // context information for an agent
  protected def apply(agent:Agent,poms:List[NPomset]):RoleCtx =
    val options =
      if poms.size == 1 then
        apply(agent,poms.head,1)(true)::Nil
      else for
        (p,i)<-poms.zipWithIndex
      yield apply(agent,p,i+1)(false)
    RoleCtx(agentClassName(agent),agent,options)

  // context information for an option
  protected def apply(agent:Agent, pom:NPomset, index:Int)(implicit single:Boolean):RoleLocalCtx =
    val events =
      for e <- pom.events.toSet.toList.sorted yield
        mkEventCtx(e,pom)
    val forkJoin = if single then getForkJoinInfo(pom) else None
    if forkJoin.isDefined then println(forkJoin.get)
    val extraEvents = if forkJoin.isDefined then forkEventCtx::Nil else Nil
    RoleLocalCtx(optionClassName(agent,index),agent, events, extraEvents,forkJoin)

  protected def mkEventCtx(e:Event,pom:NPomset):EventCtx =
    val pre   = mkPreEvid(e,pom)
    val post  = pre++mkPostEvid(e,pom) // pre because no we don't have explicit the variable names
    EventCtx(e,pom.actions(e).asInstanceOf[In|Out],pre,post,"v"+e,TVar("V"+e),"true")

//  protected def getForkJoinInfo(pom:NPomset):Option[ForkJoin] =
//    val initials = pom.minimum()
//    if initials.size > 1 then
//      val fork                = Fork(Many(initials),initials.map(i=>i->List(i)).toMap)
//      val (forks,joins,lasts) = traverse(fork::Nil)(pom)
//      val mergedJoins         = mergeJoins(joins)
//      val lastJoin            = getLastJoin(lasts)
//      Some(mkForkJoinInfo(forks,mergedJoins++lastJoin.toList)(pom))
//    else traverseShard(initials.head)(pom)

  protected def mkForkJoinInfo(forks:List[Fork],joins:List[Join])(implicit pom:NPomset):ForkJoin =
    val lastJoin = getLastJoin(joins)
    val forkInfo = for f <- forks yield mkForkInfo(f)
    val joinInfo = for j <- joins yield mkJoinInfo(j,lastJoin)
    ForkJoin(forkInfo, joinInfo)

  protected def mkForkInfo(fork:Fork)(implicit pom:NPomset):ForkInfo =
    val pre = fork.point match
      case One(e) =>
        Map(e->"false") ++ fork.branches.keySet.map(s=> s->"true")
      case Many(events) =>
        events.map(e=>e->"true").toMap
    val post  = fork.branches.map(b=>mkRegionInfo(b))
    ForkInfo(fork.point, pre, post.toList.sortBy(_.id))

  protected def mkRegionInfo(regionEvents: (Int, List[Event]))(implicit pom:NPomset):RegionInfo =
    val (r,events) = regionEvents
    val others = pom.events.toSet -- events.toSet
    val trues  = events.map(e=>e->"true").toMap
    val falses = others.map(e=>e->"false").toMap
    RegionInfo(r,trues++falses+(forkEvent->r.toString))

  protected def mkJoinInfo(join: Join, lastJoinPoint:Option[Event])(implicit pom:NPomset):JoinInfo =
    val post = join.point match
      case One(e) =>
        val newRegion =
          if lastJoinPoint.isDefined && lastJoinPoint.get == e then
          //if !manyLasts && pom.allSuccesors(e).isEmpty then
            // avoid extra join
            forkEvent->"0"
          else
            forkEvent->e.toString
        pom.allSuccesors(e).map(s=>s->"true").toMap+newRegion
      case Many(e) => Map(forkEvent->"0")
    JoinInfo(join.point,join.regions.sorted,post)

  protected def getLastJoin(joins:List[Join])(implicit pomset: NPomset):Option[Event] =
    val manyLasts = joins.find({case j@Join(_:Many,_) => true; case _ => false})
    if !manyLasts.isDefined then // no special join at the end
      val points = for j <- joins yield j.point.asInstanceOf[One].e
      Some(getLastJoin(points))
    else None

  protected def getLastJoin(joinPoints:List[Event])(implicit pom:NPomset):Event =
    var last = joinPoints.head
    var visit = joinPoints.tail.toSet
    while visit.nonEmpty do
      val next =  visit.head
      if pom.allSuccesors(last).contains(next) then
        last = next
      visit -= next
    last



//  protected def mergeJoins(joins:List[Join]):List[Join] =
//    val joinsByPoint = joins.groupBy(j=>j.point)
//    for (jp,ji) <- joinsByPoint.toList yield
//      Join(jp,ji.flatMap(j=>j.regions))
//
//  protected def getLastJoin(lasts: List[(Event, Int)]):Option[Join] =
//    val lastsByEvent = lasts.groupBy(_._1)
//    if lastsByEvent.size <= 1 then None
//    else Some(Join(Many(lasts.map(_._1).toSet),lasts.map(_._1).distinct))

  case class ForkInfo(point: Split, pre: Event2Value, post: List[RegionInfo])
  case class JoinInfo(point:Split, regions:List[Int], post:Event2Value)
  case class RegionInfo(id:Int,values:Event2Value)

  /**
   * Information about forks
 *
   * @param forks a map from each fork point to its branching information
   * @param joins a map from each join point and its merging information
   */
  case class ForkJoin(forks: List[ForkInfo], joins: List[JoinInfo]):
    override def toString: String =
      s"""
         |forks:\n ${forks.mkString("\n")}
         |joins:\n ${joins.mkString("\n")}
         |""".stripMargin

//  protected def traverseShard(e:Event)(implicit pom:NPomset):Option[ForkJoin] =
//    val succ = pom.succ.getOrElse(e,Set())
//    if succ.isEmpty then
//      None
//    else if succ.size > 1 then
//      val fork = Fork(One(e),succ.map(s=>s->List(s)).toMap)
//      val (forks,joins,lasts) = traverse(fork::Nil,)
//      val mergedJoins         = mergeJoins(joins)
//      val lastJoin            = getLastJoin(lasts)
//      Some(mkForkJoinInfo(forks,mergedJoins++lastJoin.toList))
//    else traverseShard(succ.head)
//
//  protected def traverse(forks:List[Fork],visited:Set[Event])(implicit pom:NPomset)
//    :(List[Fork],List[Join],List[(Event,Int)],Set[Event]) =
//    var traversedForks = List[Fork]()
//    var toTraverse = List[Fork]()
//    var njoins = List[Join]()
//    var lasts = List[(Event,Int)]()
//    var allVisited = visited
//    for f <- forks do
//      val (nf,mf,mj,l,nvisited) = traverseFork(f,allVisited)
//      traversedForks :+= nf
//      toTraverse ++= mf
//      njoins ++=mj
//      lasts ++= l
//      allVisited ++=nvisited
//    if toTraverse.isEmpty then
//      (traversedForks,njoins,lasts)
//    else
//      val (fs,js,ls,nvisited) = traverse(toTraverse,allVisited)
//      (traversedForks++fs,njoins++js,lasts++ls,nvisited)
//
//  protected def traverseFork(fork: Fork,visited:Set[Event])(implicit pomset: NPomset)
//    :(Fork,List[Fork],List[Join],List[(Event,Int)]) =
//    var nforks = List[Fork]()
//    var njoins = List[Join]()
//    var lasts = List[(Event,Int)]()
//    var nfork = fork
//    var allVisited = visited
//    for (r,b) <- fork.branches do
//      val (nf,nfs,njs,l,nvisited) = traverseBranch(r,nfork,visited)
//      nfork = nf
//      if nfs.isDefined then nforks :+= nfs.get
//      if njs.isDefined then njoins :+= njs.get
//      if l.isDefined  then lasts  :+= l.get
//      allVisited++=nvisited
//
//    (nfork,nforks,njoins,lasts,allVisited)
//
//  protected def traverseBranch(r:Int,fork:Fork,visited:Set[Event])(implicit pom: NPomset)
//    :(Fork,Option[Fork],Option[Join],Option[(Event,Int)],Set[Event]) =
//    // starting to traverse the new branch
//    val succ = pom.succ.getOrElse(r,Set())
//    if succ.isEmpty then // r is the first and last element in the shard
//      (fork,None,None,Some((r,r)),visited+r)
//    else if succ.size > 1 then // r it's the 1st element and also a fork point
//      (fork,Some(Fork(One(r), succ.map(s=>s->List(s)).toMap)), None, None,visited+r)
//    else // exactly on successor, then traverse the branch
//      traverseBranch(r,fork,succ.head,visited)
//
//  protected def traverseBranch(r:Int,fork:Fork,e:Event,visited:Set[Event])(implicit pom:NPomset)
//    : (Fork,Option[Fork],Option[Join],Option[(Event,Int)],Set[Event]) =
//    if visited.contains(e) then
//      (fork,None,None,None,visited)
//    else
//      val pred = pom.realPred(e)
//      val succ = pom.succ.getOrElse(e,Set())
//      if succ.isEmpty then // e has no succesors
//        val pred = pom.realPred(e)
//        if pred.size <= 1 then // additionally, it doesn't join any branch
//          // last element in the region and a part of a special join
//          (fork.addEvent2Region(e,r),None,None,Some((e,r)),visited+e)
//        else // pred > 1, so is a join with no succesors
//          (fork,None,Some(Join(One(e), List(r))),Some((e,r)),visited+e)//Map(fork.branches(r).last,r)),None)
//      else if succ.size > 1 then // fork point
//        val pred = pom.realPred(e)
//        if pred.size <= 1 then // no join
//          (fork,Some(Fork(One(e),succ.map(s=>s->List(s)).toMap)),None,None,visited+e)
//        else // also a join
//          (fork,None,Some(Join(One(e), List(r))),None,visited+e)
//      else // has exactly one successor, it is part of the shard
//        traverseBranch(r,fork.addEvent2Region(e,r),succ.head,visited+e)
//
//  protected def traverseBranch1(r:Int,fork:Fork,e:Event,visited:Set[Event])(implicit pom:NPomset)
//  : (Fork,Option[Fork],Option[Join],Option[(Event,Int)],Set[Event]) =
//    if visited.contains(e) then
//      (fork,None,None,None,visited)
//    else
//      val pred = pom.realPred(e)
//      val succ = pom.succ.getOrElse(e,Set())
//      val mbFork =
//        if succ.size > 1 then // is a fork
//          Some(Fork(One(e),succ.map(s=>s->List(s)).toMap))
//        else None
//      val mbJoin =
//        if pred.size > 1 then // is a join
//          Some(Join(One(e), List(r)))
//        else None
//      val mbLast =
//        if succ.isEmpty then // last element
//          Some((e,r))
//        else None
//      if mbFork.isDefined || mbJoin.isDefined || mbLast.isDefined then
//        (fork,mbFork,mbJoin,mbLast,visited+e)
//      else // has exactly one successor and one predecessor, it is part of the shard
//        traverseBranch(r,fork.addEvent2Region(e,r),succ.head,visited+e)

  case class TraverseInfo(
    forks:Map[Event,Fork],
    joins:Map[Event,Join],
    last:Map[Event,Int],
    visited:Set[Event]
  ):
    def addFork(e:Event,f:Fork):TraverseInfo =
      println(s"new fork $f")
      this.copy(forks = forks+(e->f),visited= visited+e)
    def addJoin(e:Event,j:Join):TraverseInfo =
      println(s"new join $j")
      if joins.isDefinedAt(e) then
        val old = joins(e)
        val upd = Join(old.point,old.regions++j.regions)
        println(s"it updates old: $old with $upd")
        this.copy(joins = joins+(e->upd),visited = visited+e)
      else this.copy(joins = joins+(e->j),visited = visited+e)
    def addLast(e:Event,r:Int):TraverseInfo =
      println(s"new last $e waiting for region $r")
      this.copy(last = last+(e->r),visited = visited+e)
    def getForks:List[Fork] = forks.values.toList
    def getJoins:List[Join] = joins.values.toList
    def getLastRegions:List[Int] = last.values.toList
    def getLastEvents:Set[Event] = last.keySet
  object TraverseInfo:
    def apply():TraverseInfo = TraverseInfo(Map(),Map(),Map(),Set())


  protected def getForkJoinInfo(pom:NPomset):Option[ForkJoin] =
//    println(s"fork info for: $pom")
    val min = pom.minimum()
    val traverseInfo = traversePom(pom)
    var forks = traverseInfo.getForks
    var joins = traverseInfo.getJoins
    if forks.size >= 1 then
      if min.size > 1 then
        forks = forks :+ Fork(Many(min),min.map(i=>i->List(i)).toMap)
//        println(s"added special fork: ${Fork(Many(min),min.map(i=>i->List(i)).toMap)}")
      if traverseInfo.last.size > 1 then
        joins = joins :+ Join(Many(traverseInfo.getLastEvents),traverseInfo.getLastRegions)
//        println(s"added special join: ${Join(Many(traverseInfo.getLastEvents),traverseInfo.getLastRegions)}")
      Some(mkForkJoinInfo(forks,joins)(pom))
    else None

  protected def traversePom(pom:NPomset) =
    var min = pom.minimum()
    var nti = TraverseInfo()
    while min.nonEmpty do
      val next = min.head
      nti = visitAll(next,next,nti)(pom)
      min -= next
    nti

  protected def visitAll(e:Event,r:Int,traverseInfo: TraverseInfo)(implicit pom:NPomset):TraverseInfo =
//    println(s"visitAll e:$e, r:$r")
    val succ = pom.succ.getOrElse(e,Set())
    val pred = pom.realPred(e)
    var nti = visit(e,r,traverseInfo)
    if succ.size > 1  then // is a fork -> change region
      for s <- succ /*; if ! nti.visited.contains(e) */do
        nti = visitAll(s,s,nti)
      nti
    else if succ.size == 1 then // one succ
      if pred.size > 1 then // join, -> change region
        visitAll(succ.head,e,nti)
      else
        visitAll(succ.head,r,nti) // same region
    else if pred.size > 1 then  // no succ and join -> update wait region to self
      nti.addLast(e,e)
    else nti // no succ



  protected def visit(e:Event,r:Int,traversInfo:TraverseInfo)(implicit pom:NPomset):TraverseInfo =
    var nti = traversInfo
    val succ = pom.succ.getOrElse(e,Set())
    val nti1 = checkLast(e,r, traversInfo)
    val nti2 = checkJoin(e,r,nti1)
    val nti3 = checkFork(e,r,nti2)
    nti3

  protected def checkLast(e:Event,r:Int,traversInfo:TraverseInfo)(implicit pom:NPomset):TraverseInfo =
    if pom.succ.getOrElse(e,Set()).isEmpty then
      traversInfo.addLast(e,r)
    else traversInfo

  protected def checkFork(e:Event,r:Int,traversInfo:TraverseInfo)(implicit pom:NPomset):TraverseInfo =
    val succ = pom.succ.getOrElse(e,Set())
    if succ.size > 1 then
      traversInfo.addFork(e,Fork(One(e),succ.map(s=>s->List(s)).toMap))
    else traversInfo

  protected def checkJoin(e:Event,r:Int,traversInfo:TraverseInfo)(implicit pom:NPomset):TraverseInfo =
    val pred = pom.realPred(e)
    if pred.size > 1 then
      traversInfo.addJoin(e,Join(One(e),List(r)))
    else traversInfo


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
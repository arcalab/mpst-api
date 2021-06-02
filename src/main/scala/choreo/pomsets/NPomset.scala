package choreo.pomsets

import choreo.{DSL, Examples}
import choreo.pomsets.NPomset._
import choreo.syntax.{Agent, Choreo, Msg}
import choreo.syntax.Choreo.{Action, In, Out, agents}

/**
 * Variation of the Pomset structure, using a nesting structure `N` that groups events.
 * It is not kept normalised, i.e., the mapping of `actions` and `order` may refer to
 * non-existing events in the `event` nested set.
 *
 * So far, ignoring loops and delayed choices.
 * @author José Proença
 */
case class NPomset(events: Events,
                   actions: Actions,
                   pred:Order,
                   loop:LoopInfo):
  lazy val agents:Iterable[Agent] =
    actions.flatMap(kv => Choreo.agents(kv._2))

  /** Remove an event from the NPomset */
  def -(e:Event) = this -- Set(e)
  /** Remove a set of events from the NPomset */
  def --(es:Set[Event]):NPomset =
    NPomset(events--es,actions--es, pred, loop) // not dropping from the order, since it could break chains
//            order.filterNot(o=>es.contains(o.left) || es.contains(o.right)))

  /** Weak sequencing of NPomsets */
  def >>(other:NPomset): NPomset =
    val deps =
      for a <- other.agents
          in <- actions.filter(p=>isActive(a,p._2)).keys
          inOther <- other.actions.filter(p=>isActive(a,p._2)).keys
      yield (inOther,in)
    NPomset(events++other.events,actions++other.actions,
      add(deps,add(pred,other.pred)), add(loop,other.loop))

  private def isActive(agent: Agent, act: Action) = act match
    case In(`agent`,_,_) => true
    case Out(`agent`,_,_) => true
    case _ => false

  /** Choice of NPomsets (ignoring loops so far) */
  def or(other:NPomset): NPomset =
    NPomset(events or other.events, actions++other.actions,
      add(pred,other.pred), add(loop,other.loop))

  /** Parallel composition of NPomsets */
  def ++(other: NPomset): NPomset =
    NPomset(events ++ other.events, actions++other.actions,
      add(pred,other.pred), add(loop,other.loop))

  //  def refinements: Set[NPomset] =
//    (for choice <- events.cs do NPomset(Nesting())

  ///////////////
  // Refinement functions to be used in the semantics
  ///////////////

  /** true if the event is not in a choice */
  def onTop(e:Event) = events.acts contains e

  /** Do minimum refinement until the pomset is ready to perform `e` */
  def readyFor(e:Event): Option[(NPomset,Event)] =
    for
    // 1. for all predecessor, try to remove it by chosing empty choices (if available)
      evs1 <- dropEvents(allRealPred(e),events)
    // 2. if it is in a choice, remove alternatives.
      (evs2,genEvs,seed2) <- select(e,evs1,loop._2)
    yield
      val newActions = adaptActions(genEvs)
      val newPred    = adaptPred(genEvs)
      val realEvent  = genEvs.getOrElse(e,e)
      // return final pomset, with new actions, predecessors, and seed
      (NPomset(evs2, actions++newActions, pred++newPred, (loop._1,seed2)) , realEvent)

  /** Go through the actions and, for every ev->act with ev a generator, ADD gen(nEv)->act */
  def adaptActions(genEvs: Map[Event, Event]): Actions =
    for ((e,act)<-actions; nEv<-genEvs.get(e)) yield nEv->act

  ///** Create new orders for the generated events: predecessors and sucessors (latter only in the loops)  */
  //def adaptPred(genEvs: Map[Event, Event]): Order =
  //  // 1. Generated vs. existing: go through the order and, for every e2<e1 with e2 a generator, ADD gen(e2)<e1
  //  val newPred1: Order = for (e2,e1s) <- pred yield
  //    val newE1s: Set[Event] = for (e1<-e1s; ne1<-genEvs.get(e1)) yield ne1
  //    e2 -> (e1s ++ newE1s)
  //  //val newPred1: Order = for (e2,e1) <- pred ; ne2 <- genEvs.get(e2) yield ne2->e1
  //  // 2. Generated vs. Loops: go through the generated and, forall newE<-e, eDep<-loopInfo(e), ADD newE<eDep
  //  val newPred2a:List[(Event,Event)] = for ((newE,e) <- genEvs.toList; eDep <- loop._1.getOrElse(e,Set())) yield
  //    eDep->newE
  //    //newE->eDep
  //  val newPred2b:List[(Event,Event)] = for (newE,e) <- genEvs.toList yield
  //    e->newE
  //    //newE->e
  //  val newPred2c = (newPred2a++newPred2b).foldLeft[Order]
  //    (Map()) ((m,pair) => add(pair,m))
  //  // 3. combine both sides
  //  add(newPred1,newPred2c)

  /** Create new orders for the generated events: predecessors and sucessors (latter only in the loops)  */
  def adaptPred(genEvs: Map[Event, Event]): Order =
    // 1. Generated vs. existing: go through the order and, for every e2<e1 and ADD e2<e1 and e2<gen(e1) (if exists)
    val newPred1: Order = for (e2,e1s) <- pred yield
      val newE1s: Set[Event] = for (e1<-e1s; ne1<-genEvs.get(e1)) yield ne1
      e2 -> (e1s ++ newE1s)
    // 1b. go through the order and  for every e2<e1 with e2 a generator, ADD gen(e2)<genEvs.getOrElse(e1,e1)
    val newPred1b:Order = for (e2,e1s) <- pred ; ge2 <- genEvs.get(e2) yield
      ge2 -> (e1s.map(e1=>genEvs.getOrElse(e1,e1)))
    // 2. Generated vs. Loops: go through the generated and, forall newE<-e, eDep<-loopInfo(e), ADD newE<eDep
    // fixed order: (e,newE) instead of (newE,e)
    val newPred2a:List[(Event,Event)] = for ((e,newE) <- genEvs.toList; eDep <- loop._1.getOrElse(e,Set())) yield
      eDep->newE
    val newPred2b:List[(Event,Event)] = for (e,newE) <- genEvs.toList yield
      e->newE
    val newPred2c = (newPred2a++newPred2b).foldLeft[Order]
      (Map()) ((m,pair) => add(pair,m))
    // 3. combine all sides
    add(newPred1b,add(newPred1,newPred2c))

  /** Calculate the real predecessors of an event, skiping over elements of the order not in the NPomset */
  def realPred(e: Event): Set[Event] =
   pred.getOrElse(e,Set())
     .flatMap(e0=>if events.toSet contains e0 then Set(e0) else realPred(e0))

  /** Calculate ALL real predecessors of an event, skiping over elements of the order not in the NPomset */
  def allRealPred(e: Event): Set[Event] =
    val next = realPred(e)
    next ++ next.flatMap(allRealPred)

  /** Refines (minimally) a nested set of events to drop a set of events.
   * Returne None if the events cannot be dropped. */
  def dropEvents(es:Set[Event],n:Events): Option[Events] =
    if n.acts.intersect(es).nonEmpty then None
    else
      val newChoices:Set[Events] = for c <- n.choices yield
        (dropEvents(es,c.left),dropEvents(es,c.right)) match
          case (None,None) => return None // no need to continue
          case (Some(a),None) => a
          case (None,Some(b)) => b
          case (Some(a),Some(b)) => Nesting(Set(),Set(NChoice(a,b)),Set())
      val newLoops: Set[Events] = n.loops.filter(n => (n.toSet intersect es).isEmpty)

      val baseResult = Nesting(n.acts,Set(),newLoops)
      val joinChoices = newChoices.fold(baseResult)(_++_)
      Some(joinChoices)

  /** Refines (minimally) a nested set of events to lift a given event up.
   * Returns None if the event is not found. */
  def select(event: NPomset.Event,n:Events,seed:Event): Option[(Events,Map[Event,Event],Event)] =
    if n.acts.contains(event)
      then Some(n,Map(),seed)
    else
      // traversing choices and loops while upating "found,next,genEvs".
      var found = false
      var next = seed
      var genEvs: Map[Event,Event] = Map() // new generated events mapped from their originals
      // 1. Choices
      val newChoices = for c<-n.choices yield
        val (evs2,genEvs2,next2,found2) = selectChoice(event,c,next)
        found = found || found2
        next = next2
        genEvs ++= genEvs2
        evs2
      // 2. Loops
      val newLoops = for l<-n.loops yield
        selectLoop(event,l,next) match
          case None => Nesting(Set(),Set(),Set())
          case Some((evs2,genEvs2,next2)) =>
            found = true
            next = next2
            genEvs ++= genEvs2
            evs2
      // 3. compile result
      if !found then None
      else
        val jointEvents = (newChoices++newLoops).fold(Nesting(n.acts,Set(),n.loops))(_++_)
        Some(jointEvents, genEvs, next)

//        // recall result type
//        type R = (Events,Map[Event,Event],Event)
//        val baseResult: R =
//          ( Nesting(n.acts,Set(),n.loops) , genEvs , next )
//        def join(res:R,evs:Events) = (res._1++evs,res._2,res._3)
//        // join newChoices and newLoops to BaseResult
//        Some((newChoices++newLoops).foldLeft[R](baseResult)(join))

  /////

//      var next = seed
//      // todo: val newChoices: Option[(Evs,NewEvs,NewSeed)] = for c<-n.choices; selectChoices(c)
//      // selectChoices: given a choice, returns the refined version + bool (if it found)
//      val newChoices = for c <- n.choices yield
//        (select(event,c.left),select(event,c.right)) match
//          case (None,None) => Nesting(Set(),Set(c),Set())
//          case (Some(a),_) => {found=true; a}
//          case (_,Some(b)) => {found=true; b}
////      val newLoops = for c <- n.loops yield c // todo: now for loops
//      if !found then None
//      else
//        type R = (Events,Map[Event,Event])
//        def join(p1:R,p2:R) = (p1._1++p2._1,p1._2++p2._2)
//        Some(newChoices.fold[R]((Nesting(n.acts,Set(),Set()),Map()))(join))

  private def selectChoice(e:Event,c:NChoice[Event],seed:Event):
      (Events,Map[Event,Event],Event,Boolean) = // true if found
    select(e,c.left,seed) match
      case Some(es1,n1,s1) => (es1,n1,s1,true)
      case None => select(e,c.right,seed) match
        case Some(es2,n2,s2) => (es2,n2,s2,true)
        case None => (Nesting(Set(),Set(c),Set()),Map(),seed,false)

  private def selectLoop(e:Event,loop:Events,seed:Event):
      Option[(Events,Map[Event,Event],Event)] = // true if found
    select(e,loop,seed) match
      case None => None
      case Some((es1,new1,s1)) =>
        var next = s1
        val newEvents = for e <- es1.toSet yield
          next += 1
          e -> next
        val nest2: Events = es1.map(newEvents.toMap.apply)
        val nest3 = Nesting(nest2.acts,nest2.choices,nest2.loops+loop)
        Some((nest3,new1++newEvents.toMap,next))


  ///////////////////

  def accepting: Boolean = canTerminate(events)
  private def canTerminate(es: Events): Boolean =
    es.acts.isEmpty && es.choices.forall(canTerminate)
  private def canTerminate(ch: NChoice[Event]): Boolean =
    canTerminate(ch.left) || canTerminate(ch.right)

  //////////////////
  // Auxiliary
  //////////////////
  override def toString: String =
//    val evs = events.toSet
    val sEv = pretty(events)
    val sAct = actions.map((a,b)=>s"$a:$b").mkString(",")
    val sOrd = (for ((a,bs)<-pred; b<-bs) yield s"$b<$a").mkString(",")
    val sLoop = (for ((a,bs)<-loop._1; b<-bs) yield s"$a'<$b").mkString(",")
    val sSeed = loop._2
    List(sEv,sAct,sOrd,sLoop,sSeed).filterNot(_=="").mkString(" | ")

  private def pretty(e:Events): String =
    (e.acts.map(_.toString).toList ++
      e.choices.map(c => s"[${pretty(c.left)}+${pretty(c.right)}]").toList ++
      e.loops.map(l=> s"(${pretty(l)})*")).toList
      .mkString(",")

  /** True if it has no events */
  def isEmpty = events.toSet.isEmpty


object NPomset:
  type Event = Int
  type Actions = Map[Event,Action]
  type Order = MS[Event,Event] // Map[Event,Set[Event]]
  type Events = Nesting[Event]

  /** Isomorphic to List[(A,B)], indexed on A */
  type MS[A,B] = Map[A,Set[B]]
  def mapset[A,B](a:A,b:B) = Map(a->Set(b))
  def mapset[A,B](abs:Iterable[(A,B)]) = add(abs,Map())
  def add[A,B](ab:(A,B),m:MS[A,B]): MS[A,B] =
    val (a,b) = ab
    if m contains a then m+(a->(m(a)+b)) else m+(a->Set(b))
  def add[A,B](abs:Iterable[(A,B)],m:MS[A,B]): MS[A,B] =
    abs.foldRight(m)((ab,prev)=> add(ab,prev))
  def add[A,B](m1:MS[A,B], m2:MS[A,B]): MS[A,B] =
    m1 ++ (for (a,bs)<-m2 yield
      if m1 contains a then a->(m1(a)++bs) else a->bs)

  /** Nested sets: with choices and loops structures that can be refined */
  case class Nesting[A](acts:Set[A], choices:Set[NChoice[A]],loops:Set[Nesting[A]]):
    lazy val toSet:Set[A] = acts ++ choices.flatMap(_.toSet) ++ loops.flatMap(_.toSet)
    def --(as:Set[A]):Nesting[A] = Nesting(acts--as,choices.map(_--as),loops)
    def ++(other:Nesting[A]): Nesting[A] = Nesting(acts++other.acts,choices++other.choices,loops++other.loops)
    def or(other:Nesting[A]): Nesting[A] = Nesting(Set(),Set(NChoice(this,other)),Set())
    def map[B](f:A=>B):Nesting[B] = Nesting(acts.map(f),choices.map(_.map(f)),loops.map(_.map(f)))

  case class NChoice[A](left:Nesting[A],right:Nesting[A]):
    lazy val toSet:Set[A] = left.toSet ++ right.toSet
    def --(as:Set[A]):NChoice[A] = NChoice(left--as,right--as)
    def map[B](f:A=>B):NChoice[B] = NChoice(left.map(f),right.map(f))

  /** Information needed to unfold loops: order between instances, and seed to generate events */
  type LoopInfo = (Order,Event) // inner order of loops and seed to generate events
  def join(l1:LoopInfo,l2:LoopInfo): LoopInfo = (l1._1++l2._1,l1._2 max l2._2)
  def loopInfo(e1:Event,e2:Event,seed:Event=0): LoopInfo = (mapset(e1,e2),seed)
  def noLoopInfo: LoopInfo = (Map(),0)
  def add(l1:LoopInfo,l2:LoopInfo): LoopInfo = (add(l1._1,l2._1), l1._2 max l2._2)


  def empty = NPomset(Nesting(Set(),Set(),Set()),Map(),Map(),(Map(),0))

  val nex = Nesting(Set(1,2,3),Set(NChoice(Nesting(Set(4,5),Set(),Set()),Nesting(Set(6,7),Set(),Set()))),Set()) // 1,2,3,[4,5+6,7]
  val pex = NPomset(nex,Map(1->Out(Agent("a"),Agent("b")),4->In(Agent("b"),Agent("a"))), add((2,1),mapset(4,3)), (Map(),0))
  import choreo.Examples._
  val ex2 = Choreo2NPom(((a->d) + (b->d)) > (a->d))

  def getEx(e:String) = Choreo2NPom(Examples.examples2show.find(_._1==e)
    .getOrElse("",choreo.syntax.Choreo.End)._2)
  val ex3 = getEx("ex30")
  val ex4 = Choreo2NPom(DSL.loop((a!b)>(a!c))>(a!d))
  val ex5 = NPomDefSOS.next(ex4).tail.head._2



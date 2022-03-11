package choreo.api

import choreo.api
import choreo.api.Code
import choreo.api.EventsCtx.EventCtx
import choreo.api.MiniScala.*
import choreo.api.NPom2SessionCtx.*
import choreo.api.LocalAPI.*
import choreo.api.SessionAPI.mkImports
import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.Event
import choreo.syntax.Choreo.{In, Out, agents}
import choreo.syntax.{Agent, Msg}

import scala.collection.immutable.HashMap

case class SessionAPI(modules:List[ScalaModule],globalImports:ScalaModule):
  def getModulesNameWithCode:List[(String,String)] =
    modules.map(m=> (m.name,m.toString))

  override def toString: String =
    globalImports.toString ++ "\n\n" ++ modules.map(m=>m.toString).mkString("\n\n")


object SessionAPI:

  type InOut = In | Out

  def apply(npom:NPomset):SessionAPI =
    val ctx       = api.NPom2SessionCtx(npom)
    val localAPIs = for (a,roleCtx) <- ctx.roles yield LocalAPI(roleCtx)
    val modules = localAPIs.map(api=>ScalaModule(api.clas.name,Statements(api.clas::api.co::Nil)))
    val extras = mkRoles(ctx)::mkNetwork(ctx)::mkProtocol(ctx)::mkMsgs(ctx)::mkUtils()::Nil
    SessionAPI(modules.toList++extras,mkImports(ctx))

  /**
   * Replaces every event in the list with its corresponding value (e2v) if it exists, or
   * the default value (default) if undefined.
   * @param events list of order events
   * @param evid the corresponding pattern value for each event
   * @return a list with the corresponding pattern value for each event,
   *         or "_" if undefined.
   */
  def mkPattern(events:List[Event], e2v:Event2Value, default:Event2Value):List[String] =
    for e <- events yield
      if e2v.contains(e) then
        e2v(e)
      else default(e)

  def mkCaseComment(eventCtx:EventCtx):String =
    s"""${eventCtx.e}:${eventCtx.act.toString}"""

  def mkCaseComment(f:ForkInfo):String =
    s"""fork at ${f.point.toString} with branches ${f.post.map(r=>r.id).mkString(",")}"""

  def mkComment(j:JoinInfo):String =
    s"""join at ${j.point.toString}"""

  def mkUtils():ScalaModule =
    val code =
      s"""object SessionUtils:
         |  type Channel = java.util.concurrent.ConcurrentLinkedQueue[Any]
         |
         |  def out(c: Channel, v: Any) = c.offer(v)
         |
         |  def in(ccc: Channel*) =
         |    var (i, c, v) = (0, ccc(0), ccc(0).poll())
         |    while v == null do
         |      i = (i + 1) % ccc.size
         |      c = ccc(i)
         |      v = ccc(i).poll()
         |    (c, v)
         |
         |  trait UseOnce:
         |    var used = false
         |    def use = if used then throw new Exception() else used = true
         |
         |  type Cont = (Any, Any, Any) => Any
         |
         |  //
         |  // Generic: Match Types
         |  //
         |
         |  type RemoveUnit[X] = X match
         |    case EmptyTuple | Unit  => EmptyTuple
         |    case Unit *: t          => RemoveUnit[t]
         |    case h *: t             => h *: RemoveUnit[t]
         |    case _                  => X
         |
         |  type Simplify[X] = RemoveUnit[X] match
         |    case EmptyTuple      => Unit
         |    case h *: EmptyTuple => h
         |    case h *: t          => RemoveUnit[X]
         |    case _               => X
         |
         |  type IfThenElse[A, B, C] = A match
         |    case true  => B
         |    case false => C
         |
         |  def ifThenElse[A, B, C](a: A, b: B, c: C): IfThenElse[A, B, C] = a match
         |    case _: true  => b
         |    case _: false => c
         |
         |  type And[A, B] = (A, B) match
         |    case (true, true)   => true
         |    case (true, false)  => false
         |    case (false, true)  => false
         |    case (false, false) => false
         |
         |  def and[A, B](a: A, b: B): And[A, B] = (a, b) match
         |    case (_: true, _: true): (true, true)     => true
         |    case (_: true, _: false): (true, false)   => false
         |    case (_: false, _: true): (false, true)   => false
         |    case (_: false, _: false): (false, false) => false
         |
         |  type IsError[A] = A match
         |    case Error => true
         |    case Any   => false
         |
         |  def isError[A](a: A): IsError[A] = a match
         |    case _: Error => true
         |    case _: Any   => false
         |
         |""".stripMargin
    ScalaModule("SessionUtils",PreCode(code))


  def mkImports(ctx:SessionCtx):ScalaModule =
    val imports:List[Statement] =
      PreCode(
        s"""import SessionUtils.*
           |import Roles.*
           |import Messages.*
           |import scala.concurrent.{Future, Await}
           |import scala.concurrent.duration.Duration
           |import scala.concurrent.ExecutionContext.Implicits.global
           |""".stripMargin
      ) ::
        (for n <- ctx.roles.values.map(a=>a.name) yield
          Import(n++".*")).toList
    ScalaModule("GlobalImports",NoSepStatements(imports))

  def mkNetwork(globalCtx: SessionCtx):ScalaModule =
    val imp = Import("SessionUtils.*")
    val chs = (globalCtx.ins ++ globalCtx.outs).map(i=>chanName(i))
    val sts = for c <- chs.toSet yield VarDef(s"val $c",None,"new Channel")
    ScalaModule("Network",
      ScalaClass("Network",List(),Nil,
        Some(Statements(imp::NoSepStatements(sts.toList)::Nil)),None,Nil,false))

  def mkRoles(globalCtx: SessionCtx):ScalaModule =
    val agents = globalCtx.roles.map(o=>o._1)
    var sts = List[Statement]()
    for a <- agents do
      sts ++= ScalaObject(roleName(a),None,None)::
        TypeDef(TName(roleName(a)),TName(roleName(a)++".type"))::Nil
    val st = NoSepStatements(sts)
    ScalaModule("Roles",ScalaObject("Roles",Some(st),None))

  def mkMsgs(globalCtx: SessionCtx):ScalaModule =
    var sts = List[Statement]()
    for
      m <- globalCtx.msgs
    yield
      sts ++= ScalaObject(msgName(m),None,None)::
        TypeDef(TName(msgName(m)),TName(msgName(m)++".type"))::Nil
    val st = NoSepStatements(sts)
    ScalaModule("Messages",ScalaObject("Messages",Some(st),None))

  def mkProtocol(globalCtx: SessionCtx):ScalaModule =
    val imports =
      Import("scala.annotation.targetName")::
        Import("SessionUtils.*")::Nil ++
        (for n <- globalCtx.roles.values.map(a=>a.name) yield
          Import(n++".*")).toList
    val net = VarDef("val net",None,"new Network")
    val runObjs = mkRunObjects(globalCtx)
    val runs = mkRuns(globalCtx)
    val st = Statements(NoSepStatements(imports)::net::Nil++runObjs++runs)
    ScalaModule("Protocol",ScalaClass("Protocol",List(),Nil,Some(st),None,Nil,false))

  def mkRuns(ctx: SessionCtx):List[Statement] =
    for a <- ctx.roles.values.toList yield
      ScalaObject(s"Run${a.name}",None,None,"UseOnce"::Nil)

  def mkRunObjects(ctx: SessionCtx):List[Statement] =
    for a <- ctx.roles.values.toList yield
      val name = s"run${a.name}"
      val params = mkRunParam(a)::Nil
      val st = mkRunSt(a)
      MethodDef("run",Nil,params,Set(),None,st,None,Some(s"""@targetName("$name")"""))

  def mkRunSt(agentCtx: RoleCtx):Statement =
    NoSepStatements(
      Variable(s"Run${agentCtx.name}.use")
        :: PreCode(
          s"""val thread = new Thread(() => {
             |  f(${mkInitTypeInstance(agentCtx)});
             |  ()
             |  })""".stripMargin
        ):: FunCall("thread.start",Nil,Nil)::Nil
//        VarDef("val thread",None,s"""new Thread(() => { f(${agentCtx.name}.start(net)); () })""")::

    )

  def mkRunParam(agentCtx: RoleCtx):Param =
    val from = TName(agentCtx.name++s".${agentCtx.name}$$Init")
    val to = //mkDefaultFinalType(agentCtx)
     TName(agentCtx.name++s".${agentCtx.name}$$Final")
    Param("f",TFun(from,to))


  /* Naming conventions */
  def roleName(a:Agent):String        = /*"Role"++*/a.s.capitalize
  def msgName(m:Msg):String           = m.names.capitalize
  def chanName(action:In|Out):String  = action match
    case In(a,b,_)  => b.s.capitalize++a.s.capitalize
    case Out(a,b,_) => a.s.capitalize++b.s.capitalize
  def sbj(a:InOut):Agent = a match
    case In(_,b,_)  => b
    case Out(_,b,_) => b
  def msgName(a:InOut):String = a match
    case In(_,_,m)  => msgName(m)
    case Out(_,_,m) => msgName(m)
  def className(a:Agent):String = a.s.capitalize

  def msg(a:InOut):Msg = a match
    case In(_,_,m) => m
    case Out(_,_,m) => m


package choreo.gen

import choreo.api.Code
import choreo.api.MiniScala.*
import choreo.gen.NPom2SessionCtx
import choreo.gen.NPom2SessionCtx.*
import choreo.gen.SessionAPI.ScalaModule
import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.Event
import choreo.syntax.{Agent, Msg}
import choreo.syntax.Choreo.{In, Out}

import scala.collection.immutable.HashMap

case class SessionAPI(modules:List[ScalaModule]):
  def modulesToCode:List[(String,String)] =
    modules.map(m=> (m.name,m.toString))

object SessionAPI:

  case class ScalaModule(name:String, statements:Statement) extends Code:
    def toCode(implicit i: Int): String =
      statements.toCode

  type InOut = In | Out

  def apply(npom:NPomset):SessionAPI =
    val ctx       = NPom2SessionCtx(npom)
    val localAPIs = for (a,roleCtx) <- ctx.roles yield LocalAPI(roleCtx)
    val modules = localAPIs.map(api=>ScalaModule(api.clas.name,Statements(api.clas::api.co::Nil)))
//    val extras = mkRoles(ctx)::mkNetwork(ctx)::mkProtocol(ctx)::mkMsgs(ctx)::mkUtils()::Nil
    SessionAPI(modules.toList)//modules)

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


  /* Naming conventions */
  def roleName(a:Agent):String        = "Role"++a.s.capitalize
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

//
//
//
//import choreo.syntax.Choreo.*
//import choreo.syntax.Msg
//
//class Session:
//
//  val participants: Set[SessionParticipant]
////  val messages:Set[Message]
////  val channels:Set[Channel]
//
//object Session:
//
//  type Action = In | Out
//
//  def msg[A<:Action](a:A):Msg = a match
//    case In(_,_,m)  => m
//    case Out(_,_,m) => m
//
//
//case class SessionParticipant(
//  name:    String,
//  inOutActions: Map[Int, Action],
//  order: Order
//  //  forkInfo:
//):
//
//  lazy val messageNames:Set[String] =
//    inOutActions.values.map(a=>Session.msg(a).toString)
//
//  lazy val inputs:List[In] =
//    inOutActions.collect({case a:In => a}).toList
//
//
//case class SessionAction(act:Action,  preConditions:)
//
//case class RecvMethod(actions:List[Action],)
//
//case class MatchType(name:string, )
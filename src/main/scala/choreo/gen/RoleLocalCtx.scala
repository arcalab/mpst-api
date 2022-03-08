package choreo.gen


import choreo.gen.NPom2SessionCtx.{Event2Value, ForkJoin}
import choreo.api.MiniScala.{Param, TName, TVar}
import choreo.gen.SessionAPI.*
import choreo.gen.EventsCtx.*
import choreo.npomsets.NPomset.*
import choreo.syntax.Agent
import choreo.syntax.Choreo.{In, Out}

// todo: Bring back fork information
case class RoleLocalCtx(
  name:String,
  agent:Agent,
  eventsCtx:List[EventCtx],
  extEventsCtx:List[ExtEventCtx],
  forkJoin:Option[ForkJoin] = None
):

  lazy val events:List[Event] = eventsCtx.map(_.e) ++ extEventsCtx.map(_.e)
  lazy val event2Param:Map[Event,String] =
    eventsCtx.map(e=>e.e->e.param).toMap ++ extEventsCtx.map(e=>e.e->e.param).toMap
  lazy val event2TVar:Map[Event,String] =
    eventsCtx.map(e=>e.e->e.tVar.name).toMap ++ extEventsCtx.map(e=>e.e->e.tVar.name).toMap

  lazy val parameters:List[Param] =
    eventsCtx.map(_.getParam) ++ extEventsCtx.map(_.getParam)
  lazy val parametersName:List[String] =
    eventsCtx.map(_.param) ++ extEventsCtx.map(_.param)

  lazy val typeVariables:List[TVar] =
    eventsCtx.map(_.tVar) ++ extEventsCtx.map(_.tVar)
  lazy val typeVariablesName:List[String] =
    eventsCtx.map(_.tVar.name) ++ extEventsCtx.map(_.tVar.name)

  lazy val (getAllRecvEventsCtx,getAllSendEventsCtx): (List[EventCtx],List[EventCtx]) =
    eventsCtx.foldLeft[(List[EventCtx],List[EventCtx])](Nil,Nil)({
      case ((rec,send),e) => e.act match
        case a:In => (rec:+e,send)
        case a:Out => (rec,send:+e)
    })









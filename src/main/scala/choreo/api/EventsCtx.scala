package choreo.api

import choreo.api.MiniScala.{Param, TName, TVar}
import choreo.npomsets.NPomset.Event
import choreo.api.SessionAPI.*
import choreo.api.NPom2SessionCtx.*

object EventsCtx:

  case class EventCtx(
    e: Event,
    act: InOut,
    pre: Event2Value,
    post: Event2Value,
    param: String,
    tVar: TVar,
    initialVal: String
  ):
    def getParam: Param = Param(param, TName(tVar.name))

  case class ExtEventCtx(e: Event, param: String, tVar: TVar, initialVal: String):
    def getParam: Param = Param(param, TName(tVar.name))

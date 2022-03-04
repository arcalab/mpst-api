package choreo.gen

import choreo.api.GlobalCtx.Event2Value
import choreo.gen.Session.*
import choreo.npomsets.NPomset.*
import choreo.api.MiniScala.{Param, TName, TVar}


object EventsCtx:

  case class EventCtx(
    e:Event,
    act:InOut,
    pre:Event2Value,
    post:Event2Value,
    param:String,
    tVar:TVar,
    initialVal:String
  ):
    def getParam:Param = Param(param,TName(tVar.name))


  case class ExtEventCtx(e:Event,param:String,tVar:TVar,initialVal:String):
    def getParam:Param = Param(param,TName(tVar.name))

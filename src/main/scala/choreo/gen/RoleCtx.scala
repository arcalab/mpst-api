package choreo.gen

import choreo.gen.RoleLocalCtx
import choreo.gen.EventsCtx.{EventCtx, ExtEventCtx}
import choreo.syntax.{Agent,Msg}
import choreo.gen.SessionAPI.*

//case class SessionCtx

case class RoleCtx(
  name:String,
  agent:Agent,
  localCtx:List[RoleLocalCtx]
):
  def forkes:Boolean =
    localCtx.size == 1 && localCtx.head.forkJoin.isDefined

  lazy val ins:List[InOut]  = for l <- localCtx ; e <- l.getAllRecvEventsCtx yield e.act
  lazy val outs:List[InOut] = for l <- localCtx ; e <- l.getAllSendEventsCtx yield e.act
  lazy val msgs:List[Msg]   = (for a <- (ins++outs) yield msg(a)).distinct

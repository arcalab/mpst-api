package choreo.gen

import choreo.gen.RoleLocalCtx
import choreo.gen.EventsCtx.{EventCtx, ExtEventCtx}
import choreo.syntax.Agent

//case class SessionCtx

case class RoleCtx(
  name:String,
  agent:Agent,
  localCtx:List[RoleLocalCtx]
)
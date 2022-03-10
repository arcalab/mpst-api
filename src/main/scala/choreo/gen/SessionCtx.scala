package choreo.gen

import choreo.syntax.{Agent,Msg}
import choreo.gen.RoleCtx
import choreo.gen.SessionAPI.*

case class SessionCtx(roles:Map[Agent,RoleCtx]):

  lazy val ins:List[InOut]  = this.roles.flatMap(r=> r._2.ins).toList
  lazy val outs:List[InOut] = this.roles.flatMap(r=> r._2.outs).toList
  lazy val msgs:List[Msg]   = (this.roles.flatMap(r=> r._2.msgs).toList).distinct



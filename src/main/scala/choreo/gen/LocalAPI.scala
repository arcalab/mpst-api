package choreo.gen

import choreo.api.MiniScala.*
import choreo.gen.SessionAPI.*

case class LocalAPI(clas:ScalaClass,co:ScalaObject)

object LocalAPI:

  def apply(ctx:RoleCtx):LocalAPI =
    val (clas, types) = mkClass(ctx)
    val co            = mkCO(ctx,types)
    LocalAPI(clas,co)

  val stateVar = "x"
  val stateTVar = "X"
  val localVar  = "v"

  protected def mkClass(ctx: RoleCtx):(ScalaClass,Statement) =
    val name        = ctx.name //className(ctx.agent)
    val typeVars    = mkTVars(ctx)
    val parameters  = mkParams(ctx)
    val methodsWithMT = GlobalMethod(ctx)
    val methods       = Statements(methodsWithMT.map(g=>g.methodDef))
    val types         = methodsWithMT.map(g=>g.methodTypeTDef)
//      Statements(GlobalMethod(ctx).map(g=>g.getStatements()))
    val comment     = ""
    val extendsWith = "UseOnce"
    val clas = ScalaClass(name,typeVars,parameters,Some(methods),Some(comment),extendsWith::Nil)
    (clas,Statements(types))

  protected def mkTVars(ctx:RoleCtx):List[(String,Option[String])] =
    for i <- (1 to ctx.localCtx.size).toList yield (stateTVar+i, Option.empty[String])

  protected def mkParams(ctx:RoleCtx):List[Param] =
    val stParams =
      for i <- (1 to ctx.localCtx.size).toList yield
        Param(stateVar+i,TName(stateTVar+i))
    stParams:+Param("net",TName("Network"))

  protected def mkCO(ctx:RoleCtx,typeDefs:Statement):ScalaObject =
    val localMethods = mkLocalMethods(ctx)
    val statements = Statements(typeDefs::localMethods.map(l=>l.getStatements()))
    /// mk statement with match type, method
    // mk init and end alias for each local + global
    ScalaObject(ctx.name,Some(statements),None,Nil) // todo

  protected def mkLocalMethods(ctx:RoleCtx):List[LocalMethod] =
    ctx.localCtx.foldLeft[List[LocalMethod]](Nil)({
      case (acc,lc) => acc++mkLocalMethods(lc)
    })

  protected def mkLocalMethods(localCtx:RoleLocalCtx):List[LocalMethod] =
    LocalMethod.localSendFrom(localCtx)
      :: LocalMethod.localRecvFrom(localCtx)
      :: Nil

  def mkEndType(ctx:RoleCtx):TExp =
    TName(className(ctx.agent)+"$Final",None)
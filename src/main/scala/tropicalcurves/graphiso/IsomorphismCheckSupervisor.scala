//package tropicalcurves.graphiso
//
//import akka.actor.typed.Behavior
//import akka.actor.typed.scaladsl.AbstractBehavior
//import akka.actor.typed.scaladsl.ActorContext
//import akka.actor.typed.scaladsl.Behaviors
//
//object IsomorphismCheckSupervisor {
//  def apply(): Behavior[IsoMessage] = Behaviors.setup[IsoMessage](context => new IsomorphismCheckSupervisor(context))
//}
//
//class IsomorphismCheckSupervisor(context: ActorContext[IsoMessage]) extends AbstractBehavior[IsoMessage](context) {
//
//  override def onMessage(msg: IsoMessage): Behavior[IsoMessage] = msg match {
//    case ReduceGraphsByIsomorphism(graphs) =>
//      println("Got some graphs, yo!")
//      graphs foreach println
//      this
//    case _ => this
//  }
//
//}

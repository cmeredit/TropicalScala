//package tropicalcurves.graphiso
//
//import akka.actor.typed.Behavior
//import akka.actor.typed.scaladsl.AbstractBehavior
//import akka.actor.typed.scaladsl.ActorContext
//import akka.actor.typed.scaladsl.Behaviors
//
//import tropicalcurves.puregraphs.Graph
//
//object IsomorphismCheckWorker {
//  def apply[A, B](isotypes: Vector[Graph[A, B]]): Behavior[IsoMessage] =
//    Behaviors.setup[IsoMessage](context => new IsomorphismCheckWorker(context, isotypes))
//}
//
//class IsomorphismCheckWorker[A, B](context: ActorContext[IsoMessage], val isotypes: Vector[Graph[A, B]]) extends
//  AbstractBehavior[IsoMessage](context) {
//
//  override def onMessage(msg: IsoMessage): Behavior[IsoMessage] = {
//    msg match {
//      case _ => this
//    }
//  }
//
//}

package com.cotitan.server

import akka.actor.{Props, ActorRef}
import spray.can.websocket.WebSocketServerWorker
import spray.can.websocket.frame.TextFrame
import spray.routing.HttpServiceActor

object WebSocketClient {

  def props(serverConnection: ActorRef) = Props(classOf[WebSocketClient], serverConnection)

}

final case class WebSocketClient(serverConnection: ActorRef) extends HttpServiceActor with WebSocketServerWorker {

  override def businessLogic: Receive = {
    case t: TextFrame => {
      sender() ! t
    }
    case x => {
      println(s"object: ${x}")
    }
  }

}

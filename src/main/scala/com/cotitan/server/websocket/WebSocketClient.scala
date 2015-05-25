package com.cotitan.server.websocket

import akka.actor.{ActorRef, Props}
import spray.can.websocket.WebSocketServerWorker
import spray.can.websocket.frame.TextFrame
import spray.routing.HttpServiceActor

object WebSocketClient {

  def props(serverConnection: ActorRef) = Props(classOf[WebSocketClient], serverConnection)

}

final case class WebSocketClient(serverConnection: ActorRef) extends HttpServiceActor with WebSocketServerWorker {

  override def businessLogic: Receive = {
    case t: TextFrame => {
      val data = t.payload.decodeString("UTF-8")
      println(s"payload: $data")

      // SHOULD ROUTE PAYLOAD HERE

      sender() ! t
    }
    case x => {
      println(s"object: ${x}")
    }
  }

}

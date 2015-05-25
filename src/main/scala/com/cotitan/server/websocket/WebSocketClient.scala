package com.cotitan.server.websocket

import akka.actor.{ActorRef, Props}
import spray.can.websocket.WebSocketServerWorker
import spray.can.websocket.frame.TextFrame
import spray.routing.HttpServiceActor

object WebSocketClient {

  def props(serverConnection: ActorRef, router: ActorRef) = Props(classOf[WebSocketClient], serverConnection, router)

}

final case class WebSocketClient(serverConnection: ActorRef, router: ActorRef) extends HttpServiceActor with WebSocketServerWorker {

  override def businessLogic: Receive = {
    case t: TextFrame => {
      val data = t.payload.decodeString("UTF-8")

      router ! data

      sender() ! t
    }
    case x => {
      println(s"object: ${x}")
    }
  }

}

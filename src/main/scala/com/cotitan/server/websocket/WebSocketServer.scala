package com.cotitan.server.websocket

import akka.actor.{Actor, ActorLogging, Props}
import spray.can.Http

object WebSocketServer {

  def props() = Props(classOf[WebSocketServer])

}

final case class WebSocketServer() extends Actor with ActorLogging {

  override def receive = {
    case Http.Connected(remote, local) => {
      println(s"New connection from ${remote.getHostString}.");

      val serverConnection = sender()
      val connection = context.actorOf(WebSocketClient.props(serverConnection))
      serverConnection ! Http.Register(connection)
    }
  }

}

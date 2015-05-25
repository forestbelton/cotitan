package com.cotitan.server

import akka.actor.{ActorLogging, Props, Actor}
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

package com.cotitan.server.websocket

import akka.actor.{ActorRef, Actor, ActorLogging, Props}
import spray.can.Http

final case class WebSocketServer(router: ActorRef) extends Actor with ActorLogging {

  override def receive = {
    case Http.Connected(remote, local) => {
      println(s"New connection from ${remote.getHostString}.");

      val serverConnection = sender()
      val connection = context.actorOf(WebSocketClient.props(serverConnection, router))
      serverConnection ! Http.Register(connection)
    }
  }

}

object WebSocketServer {

  def props(router: ActorRef) = Props(classOf[WebSocketServer], router)

}

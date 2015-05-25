package com.cotitan.server

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import com.cotitan.server.websocket.WebSocketServer
import spray.can.Http
import spray.can.server.UHttp

object Cotitan {

  def main(args: Array[String]) {
    println("Starting cotitan server...")

    implicit val system = ActorSystem()

    val router = system.actorOf(Props(classOf[Router]))
    val server = system.actorOf(WebSocketServer.props(router), "websocket")

    IO(UHttp) ! Http.Bind(server, "localhost", 8400)
  }

}


package com.cotitan.server

import akka.actor.ActorSystem
import akka.io.IO
import spray.can.Http
import spray.can.server.UHttp

object Cotitan {

  def main(args: Array[String]) {
    println("Starting cotitan server...")

    implicit val system = ActorSystem()

    val server = system.actorOf(WebSocketServer.props(), "websocket")

    IO(UHttp) ! Http.Bind(server, "localhost", 8400)
  }

}


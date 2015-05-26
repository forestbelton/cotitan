package com.cotitan.server.routing

import akka.actor.Actor
import com.cotitan.server.routing.Payload.Hello

final class Router() extends Actor {

  override def receive = {
    case Packet(_, Hello(name)) => {
      println(s"router received: hello from $name")
    }
  }

}

package com.cotitan.server

import akka.actor.Actor

final class Router() extends Actor {

  override def receive = {
    case x => {
      println(s"router received: ${x}")
    }
  }

}

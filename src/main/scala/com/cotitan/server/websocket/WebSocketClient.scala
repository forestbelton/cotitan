package com.cotitan.server.websocket

import akka.actor.{ActorRef, Props}
import com.cotitan.server.routing.Packet
import com.plasmaconduit.validation.{Success, Failure}
import spray.can.websocket.WebSocketServerWorker
import spray.can.websocket.frame.TextFrame
import spray.routing.HttpServiceActor

final case class WebSocketClient(serverConnection: ActorRef, router: ActorRef) extends HttpServiceActor with WebSocketServerWorker {

  override def businessLogic: Receive = {
    case t: TextFrame => {
      Packet.parse(t.payload.decodeString("UTF-8")) match {
        case Success(packet) => {
          router ! packet
        }
        case Failure(f) => {
          println(f)
          sender() ! t // SHOULD RETURN ERROR BACK TO USER
        }
      }
    }
    case x => {
      println(s"object: ${x}")
    }
  }

}

object WebSocketClient {

  def props(serverConnection: ActorRef, router: ActorRef) = Props(classOf[WebSocketClient], serverConnection, router)

}

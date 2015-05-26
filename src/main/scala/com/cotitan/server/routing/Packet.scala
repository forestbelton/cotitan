package com.cotitan.server.routing

import com.cotitan.server.routing.Payload.Hello
import com.plasmaconduit.json.{JsonParser, JsObject, JsReader, JsValue}
import com.plasmaconduit.validation.{Success, Failure, Validation}

final case class Packet(route: String, payload: Payload) // should contain identification information

object Packet {

  private implicit object PacketReader extends JsReader[Packet] {
    override type JsReaderFailure = String

    override def read(value: JsValue): Validation[PacketReader.JsReaderFailure, Packet] = value match {
      case JsObject(o) => {
        val p = for(
          route <- o.get("type").flatMap(_.as[String].toOption);
          payload <- getPayload(route, o.get("data")).toOption
        ) yield {
          new Packet(route, payload)
        }

        p match {
          case Some(o) => Success(o)
          case _       => Failure(new JsReaderFailure("Incorrect packet format."))
        }
      }
      case _ => Failure("Packet not a json object.")
    }
  }

  private def getPayload(route: String, payload: JsValue): Validation[String, Payload] = route match {
    case "hello" => payload.as[Hello]
    case _       => Failure("Invalid route specified.")
  }

  def parse(input: String): Validation[String, Packet] = {
    JsonParser.parse(input).mapError(_.message).flatMap(_.as[Packet])
  }

}

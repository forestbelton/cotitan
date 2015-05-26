package com.cotitan.server.routing

import com.cotitan.server.routing.Payload.Hello
import com.plasmaconduit.json.{JsonParser, JsObject, JsReader, JsValue}
import com.plasmaconduit.validation.{Success, Failure, Validation}

final case class Packet(route: String, payload: Payload) // should contain identification information

object Packet {

  private final case class UnparsedPacket(route: String, payload: JsValue)

  private implicit object UnparsedPacket extends JsReader[UnparsedPacket] {
    override type JsReaderFailure = String

    override def read(value: JsValue): Validation[UnparsedPacket.JsReaderFailure, UnparsedPacket] = value match {
      case JsObject(o) => {
        val p = for(
          route <- o.get("type").flatMap(_.as[String].toOption);
          payload <- o.get("data")
        ) yield UnparsedPacket(route, payload)

        p match {
          case Some(o) => Success(o)
          case _       => Failure(new JsReaderFailure("Incorrect packet format."))
        }
      }
      case _ => Failure("Packet not a json object.")
    }
  }

  def parse(input: String): Validation[String, Packet] = {
    val unparsedValidation = JsonParser.parse(input).mapError(_.message).flatMap(_.as[UnparsedPacket])

    val parsedValidation = for (
      unparsed <- unparsedValidation
    ) yield {
      val payload = unparsed.route match {
        case "hello" => unparsed.payload.as[Hello]
      }

      payload.map(p => Packet(unparsed.route, p))
    }

    parsedValidation.flatMap(x => x)
  }

}

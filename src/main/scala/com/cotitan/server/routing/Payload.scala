package com.cotitan.server.routing

import com.plasmaconduit.json.{JsObject, JsValue, JsReader}
import com.plasmaconduit.validation.{Success, Failure, Validation}

sealed trait Payload

object Payload {

  final case class Hello(name: String) extends Payload

  implicit object Hello extends JsReader[Hello] {
    override type JsReaderFailure = String

    override def read(value: JsValue): Validation[Hello.JsReaderFailure, Hello] = value match {
      case JsObject(o) => {
        val h = for(
          name <- o.get("name").flatMap(_.as[String].toOption)
        ) yield Hello(name)

        h match {
          case Some(o) => Success(o)
          case _       => Failure(new JsReaderFailure("Incorrect payload format."))
        }
      }
      case _ => Failure("Payload not a json object.")
    }
  }

  // add new route types here

}



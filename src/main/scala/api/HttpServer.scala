package org.interscity.htc
package api

import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.server.Route

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

import core.HttpConfig

/** Binds the Pekko HTTP server to the configured host/port. */
object HttpServer:

  def bind(route: Route, cfg: HttpConfig)(using system: ActorSystem[?]): Unit =
    given ExecutionContext = system.executionContext

    Http()(system)
      .newServerAt(cfg.host, cfg.port)
      .bind(route)
      .onComplete {
        case Success(binding) =>
          system.log.info(
            "SimEDaPE HTTP API listening on http://{}:{}/",
            binding.localAddress.getHostString,
            binding.localAddress.getPort,
          )
        case Failure(ex) =>
          system.log.error("Failed to bind HTTP server on {}:{} — {}", cfg.host, cfg.port, ex.getMessage)
          system.terminate()
      }

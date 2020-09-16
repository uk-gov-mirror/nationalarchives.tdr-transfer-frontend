package configuration

import sttp.client.{NothingT, SttpBackend}
import sttp.client.asynchttpclient.future.AsyncHttpClientFutureBackend

import scala.concurrent.Future

object GraphqlBackend {
  implicit val backend: SttpBackend[Future, Nothing, NothingT] = AsyncHttpClientFutureBackend()
}

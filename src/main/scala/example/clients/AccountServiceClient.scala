package example.clients

import cats.effect.Sync
import cats.implicits._
import cats.syntax.all.none
import example.clients.AccountServiceClient.AccountServiceClientError
import example.model.Account
import io.estatico.newtype.macros.newtype
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

import java.util.UUID

trait AccountServiceClient[F[_]] {
  def fetchAccount(accountId: UUID): F[Either[AccountServiceClientError, FullAccount]]

  def fetchUserPersonalInfo(accountId: UUID): F[Either[AccountServiceClientError, PersonalInfo]]

  def updateAccount(fullAccount: FullAccount): F[Either[AccountServiceClientError, Unit]]
}

object AccountServiceClient {


  def accountRoute[F[_]: Sync](A: Account[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "account" / accountId =>
        for {
          greeting <- A.hello(Account.AccountId(accountId))
          resp <- Ok(greeting)
        } yield resp
    }
  }

  override def fetchUserPersonalInfo(accountId: UUID): F[Either[AccountServiceClientError, FullAccount]] = {

  }

  sealed abstract class AccountServiceClientError(override val message: String) extends CsException(message, none)

  object AccountServiceClientError {
    final case class HttpError(statusCode: HttpError.StatusCode, error: String) extends AccountServiceClientError(show"Http error")

    final case class UnknownAccountError(accountId: UUID) extends AccountServiceClientError(show"Account $accountId is unknown")

    final case class UnexpectedResponseError(statusCode: Int, statusText: Option[String])
      extends AccountServiceClientError(show"Account service call failed with status $statusCode")

    def httpError(statusCode: HttpError.StatusCode, error: String): AccountServiceClientError           = HttpError(statusCode, error)
    def unknownAccountError(accountId: UUID): AccountServiceClientError                                 = UnknownAccountError(accountId)
    def unexpectedResponseError(statusCode: Int, statusText: Option[String]): AccountServiceClientError = UnexpectedResponseError(statusCode, statusText)

    object HttpError {
      @newtype case class StatusCode(statusCode: Int)
    }

}
abstract class CsException(val message: String, val cause: Option[Throwable]) extends Exception(message, cause.orNull)
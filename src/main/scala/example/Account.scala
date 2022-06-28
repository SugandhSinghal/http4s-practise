package example

import cats.{Applicative, Show}
import cats.data.NonEmptyList
import example.Account.Email
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder, deriveEncoder}
import io.circe.{Codec, Decoder, Encoder, Json}
import org.http4s.EntityEncoder
import org.http4s.circe.jsonEncoderOf

import java.time.LocalDate
import java.util.UUID

trait Account[F[_]]{
  def get: F[Jokes.Joke]
}
object Account {
  implicit def apply[F[_]](implicit ev: Account[F]): Account[F] = ev

  final case class AccountId(accountId: String) extends AnyVal
  /**
    * More generally you will want to decouple your edge representations from
    * your internal data structures, however this shows how you can
    * create encoders for your data.
    **/
  final case class Greeting(greeting: String) extends AnyVal
  object Greeting {
    implicit val greetingEncoder: Encoder[Greeting] = new Encoder[Greeting] {
      final def apply(a: Greeting): Json = Json.obj(
        ("message", Json.fromString(a.greeting)),
      )
    }
    implicit def greetingEntityEncoder[F[_]]: EntityEncoder[F, Greeting] =
      jsonEncoderOf[F, Greeting]
  }



  final case class PersonalInfo(uuid: UUID, email: Email, name: NameTFML, dateOfBirth: LocalDate, mobile: Option[PhoneNumber])
  object PersonalInfo {

    def fromPersonInfoFullAccount(fullAccount: FullAccount): PersonalInfo =
      PersonalInfo(
        fullAccount.uuid,
        fullAccount.email,
        fullAccount.account.user.name,
        fullAccount.account.user.dateOfBirth,
        fullAccount.account.phones.flatMap(_.find(_.`type` == PhoneNumberType.MOBILE)),
      )

  final case class UserPersonalInfo(name: NameTFML, dateOfBirth: LocalDate)
  private[internal] object UserPersonalInfo {
    implicit final val userPersonalInfoEncoder: Encoder[UserPersonalInfo] = deriveEncoder
    implicit final val userPersonalInfoDecoder: Decoder[UserPersonalInfo] = deriveDecoder
  }


  final case class Account(user: UserPersonalInfo, emails: NonEmptyList[CsEmail], phones: Option[NonEmptyList[PhoneNumber]])
  private[internal] object Account {
    implicit final val accountEncoder: Codec[Account] = deriveCodec
  }

  final case class FullAccount(uuid: UUID, account: Account)
  private[internal] object FullAccount {
    implicit final val accountEncoder: Codec[FullAccount] = deriveCodec

    implicit final class FullAccountOps(private val fullAccount: FullAccount) extends AnyVal {
      def email: Email = fullAccount.account.emails.head.address
    }
  }

  def impl[F[_]: Applicative]: HelloWorld[F] = new HelloWorld[F]{
    def hello(n: HelloWorld.Name): F[HelloWorld.Greeting] =
      Greeting("Hello, " + n.name).pure[F]
  }
}


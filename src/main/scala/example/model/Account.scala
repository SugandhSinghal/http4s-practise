package example.model

import cats.Applicative
import cats.data.NonEmptyList
import cats.implicits._
import example.model.Types.Amount
import example.model.Types.DateOfBirth.EmploymentStatus
import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Encoder, Json}
import org.http4s.EntityEncoder
import org.http4s.circe._

import java.time.LocalDate
import java.util.UUID


trait Account[F[_]]{
  def hello(n: Account.AccountId): F[Account.Greeting]
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

  def impl[F[_]: Applicative]: Account[F] = new Account[F]{
    def hello(n: Account.AccountId): F[Account.Greeting] =
      Greeting("Hello, " + n.accountId).pure[F]
  }
}

final case class AnnualSalary(amount: Amount)
object AnnualSalary {
  implicit val salaryCodec: Codec[AnnualSalary] = deriveCodec
}

final case class UserPersonalInfo(dateOfBirth: LocalDate, salary: AnnualSalary, employmentStatus: EmploymentStatus)
object UserPersonalInfo {
  implicit val userPersonalInfoCodec: Codec[UserPersonalInfo] = deriveCodec
}

final case class Account1(user: UserPersonalInfo)
object Account1 {
  implicit val accountCodec: Codec[Account1] = deriveCodec
}

final case class FullAccount(uuid: UUID, account: Account1) {
}
object FullAccount {
  implicit final val codec: Codec[FullAccount] = deriveCodec

}
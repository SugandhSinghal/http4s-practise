package example.model

import cats.Eq
import cats.data.NonEmptyList
import example.model.Types.DateOfBirth.{AusState, EmploymentStatus}
import example.model.Types.{Amount, DateOfBirth}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

import java.time.LocalDate
import java.util.UUID

final case class AccountDetails(uuid: UUID, dateOfBirth: DateOfBirth, amount: Amount, employmentStatus: EmploymentStatus)
final case class AccountDetailsForEligibility(uuid: UUID, dateOfBirth: DateOfBirth, amount: Amount, employmentStatus: EmploymentStatus)
object AccountDetailsForEligibility {

  def fromFullAccountIntoAccountDetailsForEligibility(accountDetails: FullAccount): AccountDetailsForEligibility =
    AccountDetailsForEligibility(
      accountDetails.uuid,
      accountDetails.dateOfBirth,
      accountDetails.amount,
      accountDetails.employmentStatus,
    )

  final case class Address(
                            addressLine1: String,
                            addressLine2: Option[String],
                            city: String,
                            state: AusState,
                            postalCode: PostalCode,
                            format: Option[String] = Some("AU"),
                          )
  object Address {
    implicit final val codec: Codec[Address] = deriveCodec
    implicit final val eq: Eq[Address]       = Eq.fromUniversalEquals
  }

  final case class AddressHistory(structured: Address, country: NonEmptyString = NonEmptyString("AU"))
  object AddressHistory {
    implicit final val codec: Codec[AddressHistory] = deriveCodec
  }

  final case class UserPersonalInfo(dateOfBirth: LocalDate, salary: AnnualSalary, employmentStatus: EmploymentStatus, addressHistory: NonEmptyList[AddressHistory])
  object UserPersonalInfo {
    implicit val userPersonalInfoCodec: Codec[UserPersonalInfo] = deriveCodec
  }

  implicit val personalInfoCodec: Codec[AccountDetailsForEligibility] = deriveCodec

  final case class UserPersonalInfo(dateOfBirth: LocalDate, salary: AnnualSalary, employmentStatus: EmploymentStatus)
  object UserPersonalInfo {
    implicit val userPersonalInfoCodec: Codec[UserPersonalInfo] = deriveCodec
  }

  final case class Account(user: UserPersonalInfo)
  object Account {
    implicit val accountCodec: Codec[Account] = deriveCodec
  }

  final case class AnnualSalary(amount: Amount)
  object AnnualSalary {
    implicit val salaryCodec: Codec[AnnualSalary] = deriveCodec
  }

  final case class FullAccount(uuid: UUID, account: Account)
  object FullAccount {
    implicit val accountCodec: Codec[FullAccount] = deriveCodec

    implicit final class FullAccountOps(private val accountDetails: FullAccount) extends AnyVal {
      def dateOfBirth: DateOfBirth           = DateOfBirth(accountDetails.account.user.dateOfBirth)
      def amount: Amount                     = accountDetails.account.user.salary.amount
      def employmentStatus: EmploymentStatus = accountDetails.account.user.employmentStatus
    }
  }
}



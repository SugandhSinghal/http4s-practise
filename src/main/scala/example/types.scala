package example

import cats.Show
import cats.syntax.all._
import io.circe._
import io.estatico.newtype.macros.newtype
import com.clearscore.aep.model.types.AccountData._
import io.circe.{Decoder, DecodingFailure, Encoder, Json, KeyDecoder, KeyEncoder}

import java.time.{LocalDate, Period}
import java.util.UUID

object types {

  @newtype case class AccountUuid(value: UUID)

  final case class AccountData(
    age: Option[Age],
    creditScore: Option[CreditScore],
    dateOfBirth: Option[DateOfBirth],
    employmentStatus: Option[EmploymentStatus],
    amount: Option[Amount],
  )

  object AccountData {

    @newtype case class Age(value: Int)

    object Age {
      implicit val ageOrdering: Ordering[Age] = Ordering.by(_.value)

      implicit final class AgeOps(private val age: Age) extends AnyVal with Ordered[Age] {
        override def compare(that: Age): Int = ageOrdering.compare(age, that)
      }

      implicit val ageEncoder: Encoder[Age] = deriving
      implicit val ageDecoder: Decoder[Age] = deriving

    }

    @newtype case class CreditScore(value: Int)
    object CreditScore {
      implicit val creditScoreEncoder: Encoder[CreditScore] = deriving
      implicit val creditScoreDecoder: Decoder[CreditScore] = deriving
    }

    @newtype case class Amount(value: Int)
    object Amount {
      implicit val annualSalaryEncoder: Encoder[Amount] = deriving
      implicit val annualSalaryDecoder: Decoder[Amount] = deriving
    }

    @newtype case class DateOfBirth(value: LocalDate)
    object DateOfBirth {
      implicit final class DateOfBirthOps(private val dateOfBirth: DateOfBirth) extends AnyVal {
        def ageWhen(date: LocalDate): Option[Age] = {
          val dob = dateOfBirth.value
          if (date.isBefore(dob))
            none
          else {
            Age(Period.between(dob, date).getYears).some
          }
        }
      }

      implicit val dateOfBirthEncoder: Encoder[DateOfBirth] = deriving
      implicit val dateOfBirthDecoder: Decoder[DateOfBirth] = deriving
    }

    sealed abstract class EmploymentStatus(val value: String) extends StringEnumEntry with Product with Serializable
    object EmploymentStatus extends StringEnum[EmploymentStatus] with StringCirceEnum[EmploymentStatus] {
      case object FullTime extends EmploymentStatus("FT_EMPLOYED")
      case object PartTime extends EmploymentStatus("PT_EMPLOYED")
      case object Student extends EmploymentStatus("STUDENT")
      case object Unemployed extends EmploymentStatus("UNEMPLOYED")
      case object Retired extends EmploymentStatus("RETIRED")
      case object SelfEmployed extends EmploymentStatus("SELF_EMPLOYED")
      case object WorkAtHome extends EmploymentStatus("WORK_AT_HOME")

      override def values: IndexedSeq[EmploymentStatus] = findValues
    }
  }

  sealed abstract class PartnerName(val value: String) extends StringEnumEntry
  case object PartnerName extends StringEnum[PartnerName] with CatsValueEnum[String, PartnerName] {

    object OurMoneyMarketLoans extends PartnerName("our-money-market-loans")
    object OurMoneyMarketLoansExcellentCredit extends PartnerName("our-money-market-loans-excellent-credit")
    object Money3Loans extends PartnerName("money3-loans")
    object NowFinanceLoansExcellentCredit extends PartnerName("now-finance-loans-excellent-credit")
    object NowFinanceLoans extends PartnerName("now-finance-loans")
    object NowFinanceLoansApiExcellentCredit extends PartnerName("nowfinance-personal-loans-api-excellent-credit")
    object NowFinanceLoansApi extends PartnerName("nowfinance-personal-loans-api-fixed-rate")
    object FairGoFinanceLarge extends PartnerName("FairGoFinanceLarge")
    object FairGoFinanceMedium extends PartnerName("FairGoFinanceMedium")
    object FairGoFinanceSmall extends PartnerName("FairGoFinanceSmall")
    object DrivaPreApprovedCarLoans extends PartnerName("driva-preapproved-carloans")

    implicit val partnerNameKeyEncoder: KeyEncoder[PartnerName] = partnerName => KeyEncoder.encodeKeyString(partnerName.value)
    implicit val partnerNameEncoder: Encoder[PartnerName]       = partNername => Json.fromString(partNername.value)

    implicit val partnerNameKeyDecoder: KeyDecoder[PartnerName] = key => PartnerName.withValueOpt(key)
    implicit val partnerNameDecoder: Decoder[PartnerName]       = c =>
      c.as[String]
        .flatMap(s =>
          withValueOpt(s)
            .toRight(DecodingFailure(s"Unknown partner name: $s (Known names: ${values.mkString("[", ", ", "]")})", c.history))
        )

    override def values: IndexedSeq[PartnerName] = findValues

    implicit val ordering: Ordering[PartnerName] = Ordering.by(_.value)

    //    implicit def partnerNameShow[A <: PartnerName]: Show[A] = _.value

  }

  sealed abstract class Vertical(val value: String) extends StringEnumEntry
  object Vertical extends StringEnum[Vertical] {
    case object Loans extends Vertical("loans")
    case object CarLoans extends Vertical("car-loans")

    def loans: Vertical    = Loans
    def carLoans: Vertical = CarLoans

    override def values: IndexedSeq[Vertical] = findValues

    implicit def verticalShow[A <: Vertical]: Show[A] = _.value
  }

  object VerticalVar {
    def unapply(path: String): Option[Vertical] =
      Vertical.withValueOpt(path)
  }

}

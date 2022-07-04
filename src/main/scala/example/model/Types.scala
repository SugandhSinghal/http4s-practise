package example.model

import cats.Show
import cats.implicits.catsSyntaxOptionId
import cats.syntax.all.none
import enumeratum.values.{StringCirceEnum, StringDoobieEnum, StringEnum, StringEnumEntry}
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.semiauto.deriveCodec
import io.estatico.newtype.macros.newtype

import scala.collection.immutable
//import enumeratum.values.StringCirceEnum

import java.time.{LocalDate, Period}

object Types {
  final case class CsMessage(
                              `type`: String,
    version: Int,
    clientRef: String,
    createdAt: Long,
    source: String,
    tags: Map[String, String],
  )

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
      type PostCodeRule = MatchesRegex["""[0-9][0-9][0-9][0-9]"""]
      type PostalCode   = Refined[String, PostCodeRule]
      object PostalCode {
        def asValidationRules(fieldId: String, required: Boolean): List[Rule] =
          List(
            RegexMatch(questionId = fieldId, regex = "[0-9][0-9][0-9][0-9]", error = "Please enter a valid postal code").some,
            if (required) Required(fieldId, error = "Please input a valid postal code").some else None,
          ).flatten
      }

      sealed abstract class AusState(override val value: String) extends StringEnumEntry
      object AusState
        extends StringEnum[AusState]
          with StringCirceEnum[AusState]
          with StringDoobieEnum[AusState]
          with ClearTextStringEnum[AusState] {
        override final val values: immutable.IndexedSeq[AusState] = findValues

        case object NSW extends AusState("NSW")
        case object VIC extends AusState("VIC")
        case object ACT extends AusState("ACT")
        case object NT extends AusState("NT")
        case object QLD extends AusState("QLD")
        case object SA extends AusState("SA")
        case object TAS extends AusState("TAS")
        case object WA extends AusState("WA")

        def nsw: AusState = NSW
        def vic: AusState = VIC
        def act: AusState = ACT
        def nt: AusState  = NT
        def qld: AusState = QLD
        def sa: AusState  = SA
        def tas: AusState = TAS
        def wa: AusState  = WA
        implicit final val ausStateShow: Show[AusState]               = {
          case NSW => "New South Wales"
          case VIC => "Victoria"
          case ACT => "Australian Capital Territory"
          case NT => "Northern Territory"
          case QLD => "Queensland"
          case SA => "South Australia"
          case TAS => "Tasmania"
          case WA => "Western Australia"
        }
        final def asTextSelectValue(state: AusState): TextSelectValue = TextSelectValue(id = state.value, label = state.show)

        final val asTextSelectValues: List[TextSelectValue] = this.values.map(asTextSelectValue).toList
      }


        /*    sealed abstract class EmploymentStatus(val value: String) extends StringEnumEntry with Product with Serializable
            object EmploymentStatus extends StringEnum[EmploymentStatus] with StringCirceEnum[EmploymentStatus] {
              case object FullTime extends EmploymentStatus("FT_EMPLOYED")
              case object PartTime extends EmploymentStatus("PT_EMPLOYED")
              case object Student extends EmploymentStatus("STUDENT")
              case object Unemployed extends EmploymentStatus("UNEMPLOYED")
              case object Retired extends EmploymentStatus("RETIRED")
              case object SelfEmployed extends EmploymentStatus("SELF_EMPLOYED")
              case object WorkAtHome extends EmploymentStatus("WORK_AT_HOME")

              override def values: IndexedSeq[EmploymentStatus] = findValues
            }*/
  }

  object CsMessage {
    implicit val csMessageDecoder: Codec[CsMessage] = deriveCodec
  }
}

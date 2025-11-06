/*
 * Copyright 2025 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.disareturnsstubs.controllers

import jakarta.inject.Singleton
import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import uk.gov.hmrc.disareturnsstubs.controllers.action.AuthorizationFilter
import uk.gov.hmrc.disareturnsstubs.models.ErrorResponse._
import uk.gov.hmrc.disareturnsstubs.models.{ErrorResponse, MonthlyReport, ReturnResultResponse}
import uk.gov.hmrc.disareturnsstubs.repositories.ReportRepository
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class NpsController @Inject() (
  cc: ControllerComponents,
  authorizationFilter: AuthorizationFilter,
  reportRepository: ReportRepository
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging {

  def submitMonthlyReturn(isaReferenceNumber: String): Action[AnyContent] =
    (Action andThen authorizationFilter).async { _ =>
      isaReferenceNumber match {
        case "Z1400" => Future.successful(BadRequest(Json.toJson(badRequestError)))
        case "Z1503" => Future.successful(ServiceUnavailable(Json.toJson(serviceUnavailableError)))
        case _       =>
          logger.info(s"Successfully submitted data for IM ref: [$isaReferenceNumber]")
          Future.successful(NoContent)
      }
    }

  def send(isaReferenceNumber: String): Action[AnyContent] = Action {
    isaReferenceNumber match {
      case "Z1500" => InternalServerError(Json.toJson(internalServerErr("Internal issue, try again later")))
      case _       =>
        logger.info(s"Successfully submitted declaration for IM Ref: [$isaReferenceNumber]")
        NoContent
    }
  }

  def getMonthlyReport(
    isaReferenceNumber: String,
    taxYear: String,
    month: String,
    skip: Int,
    take: Int
  ): Action[AnyContent] = Action.async { _ =>
    if (isaReferenceNumber == "Z1500") {
      Future.successful(
        InternalServerError(Json.toJson(internalServerErr("Internal issue, try again later")))
      )
    } else {
      reportRepository
        .getMonthlyReport(isaReferenceNumber, taxYear, month)
        .map {
          case Some(report) =>
            getReportPage(report, skip, take) match {
              case Left(error) =>
                logger.warn(s"Page not found in report for IM ref: [$isaReferenceNumber] for [$month][$taxYear]")
                NotFound(Json.toJson(error))
              case Right(page) =>
                logger.info(
                  s"Successful retrieval of monthly report for IM ref: [$isaReferenceNumber] for [$month][$taxYear]"
                )
                Ok(
                  Json.toJson(
                    ReturnResultResponse(totalRecords = report.returnResults.size, returnResults = page.returnResults)
                  )
                )
            }

          case None =>
            logger.warn(s"No monthly report found for IM ref: [$isaReferenceNumber] for [$month][$taxYear]")
            NotFound(Json.toJson(reportNotFoundError))
        }
        .recover { case ex =>
          logger.error(
            s"Unexpected error retreiving monthly report for IM ref: [$isaReferenceNumber] for [$month][$taxYear] with: [${ex.getMessage}]"
          )
          InternalServerError(Json.toJson(internalServerErr(s"Failed with exception: ${ex.getMessage}")))
        }
    }
  }

  private def getReportPage(fullReport: MonthlyReport, skip: Int, take: Int): Either[ErrorResponse, MonthlyReport] = {
    val startOfPage  = skip * take
    val totalRecords = fullReport.returnResults.size
    val endOfPage    = (startOfPage + take).min(totalRecords)

    if (startOfPage >= totalRecords) Left(pageNotFoundError(skip))
    else Right(fullReport.copy(returnResults = fullReport.returnResults.slice(startOfPage, endOfPage)))
  }
}

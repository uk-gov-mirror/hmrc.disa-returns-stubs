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

import play.api.libs.json.{JsValue, Json}
import play.api.test.Helpers._
import play.api.test._
import uk.gov.hmrc.disareturnsstubs.BaseISpec
import uk.gov.hmrc.disareturnsstubs.models.{IssueIdentifiedMessage, MonthlyReport, ReturnResult, ReturnResultResponse}

class NpsControllerISpec extends BaseISpec {

  val submitMonthlyReturnEndpoint     = "/nps/submit"
  val npsDeclarationEndpoint          = "/nps/declaration"
  val getReturnResultsSummaryEndpoint = "/nps/summary-results"
  val month                           = "APR"
  val taxYear                         = "2025-26"

  val report: MonthlyReport = MonthlyReport(
    isaManagerReferenceNumber = isaManagerReferenceNumber,
    year = taxYear,
    month = month,
    returnResults = Seq(
      ReturnResult(
        accountNumber = "100000001",
        nino = "AB123457C",
        issueIdentified = IssueIdentifiedMessage(
          code = "UNABLE_TO_IDENTIFY_INVESTOR",
          message = "Unable to identify investor"
        )
      )
    )
  )

  val validPayload: JsValue = Json.parse("""[
  |  {
  |    "accountNumber": "12345678",
  |    "nino": "AB123456C",
  |    "firstName": "John",
  |    "middleName": "Michael",
  |    "lastName": "Smith",
  |    "dateOfBirth": "1980-01-01",
  |    "isaType": "LIFETIME_CASH",
  |    "reportingATransfer": true,
  |    "dateOfLastSubscription": "2025-05-01",
  |    "totalCurrentYearSubscriptionsToDate": 5000,
  |    "marketValueOfAccount": 10000,
  |    "dateOfFirstSubscription": "2025-04-06",
  |    "lisaQualifyingAddition": 1000,
  |    "lisaBonusClaim": 1000
  |  },
  |  {
  |    "accountNumber": "12345678",
  |    "nino": "AB123456C",
  |    "firstName": "John",
  |    "middleName": "Michael",
  |    "lastName": "Smith",
  |    "dateOfBirth": "1980-01-01",
  |    "isaType": "LIFETIME_CASH",
  |    "reportingATransfer": true,
  |    "dateOfLastSubscription": "2025-05-01",
  |    "totalCurrentYearSubscriptionsToDate": 5000,
  |    "marketValueOfAccount": 10000,
  |    "accountNumberOfTransferringAccount": "87654321",
  |    "amountTransferred": 5000,
  |    "dateOfFirstSubscription": "2025-04-06",
  |    "lisaQualifyingAddition": 1000,
  |    "lisaBonusClaim": 1000
  |  }
  |]""".stripMargin)

  "POST /nps/submit/:isaReferenceNumber" should {

    "return 204 NoContent for any non-error ISA ref" in {
      val request = FakeRequest(POST, s"$submitMonthlyReturnEndpoint/$isaManagerReferenceNumber")
        .withHeaders("Authorization" -> "Bearer token")
        .withJsonBody(validPayload)

      val result = route(app, request).get
      status(result) mustBe NO_CONTENT
    }

    "return 400 BadRequest for isaRef Z1400" in {
      val request = FakeRequest(POST, s"$submitMonthlyReturnEndpoint/Z1400")
        .withHeaders("Authorization" -> "Bearer token")
        .withJsonBody(validPayload)

      val result = route(app, request).get
      status(result) mustBe BAD_REQUEST
      (contentAsJson(result) \ "code").asOpt[String] mustBe Some("BAD_REQUEST")
    }

    "return 503 ServiceUnavailable for isaRef Z1503" in {
      val request = FakeRequest(POST, s"$submitMonthlyReturnEndpoint/Z1503")
        .withHeaders("Authorization" -> "Bearer token")
        .withJsonBody(validPayload)

      val result = route(app, request).get
      status(result) mustBe SERVICE_UNAVAILABLE
      (contentAsJson(result) \ "code").asOpt[String] mustBe Some("SERVICE_UNAVAILABLE")
    }

    "return 403 Forbidden when Authorization header is missing" in {
      val request = FakeRequest(POST, s"$submitMonthlyReturnEndpoint/$isaManagerReferenceNumber")
        .withJsonBody(validPayload)

      val result = route(app, request).get
      status(result) mustBe FORBIDDEN
      (contentAsJson(result) \ "message").asOpt[String] mustBe Some("Missing required bearer token")
    }
  }

  "POST /nps/declaration/:isaReferenceNumber" should {

    "return 204 NoContent for any non-error ISA ref" in {
      val request = FakeRequest(POST, s"$npsDeclarationEndpoint/$isaManagerReferenceNumber")
      val result = route(app, request).get
      status(result) mustBe NO_CONTENT
    }

    "return 500 InternalServerError for ISA ref Z1500" in {
      val request = FakeRequest(POST, s"$npsDeclarationEndpoint/Z1500")

      val result = route(app, request).get
      status(result) mustBe INTERNAL_SERVER_ERROR
      (contentAsJson(result) \ "code").asOpt[String] mustBe Some("INTERNAL_SERVER_ERROR")
      (contentAsJson(result) \ "message").asOpt[String] mustBe Some("Internal issue, try again later")
    }
  }

  "GET /reports/:isaReferenceNumber/:taxYear/:month" should {

    val skip = 0

    "return 200 OK and the returnResults when a report exists" in {
      await(reportRepository.insertReport(report))

      val request = FakeRequest(GET, s"/monthly/$isaManagerReferenceNumber/$taxYear/$month/results?skip=$skip&take=10")
      val result  = route(app, request).get

      status(result) mustBe OK

      val jsonBody = contentAsJson(result)
      val response = jsonBody.as[ReturnResultResponse]

      response.totalRecords mustBe 1
      response.returnResults must have size 1

      val first = response.returnResults.head
      first.accountNumber mustBe "100000001"
      first.nino mustBe "AB123457C"
      first.issueIdentified.code mustBe "UNABLE_TO_IDENTIFY_INVESTOR"
    }

    "return 404 PageNotFound when page does not exist in report" in {
      await(reportRepository.collection.drop.toFuture())
      await(reportRepository.insertReport(report))

      val skip = 1
      val request = FakeRequest(GET, s"/monthly/$isaManagerReferenceNumber/$taxYear/$month/results?skip=$skip&take=10")
      val result  = route(app, request).get

      status(result) mustBe NOT_FOUND
      (contentAsJson(result) \ "code").asOpt[String] mustBe Some("PAGE_NOT_FOUND")
      (contentAsJson(result) \ "message").asOpt[String] mustBe Some(s"No page $skip found")
    }

    "return 404 NotFound when no report exists for given identifiers" in {
      await(reportRepository.collection.drop.toFuture())
      val request = FakeRequest(GET, s"/monthly/$isaManagerReferenceNumber/$taxYear/$month/results?skip=$skip&take=10")
      val result  = route(app, request).get

      status(result) mustBe NOT_FOUND
      (contentAsJson(result) \ "code").asOpt[String] mustBe Some("REPORT_NOT_FOUND")
      (contentAsJson(result) \ "message").asOpt[String] mustBe Some("Report not found")
    }

    "return 500 InternalServerError when isaReferenceNumber is Z1500" in {
      val request = FakeRequest(GET, s"/monthly/Z1500/$taxYear/$month/results?skip=$skip&take=10")
      val result  = route(app, request).get

      status(result) mustBe INTERNAL_SERVER_ERROR
      (contentAsJson(result) \ "code").asOpt[String] mustBe Some("INTERNAL_SERVER_ERROR")
      (contentAsJson(result) \ "message").asOpt[String] mustBe Some("Internal issue, try again later")
    }
  }
}

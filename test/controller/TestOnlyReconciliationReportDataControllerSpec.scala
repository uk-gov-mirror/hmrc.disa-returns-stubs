/*
 * Copyright 2026 HM Revenue & Customs
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

package controller

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{inOrder as mockitoInOrder, never, verify, when}
import play.api.http.HeaderNames.CONTENT_TYPE
import play.api.http.MimeTypes.JSON
import play.api.http.Status.{BAD_REQUEST, NO_CONTENT}
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.{POST, status, stubControllerComponents}
import uk.gov.hmrc.disareturnsstubs.models.generatereport.ReportEvent
import uk.gov.hmrc.disareturnsstubs.repositories.generatereport.{ReportEventRepository, ReportIssueRepository}
import uk.gov.hmrc.disareturnsstubs.testOnly.controllers.TestOnlyReconciliationReportDataController
import utils.BaseUnitSpec

import scala.concurrent.Future

class TestOnlyReconciliationReportDataControllerSpec extends BaseUnitSpec {

  private def request(zReferences: Seq[String]) =
    FakeRequest(POST, "/test-only/reconciliation-report-data/cleanup")
      .withHeaders(CONTENT_TYPE -> JSON)
      .withBody(Json.obj("zReferences" -> zReferences))

  "delete" should {
    "delete report issues and events for normalized Z-references" in {
      val reportEventRepository = mock[ReportEventRepository]
      val reportIssueRepository = mock[ReportIssueRepository]
      val controller            = new TestOnlyReconciliationReportDataController(
        stubControllerComponents(),
        reportEventRepository,
        reportIssueRepository
      )
      val zReferences           = Seq("Z1234", "Z5678")
      val reportEvents          = Seq(ReportEvent("report-1", "Z1234"), ReportEvent("report-2", "Z5678"))

      when(reportEventRepository.findByZReferences(zReferences)).thenReturn(Future.successful(reportEvents))
      when(reportIssueRepository.deleteByZReferences(zReferences)).thenReturn(Future.unit)
      when(reportIssueRepository.deleteByReportIds(Seq("report-1", "report-2"))).thenReturn(Future.unit)
      when(reportEventRepository.deleteByZReferences(zReferences)).thenReturn(Future.unit)

      val result = controller.delete()(request(Seq("z1234", "Z5678", "Z1234")))

      status(result) shouldBe NO_CONTENT
      val deletionOrder = mockitoInOrder(reportIssueRepository, reportEventRepository)
      deletionOrder.verify(reportIssueRepository).deleteByZReferences(zReferences)
      deletionOrder.verify(reportIssueRepository).deleteByReportIds(Seq("report-1", "report-2"))
      deletionOrder.verify(reportEventRepository).deleteByZReferences(zReferences)
    }

    "delete tagged issues when there is no surviving report event" in {
      val reportEventRepository = mock[ReportEventRepository]
      val reportIssueRepository = mock[ReportIssueRepository]
      val controller            = new TestOnlyReconciliationReportDataController(
        stubControllerComponents(),
        reportEventRepository,
        reportIssueRepository
      )
      val zReferences           = Seq("Z1234")

      when(reportEventRepository.findByZReferences(zReferences)).thenReturn(Future.successful(Seq.empty))
      when(reportIssueRepository.deleteByZReferences(zReferences)).thenReturn(Future.unit)
      when(reportIssueRepository.deleteByReportIds(Seq.empty)).thenReturn(Future.unit)
      when(reportEventRepository.deleteByZReferences(zReferences)).thenReturn(Future.unit)

      val result = controller.delete()(request(Seq(" z1234 ")))

      status(result) shouldBe NO_CONTENT
      verify(reportIssueRepository).deleteByZReferences(zReferences)
      verify(reportIssueRepository).deleteByReportIds(Seq.empty)
      verify(reportEventRepository).deleteByZReferences(zReferences)
    }

    "return BadRequest when a Z-reference is invalid" in {
      val reportEventRepository = mock[ReportEventRepository]
      val reportIssueRepository = mock[ReportIssueRepository]
      val controller            = new TestOnlyReconciliationReportDataController(
        stubControllerComponents(),
        reportEventRepository,
        reportIssueRepository
      )

      val result = controller.delete()(request(Seq("invalid")))

      status(result) shouldBe BAD_REQUEST
      verify(reportEventRepository, never()).findByZReferences(any())
      verify(reportIssueRepository, never()).deleteByZReferences(any())
      verify(reportIssueRepository, never()).deleteByReportIds(any())
    }
  }
}

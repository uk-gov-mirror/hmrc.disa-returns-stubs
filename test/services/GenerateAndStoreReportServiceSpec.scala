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

package services

import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{verify, when}
import org.mongodb.scala.result.UpdateResult
import play.api.test.Helpers.await
import uk.gov.hmrc.disareturnsstubs.config.AppConfig
import uk.gov.hmrc.disareturnsstubs.models.{IssueIdentifiedMessage, ReturnResult}
import uk.gov.hmrc.disareturnsstubs.models.generatereport.{GenerateReportRequest, ReportEvent, ReportIssueDocument}
import uk.gov.hmrc.disareturnsstubs.repositories.generatereport.{ReportEventRepository, ReportIssueRepository}
import uk.gov.hmrc.disareturnsstubs.services.{GenerateAndStoreReportService, GenerateReportIssuesService}
import utils.BaseUnitSpec

import scala.concurrent.Future

class GenerateAndStoreReportServiceSpec extends BaseUnitSpec {

  "generateAndStore" should {
    "tag every generated issue with its owning Z-reference" in {
      val reportEventRepository = mock[ReportEventRepository]
      val reportIssueRepository = mock[ReportIssueRepository]
      val reportGenerator       = mock[GenerateReportIssuesService]
      val appConfig             = mock[AppConfig]
      val result                = ReturnResult(
        accountNumber = "100000001",
        nino = "AB123456C",
        issueIdentified = IssueIdentifiedMessage("FAILED_ELIGIBILITY", "Failed eligibility")
      )

      when(appConfig.reportIssueLimit).thenReturn(10)
      when(reportGenerator.generateResults(any())).thenReturn(Seq(result))
      when(reportEventRepository.upsert(any[ReportEvent])).thenReturn(Future.successful(mock[UpdateResult]))
      when(reportIssueRepository.insertMany(any())).thenReturn(Future.unit)

      val service = new GenerateAndStoreReportService(
        reportEventRepository,
        reportIssueRepository,
        reportGenerator,
        appConfig
      )
      await(service.generateAndStore(GenerateReportRequest(1, 0, 0), "Z1234"))

      val documents = ArgumentCaptor.forClass(classOf[Seq[ReportIssueDocument]])
      verify(reportIssueRepository).insertMany(documents.capture())
      documents.getValue                   should have size 1
      documents.getValue.head.zReference shouldBe Some("Z1234")
    }
  }
}

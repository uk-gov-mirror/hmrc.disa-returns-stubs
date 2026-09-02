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

package uk.gov.hmrc.disareturnsstubs.services

import uk.gov.hmrc.disareturnsstubs.config.AppConfig
import uk.gov.hmrc.disareturnsstubs.models._
import uk.gov.hmrc.disareturnsstubs.models.generatereport.{GenerateReportRequest, ReportEvent, ReportIssueDocument}
import uk.gov.hmrc.disareturnsstubs.repositories.generatereport.{ReportEventRepository, ReportIssueRepository}

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GenerateAndStoreReportService @Inject() (
  reportEventRepository: ReportEventRepository,
  reportResultRepository: ReportIssueRepository,
  reportGenerator: GenerateReportIssuesService,
  appConfig: AppConfig
)(implicit ec: ExecutionContext) {

  def generateAndStore(
    generateReportRequest: GenerateReportRequest,
    zReference: String
  ): Future[Unit] = {

    val totalRequested =
      generateReportRequest.oversubscribed +
        generateReportRequest.traceAndMatch +
        generateReportRequest.failedEligibility

    if (totalRequested > appConfig.reportIssueLimit) {
      Future.failed(
        new IllegalArgumentException(
          s"[GenerateAndStoreReportService][generateAndStore] Requested $totalRequested records which exceeds the maximum allowed of ${appConfig.reportIssueLimit}"
        )
      )
    } else {

      val reportId = UUID.randomUUID().toString

      val results: Seq[ReturnResult] =
        reportGenerator.generateResults(generateReportRequest)

      val event = ReportEvent(
        reportId = reportId,
        zReference = zReference
      )

      val documents = results.map { result =>
        ReportIssueDocument(
          reportId = reportId,
          accountNumber = result.accountNumber,
          nino = result.nino,
          issueIdentified = result.issueIdentified,
          zReference = Some(zReference)
        )
      }

      for {
        _ <- reportEventRepository.upsert(event)
        _ <- reportResultRepository.insertMany(documents)
      } yield ()
    }
  }
}

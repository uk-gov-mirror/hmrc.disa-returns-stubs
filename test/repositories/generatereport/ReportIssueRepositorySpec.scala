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

package repositories.generatereport

import org.mongodb.scala.{ObservableFuture, SingleObservableFuture}
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.Helpers.await
import uk.gov.hmrc.disareturnsstubs.config.AppConfig
import uk.gov.hmrc.disareturnsstubs.models.IssueIdentifiedMessage
import uk.gov.hmrc.disareturnsstubs.models.generatereport.ReportIssueDocument
import uk.gov.hmrc.disareturnsstubs.repositories.generatereport.ReportIssueRepository
import uk.gov.hmrc.mongo.MongoComponent
import utils.BaseUnitSpec

import java.time.Instant

class ReportIssueRepositorySpec extends BaseUnitSpec {

  override lazy val app: Application      = new GuiceApplicationBuilder().build()
  lazy val mongoComponent: MongoComponent = inject[MongoComponent]
  lazy val appConfig: AppConfig           = inject[AppConfig]
  lazy val repo                           = new ReportIssueRepository(mongoComponent, appConfig)

  private def resetCollection(): Unit = {
    await(repo.collection.drop().toFuture())
    await(repo.ensureIndexes())
  }

  val issue1: ReportIssueDocument =
    ReportIssueDocument(
      reportId = "report-1",
      accountNumber = "100000001",
      nino = "TESTNINO",
      issueIdentified = IssueIdentifiedMessage(
        code = "UNABLE_TO_IDENTIFY_INVESTOR",
        message = "Unable To Identify Investor"
      ),
      createdAt = Instant.now()
    )

  val issue2: ReportIssueDocument =
    issue1.copy(
      accountNumber = "100000002",
      nino = "TESTNINO"
    )

  val issue3: ReportIssueDocument =
    issue1.copy(
      reportId = "report-2",
      accountNumber = "100000003"
    )

  "insertMany" should {

    "insert multiple report issues for a reportId" in {
      resetCollection()

      await(repo.insertMany(Seq(issue1, issue2)))

      val stored = await(repo.collection.find().toFuture())
      stored should contain theSameElementsAs Seq(issue1, issue2)
    }

    "do nothing when given an empty sequence" in {
      resetCollection()

      await(repo.insertMany(Seq.empty))

      val stored = await(repo.collection.find().toFuture())
      stored shouldBe empty
    }

  }

  "findByReportId" should {

    "return issues for the given reportId" in {
      resetCollection()

      await(repo.insertMany(Seq(issue1, issue2, issue3)))

      val results =
        await(repo.findByReportId("report-1", skip = 0, limit = 10))

      results should contain theSameElementsAs Seq(issue1, issue2)
    }

    "respect skip and limit parameters" in {
      resetCollection()

      val issues =
        (1 to 5).map { i =>
          issue1.copy(accountNumber = s"10000000$i")
        }

      await(repo.insertMany(issues))

      val results =
        await(repo.findByReportId("report-1", skip = 2, limit = 2))

      results.size shouldBe 2
    }

    "return empty sequence when no issues exist for the reportId" in {
      resetCollection()

      val results =
        await(repo.findByReportId("unknown-report", skip = 0, limit = 10))

      results shouldBe empty
    }

  }

  "countByReportId" should {

    "return the correct number of issues for a reportId" in {
      resetCollection()

      await(repo.insertMany(Seq(issue1, issue2, issue3)))

      val count =
        await(repo.countByReportId("report-1"))

      count shouldBe 2
    }

    "return zero when no issues exist for the reportId" in {
      resetCollection()

      val count =
        await(repo.countByReportId("missing-report"))

      count shouldBe 0
    }
  }

  "deleteByReportIds" should {
    "delete only issues for the supplied report IDs" in {
      resetCollection()
      await(repo.insertMany(Seq(issue1, issue2, issue3)))

      await(repo.deleteByReportIds(Seq("report-1")))

      await(repo.countByReportId("report-1")) shouldBe 0
      await(repo.countByReportId("report-2")) shouldBe 1
    }
  }
}

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
import org.mongodb.scala.result.UpdateResult
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.Helpers.await
import uk.gov.hmrc.disareturnsstubs.config.AppConfig
import uk.gov.hmrc.disareturnsstubs.models.generatereport.ReportEvent
import uk.gov.hmrc.disareturnsstubs.repositories.generatereport.ReportEventRepository
import uk.gov.hmrc.mongo.MongoComponent
import utils.BaseUnitSpec

import java.time.Instant

class ReportEventRepositorySpec extends BaseUnitSpec {

  override lazy val app: Application      = new GuiceApplicationBuilder().build()
  lazy val mongoComponent: MongoComponent = inject[MongoComponent]
  lazy val appConfig: AppConfig           = inject[AppConfig]
  lazy val repo                           = new ReportEventRepository(mongoComponent, appConfig)

  val event1: ReportEvent =
    ReportEvent(
      reportId = "report-1",
      zReference = validZReference,
      createdAt = Instant.now()
    )

  val event2: ReportEvent =
    event1.copy(reportId = "report-2")

  "upsert" should {

    "insert a new document when one does not already exist" in {
      await(repo.collection.drop().toFuture())

      val result: UpdateResult = await(repo.upsert(event1))

      result.wasAcknowledged() shouldBe true
      result.getUpsertedId       should not be null

      val stored = await(repo.collection.find().headOption())
      stored shouldBe Some(event1)

    }

    "replace the existing document if one already exists for the same zReference" in {
      await(repo.collection.drop().toFuture())

      await(repo.upsert(event1))
      val result: UpdateResult = await(repo.upsert(event2))

      result.wasAcknowledged() shouldBe true
      result.getUpsertedId     shouldBe null
      result.getModifiedCount    should be > 0L

      val stored = await(repo.collection.find().headOption())
      stored shouldBe Some(event2)

    }

    "allow inserting documents for different zReferences" in {
      await(repo.collection.drop().toFuture())

      val otherEvent = event1.copy(
        reportId = "report-3",
        zReference = "Z1235"
      )

      await(repo.upsert(event1))
      await(repo.upsert(otherEvent))

      val results = await(repo.collection.find().toFuture())
      results should contain theSameElementsAs Seq(event1, otherEvent)

    }
  }

  "find" should {

    "return the correct event for a given zReference" in {
      await(repo.collection.drop().toFuture())

      await(repo.upsert(event1))

      val result = await(repo.find(validZReference))
      result shouldBe Some(event1)

    }

    "return None if no event exists for the given zReference" in {
      await(repo.collection.drop().toFuture())

      val result = await(repo.find(validZReference))
      result shouldBe None

    }

    "return the correct event when multiple zReferences exist" in {
      await(repo.collection.drop().toFuture())

      val otherEvent = event1.copy(
        reportId = "report-4",
        zReference = "Z1235"
      )

      await(repo.upsert(event1))
      await(repo.upsert(otherEvent))

      val result      = await(repo.find(validZReference))
      val otherResult = await(repo.find("Z1235"))

      result      shouldBe Some(event1)
      otherResult shouldBe Some(otherEvent)

    }
  }

  "deleteByZReferences" should {
    "delete only events for the supplied Z-references" in {
      await(repo.collection.drop().toFuture())
      val otherEvent = event1.copy(reportId = "report-3", zReference = "Z1235")
      await(repo.upsert(event1))
      await(repo.upsert(otherEvent))

      await(repo.deleteByZReferences(Seq(validZReference)))

      await(repo.find(validZReference)) shouldBe None
      await(repo.find("Z1235"))         shouldBe Some(otherEvent)
    }
  }
}

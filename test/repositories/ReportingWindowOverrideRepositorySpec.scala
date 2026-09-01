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

package repositories

import org.bson.BsonType
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.documentToUntypedDocument
import org.scalatest.OptionValues.convertOptionToValuable
import play.api.test.Helpers.await
import uk.gov.hmrc.disareturnsstubs.config.AppConfig
import uk.gov.hmrc.disareturnsstubs.models.ReportingWindowOverrideRequest
import uk.gov.hmrc.disareturnsstubs.repositories.ReportingWindowOverrideRepository
import uk.gov.hmrc.mongo.MongoComponent
import utils.BaseUnitSpec

import java.time.{Clock, Instant, ZoneOffset}

class ReportingWindowOverrideRepositorySpec extends BaseUnitSpec {

  private val now                = Instant.parse("2026-08-25T12:00:00Z")
  private val clock              = Clock.fixed(now, ZoneOffset.UTC)
  private lazy val component     = inject[MongoComponent]
  private lazy val appConfig     = inject[AppConfig]
  private lazy val repository    = new ReportingWindowOverrideRepository(component, appConfig, clock)
  private lazy val rawCollection = component.database.getCollection[Document]("reportingWindowOverrides")

  override def beforeEach(): Unit = {
    super.beforeEach()
    await(repository.collection.drop().toFuture())
  }

  "ReportingWindowOverrideRepository" should {
    "configure an absolute TTL index" in {
      await(repository.ensureIndexes())

      val indexes = await(repository.collection.listIndexes().toFuture())
      val ttl     = indexes.find(_.getString("name") == "expiresAtTtlIdx").value

      ttl.get("key").value.asDocument().getInt32("expiresAt").getValue shouldBe 1
      ttl.get("expireAfterSeconds").value.asNumber().longValue         shouldBe 0
    }

    "upsert and replace an override with a refreshed one-hour expiry" in {
      await(
        repository.set(
          "Z1234",
          ReportingWindowOverrideRequest(now.minusSeconds(60), now.plusSeconds(60))
        )
      )

      val first = await(repository.getActive("Z1234")).value
      first.expiresAt shouldBe now.plusSeconds(3600)

      await(repository.set("Z1234", ReportingWindowOverrideRequest(now, now.plusSeconds(120))))
      val updated = await(repository.getActive("Z1234")).value

      updated.startDate                                        shouldBe now
      updated.endDate                                          shouldBe now.plusSeconds(120)
      await(repository.collection.countDocuments().toFuture()) shouldBe 1
    }

    "store timestamps as BSON dates for the TTL index" in {
      await(repository.set("Z1234", ReportingWindowOverrideRequest(now, now.plusSeconds(60))))

      val document = await(rawCollection.find(Filters.equal("_id", "Z1234")).first().toFuture())

      document.toBsonDocument.get("expiresAt").getBsonType shouldBe BsonType.DATE_TIME
      document.toBsonDocument.get("updatedAt").getBsonType shouldBe BsonType.DATE_TIME
    }

    "isolate overrides by Z-reference" in {
      await(repository.set("Z1234", ReportingWindowOverrideRequest(now, now.plusSeconds(60))))
      await(repository.set("Z5678", ReportingWindowOverrideRequest(now, now.plusSeconds(120))))

      await(repository.getActive("Z1234")).value.endDate shouldBe now.plusSeconds(60)
      await(repository.getActive("Z5678")).value.endDate shouldBe now.plusSeconds(120)
    }

    "delete only overrides for the supplied Z-references" in {
      await(repository.set("Z1234", ReportingWindowOverrideRequest(now, now.plusSeconds(60))))
      await(repository.set("Z5678", ReportingWindowOverrideRequest(now, now.plusSeconds(120))))

      await(repository.deleteByZReferences(Seq("Z1234")))

      await(repository.getActive("Z1234")) shouldBe None
      await(repository.getActive("Z5678"))   should not be empty
    }
  }
}

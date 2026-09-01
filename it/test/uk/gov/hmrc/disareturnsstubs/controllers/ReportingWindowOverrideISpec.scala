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

package uk.gov.hmrc.disareturnsstubs.controllers

import org.mongodb.scala.SingleObservableFuture
import org.scalatest.BeforeAndAfterEach
import play.api.Application
import play.api.http.HeaderNames.{AUTHORIZATION, CONTENT_TYPE}
import play.api.http.MimeTypes.JSON
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.disareturnsstubs.BaseISpec
import uk.gov.hmrc.disareturnsstubs.repositories.ReportingWindowOverrideRepository

import java.time.{Clock, Instant, ZoneOffset}

class ReportingWindowOverrideISpec extends BaseISpec with BeforeAndAfterEach {

  private val now   = Instant.parse("2026-08-25T12:00:00Z")
  private val clock = Clock.fixed(now, ZoneOffset.UTC)

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .overrides(bind[Clock].toInstance(clock))
      .build()

  private lazy val overrideRepository = inject[ReportingWindowOverrideRepository]

  override def beforeEach(): Unit = {
    super.beforeEach()
    await(overrideRepository.collection.drop().toFuture())
  }

  "reporting window override journey" should {
    "store and apply an override only to the normalized Z-reference" in {
      val setRequest = FakeRequest(PUT, "/reporting-window-override/z1234")
        .withHeaders(CONTENT_TYPE -> JSON)
        .withJsonBody(
          Json.obj(
            "startDate" -> now.minusSeconds(60).toString,
            "endDate"   -> now.plusSeconds(60).toString
          )
        )

      status(route(app, setRequest).get) mustBe NO_CONTENT

      val overridden = route(
        app,
        FakeRequest(GET, "/disa-returns-submission/reporting-window/status/Z1234")
          .withHeaders(AUTHORIZATION -> "internal-auth-token")
      ).get
      val otherUser = route(
        app,
        FakeRequest(GET, "/disa-returns-submission/reporting-window/status/Z5678")
          .withHeaders(AUTHORIZATION -> "internal-auth-token")
      ).get

      (contentAsJson(overridden) \ "reportingWindowOpen").as[Boolean] mustBe true
      (contentAsJson(otherUser) \ "reportingWindowOpen").as[Boolean] mustBe false
    }

    "reject an invalid override without replacing the existing value" in {
      val request = FakeRequest(PUT, "/reporting-window-override/Z1234")
        .withHeaders(CONTENT_TYPE -> JSON)
        .withJsonBody(
          Json.obj(
            "startDate" -> now.plusSeconds(60).toString,
            "endDate"   -> now.toString
          )
        )

      status(route(app, request).get) mustBe BAD_REQUEST
      await(overrideRepository.getActive("Z1234")) mustBe None
    }

    "reject invalid Z-references on write and status routes" in {
      val writeRequest = FakeRequest(PUT, "/reporting-window-override/Z123")
        .withHeaders(CONTENT_TYPE -> JSON)
        .withJsonBody(
          Json.obj(
            "startDate" -> now.minusSeconds(60).toString,
            "endDate"   -> now.plusSeconds(60).toString
          )
        )
      val statusRequest = FakeRequest(GET, "/disa-returns-submission/reporting-window/status/not-a-z-reference")
        .withHeaders(AUTHORIZATION -> "internal-auth-token")

      status(route(app, writeRequest).get) mustBe BAD_REQUEST
      status(route(app, statusRequest).get) mustBe BAD_REQUEST
    }
  }
}

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

import org.apache.pekko.stream.Materializer
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.{never, verify, when}
import play.api.http.HeaderNames.CONTENT_TYPE
import play.api.http.MimeTypes.JSON
import play.api.http.Status.{BAD_REQUEST, NO_CONTENT}
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.{PUT, contentAsJson, status, stubControllerComponents}
import uk.gov.hmrc.disareturnsstubs.controllers.ReportingWindowOverrideController
import uk.gov.hmrc.disareturnsstubs.models.ReportingWindowOverrideRequest
import uk.gov.hmrc.disareturnsstubs.repositories.ReportingWindowOverrideRepository
import utils.BaseUnitSpec

import java.time.Instant
import scala.concurrent.Future

class ReportingWindowOverrideControllerSpec extends BaseUnitSpec {

  private implicit lazy val materializer: Materializer = app.materializer
  private val startDate                                = Instant.parse("2026-08-25T11:00:00Z")
  private val endDate                                  = Instant.parse("2026-08-25T13:00:00Z")
  private val validBody                                = Json.obj(
    "startDate" -> startDate.toString,
    "endDate"   -> endDate.toString
  )

  private def request(body: play.api.libs.json.JsValue) =
    FakeRequest(PUT, "/reporting-window-override/z1234").withHeaders(CONTENT_TYPE -> JSON).withBody(body)

  "set" should {
    "store a valid override using the normalized Z-reference" in {
      val repository = mock[ReportingWindowOverrideRepository]
      val controller = new ReportingWindowOverrideController(stubControllerComponents(), repository)
      when(repository.set(eqTo("Z1234"), any())).thenReturn(Future.unit)

      val result = controller.set(" z1234 ")(request(validBody))

      status(result) shouldBe NO_CONTENT
      verify(repository).set("Z1234", ReportingWindowOverrideRequest(startDate, endDate))
    }

    "return BadRequest for an invalid Z-reference" in {
      val repository = mock[ReportingWindowOverrideRepository]
      val controller = new ReportingWindowOverrideController(stubControllerComponents(), repository)

      val result = controller.set("Z123")(request(validBody))

      status(result)                               shouldBe BAD_REQUEST
      (contentAsJson(result) \ "error").as[String] shouldBe "Invalid zReference"
      verify(repository, never()).set(any(), any())
    }

    "return BadRequest for an invalid override" in {
      val repository  = mock[ReportingWindowOverrideRepository]
      val controller  = new ReportingWindowOverrideController(stubControllerComponents(), repository)
      val invalidBody = Json.obj(
        "startDate" -> endDate.toString,
        "endDate"   -> startDate.toString
      )

      val result = controller.set("Z1234")(request(invalidBody))

      status(result)                               shouldBe BAD_REQUEST
      (contentAsJson(result) \ "error").as[String] shouldBe "Invalid reporting window override"
      verify(repository, never()).set(any(), any())
    }
  }
}

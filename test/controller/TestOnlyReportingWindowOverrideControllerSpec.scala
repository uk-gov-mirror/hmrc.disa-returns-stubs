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
import org.mockito.Mockito.{never, verify, when}
import play.api.http.HeaderNames.CONTENT_TYPE
import play.api.http.MimeTypes.JSON
import play.api.http.Status.{BAD_REQUEST, NO_CONTENT}
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.{POST, status, stubControllerComponents}
import uk.gov.hmrc.disareturnsstubs.repositories.ReportingWindowOverrideRepository
import uk.gov.hmrc.disareturnsstubs.testOnly.controllers.TestOnlyReportingWindowOverrideController
import utils.BaseUnitSpec

import scala.concurrent.Future

class TestOnlyReportingWindowOverrideControllerSpec extends BaseUnitSpec {

  private def request(zReferences: Seq[String]) =
    FakeRequest(POST, "/test-only/reporting-window-overrides/cleanup")
      .withHeaders(CONTENT_TYPE -> JSON)
      .withBody(Json.obj("zReferences" -> zReferences))

  "delete" should {
    "delete overrides for normalized Z-references" in {
      val repository  = mock[ReportingWindowOverrideRepository]
      val controller  = new TestOnlyReportingWindowOverrideController(stubControllerComponents(), repository)
      val zReferences = Seq("Z1234", "Z5678")
      when(repository.deleteByZReferences(zReferences)).thenReturn(Future.unit)

      val result = controller.delete()(request(Seq("z1234", "Z5678", "Z1234")))

      status(result) shouldBe NO_CONTENT
      verify(repository).deleteByZReferences(zReferences)
    }

    "return BadRequest when a Z-reference is invalid" in {
      val repository = mock[ReportingWindowOverrideRepository]
      val controller = new TestOnlyReportingWindowOverrideController(stubControllerComponents(), repository)

      val result = controller.delete()(request(Seq("invalid")))

      status(result) shouldBe BAD_REQUEST
      verify(repository, never()).deleteByZReferences(any())
    }
  }
}

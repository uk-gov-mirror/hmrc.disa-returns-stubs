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

import org.mockito.ArgumentMatchers.eq as eqTo
import org.mockito.Mockito.{never, verify, when}
import play.api.http.Status.{BAD_REQUEST, OK}
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentAsJson, status, stubControllerComponents}
import uk.gov.hmrc.disareturnsstubs.controllers.ReportingWindowController
import uk.gov.hmrc.disareturnsstubs.services.ReportingWindowService
import utils.BaseUnitSpec

import scala.concurrent.Future

class ReportingWindowControllerSpec extends BaseUnitSpec {

  "status" should {
    "use the normalized Z-reference path parameter" in {
      val service    = mock[ReportingWindowService]
      val controller = new ReportingWindowController(stubControllerComponents(), service, stubAuthFilter)
      when(service.isOpen(eqTo("Z1234"))).thenReturn(Future.successful(true))

      val result = controller.status(" z1234 ")(FakeRequest())

      status(result)                                              shouldBe OK
      (contentAsJson(result) \ "reportingWindowOpen").as[Boolean] shouldBe true
      verify(service).isOpen("Z1234")
    }

    "return a closed status from the service" in {
      val service    = mock[ReportingWindowService]
      val controller = new ReportingWindowController(stubControllerComponents(), service, stubAuthFilter)
      when(service.isOpen(eqTo("Z1234"))).thenReturn(Future.successful(false))

      val result = controller.status("Z1234")(FakeRequest())

      status(result)                                              shouldBe OK
      (contentAsJson(result) \ "reportingWindowOpen").as[Boolean] shouldBe false
    }

    "return BadRequest for an invalid Z-reference" in {
      val service    = mock[ReportingWindowService]
      val controller = new ReportingWindowController(stubControllerComponents(), service, stubAuthFilter)

      val result = controller.status("Z12345")(FakeRequest())

      status(result)                               shouldBe BAD_REQUEST
      (contentAsJson(result) \ "error").as[String] shouldBe "Invalid zReference"
      verify(service, never()).isOpen(org.mockito.ArgumentMatchers.any())
    }
  }
}

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

import org.mockito.Mockito.when
import uk.gov.hmrc.disareturnsstubs.config.AppConfig
import uk.gov.hmrc.disareturnsstubs.models.ReportingWindowOverride
import uk.gov.hmrc.disareturnsstubs.repositories.ReportingWindowOverrideRepository
import uk.gov.hmrc.disareturnsstubs.services.ReportingWindowService
import utils.BaseUnitSpec

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.Future

class ReportingWindowServiceSpec extends BaseUnitSpec {

  private val repository = mock[ReportingWindowOverrideRepository]
  private val config     = mock[AppConfig]
  private val now        = Instant.parse("2026-08-25T12:00:00Z")
  private val clock      = Clock.fixed(now, ZoneOffset.UTC)
  private val service    = new ReportingWindowService(repository, config, clock)

  override def beforeEach(): Unit = {
    super.beforeEach()
    when(config.declarationPeriodStart).thenReturn(6)
    when(config.declarationPeriodEnd).thenReturn(19)
  }

  "isOpen" should {
    "use an active override for the Z-reference" in {
      when(repository.getActive("Z1234")).thenReturn(
        Future.successful(
          Some(
            ReportingWindowOverride(
              "Z1234",
              now.minusSeconds(60),
              now.plusSeconds(60),
              now.plusSeconds(3600),
              now
            )
          )
        )
      )

      service.isOpen("Z1234").futureValue shouldBe true
    }

    "treat the window as closed when the current instant is outside an active override" in {
      when(repository.getActive("Z1234")).thenReturn(
        Future.successful(
          Some(
            ReportingWindowOverride(
              "Z1234",
              now.minusSeconds(120),
              now.minusSeconds(60),
              now.plusSeconds(3600),
              now
            )
          )
        )
      )

      service.isOpen("Z1234").futureValue shouldBe false
    }

    "fall back to the normal reporting period when no override exists" in {
      when(repository.getActive("Z1234")).thenReturn(Future.successful(None))

      service.isOpen("Z1234").futureValue shouldBe false
    }

    "use inclusive override boundaries" in {
      when(repository.getActive("Z1234")).thenReturn(
        Future.successful(Some(ReportingWindowOverride("Z1234", now, now, now.plusSeconds(3600), now)))
      )

      service.isOpen("Z1234").futureValue shouldBe true
    }
  }
}

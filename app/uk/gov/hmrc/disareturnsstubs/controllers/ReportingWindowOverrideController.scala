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

import jakarta.inject.{Inject, Singleton}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, ControllerComponents}
import uk.gov.hmrc.disareturnsstubs.models.{ReportingWindowOverrideRequest, ZReference}
import uk.gov.hmrc.disareturnsstubs.repositories.ReportingWindowOverrideRepository
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ReportingWindowOverrideController @Inject() (
  cc: ControllerComponents,
  repository: ReportingWindowOverrideRepository
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def set(zReference: String): Action[JsValue] = Action.async(parse.json) { request =>
    ZReference.normalize(zReference) match {
      case None                       => Future.successful(BadRequest(Json.obj("error" -> "Invalid zReference")))
      case Some(normalizedZReference) =>
        request.body
          .validate[ReportingWindowOverrideRequest]
          .fold(
            _ => Future.successful(BadRequest(Json.obj("error" -> "Invalid reporting window override"))),
            overrideRequest => repository.set(normalizedZReference, overrideRequest).map(_ => NoContent)
          )
    }
  }
}

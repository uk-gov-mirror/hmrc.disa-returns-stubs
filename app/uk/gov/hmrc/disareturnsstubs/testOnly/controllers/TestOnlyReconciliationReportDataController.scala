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

package uk.gov.hmrc.disareturnsstubs.testOnly.controllers

import jakarta.inject.{Inject, Singleton}
import play.api.libs.json.JsValue
import play.api.mvc.{Action, ControllerComponents}
import uk.gov.hmrc.disareturnsstubs.models.ZReference
import uk.gov.hmrc.disareturnsstubs.repositories.generatereport.{ReportEventRepository, ReportIssueRepository}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TestOnlyReconciliationReportDataController @Inject() (
  cc: ControllerComponents,
  reportEventRepository: ReportEventRepository,
  reportIssueRepository: ReportIssueRepository
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def delete(): Action[JsValue] = Action.async(parse.json) { request =>
    (request.body \ "zReferences").validate[Seq[String]].asOpt match {
      case Some(zReferences) if zReferences.nonEmpty =>
        val normalized = zReferences.map(ZReference.normalize)

        if (normalized.exists(_.isEmpty)) {
          Future.successful(BadRequest)
        } else {
          val normalizedZReferences = normalized.flatten.distinct

          for {
            reportEvents <- reportEventRepository.findByZReferences(normalizedZReferences)
            _            <- reportIssueRepository.deleteByReportIds(reportEvents.map(_.reportId))
            _            <- reportEventRepository.deleteByZReferences(normalizedZReferences)
          } yield NoContent
        }
      case _                                         => Future.successful(BadRequest)
    }
  }
}

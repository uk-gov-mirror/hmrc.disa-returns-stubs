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

package uk.gov.hmrc.disareturnsstubs.repositories.generatereport

import org.mongodb.scala.model.Filters.{equal, in}
import org.mongodb.scala.model.{IndexModel, IndexOptions, Indexes, Sorts}
import play.api.Logging
import uk.gov.hmrc.disareturnsstubs.config.AppConfig
import uk.gov.hmrc.disareturnsstubs.models.generatereport.ReportIssueDocument
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

import java.util.concurrent.TimeUnit
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ReportIssueRepository @Inject() (mc: MongoComponent, appConfig: AppConfig)(implicit ec: ExecutionContext)
    extends PlayMongoRepository[ReportIssueDocument](
      mongoComponent = mc,
      collectionName = "reportResults",
      domainFormat = ReportIssueDocument.format,
      indexes = Seq(
        IndexModel(
          Indexes.compoundIndex(
            Indexes.ascending("reportId"),
            Indexes.ascending("_id")
          )
        ),
        IndexModel(
          Indexes.ascending("createdAt"),
          IndexOptions()
            .name("createdAt_ttl_index")
            .expireAfter(appConfig.reportTtlDays.toLong, TimeUnit.DAYS)
        )
      )
    )
    with Logging {

  def insertMany(results: Seq[ReportIssueDocument]): Future[Unit] = {
    logger.debug(s"[ReportIssueRepository][insertMany] Inserting ${results.size} report results")
    if (results.isEmpty) Future.successful(())
    else collection.insertMany(results).toFuture().map(_ => ())
  }

  def findByReportId(
    reportId: String,
    skip: Int,
    limit: Int
  ): Future[Seq[ReportIssueDocument]] =
    collection
      .find(equal("reportId", reportId))
      .sort(Sorts.ascending("_id"))
      .skip(skip)
      .limit(limit)
      .toFuture()

  def countByReportId(reportId: String): Future[Long] =
    collection
      .countDocuments(equal("reportId", reportId))
      .toFuture()

  def deleteByReportIds(reportIds: Seq[String]): Future[Unit] =
    if (reportIds.isEmpty) Future.successful(())
    else collection.deleteMany(in("reportId", reportIds: _*)).toFuture().map(_ => ())
}


# disa-returns-stubs

This service provides stubs for the external and internal services used by the [ISA Returns service](https://github.com/hmrc/disa-returns). It supports local development and automated testing by simulating NPS, return submission and Upscan interactions.

### Before running the app

This repository relies on having mongodb running locally. You can start it with:

```bash
# first check to see if mongo is already running
docker ps | grep mongodb

# if not, start it
docker run --restart unless-stopped --name mongodb -p 27017:27017 -d percona/percona-server-mongodb:7.0 --replSet rs0
```

Reference instructions for [setting up docker](https://docs.tax.service.gov.uk/mdtp-handbook/documentation/developer-set-up/install-docker.html) and [running mongodb](https://docs.tax.service.gov.uk/mdtp-handbook/documentation/developer-set-up/set-up-mongodb.html#install-mongodb-applesilicon-mac).

### Running the app locally

```bash
sbt run -Dapplication.router=testOnlyDoNotUseInAppConf.Routes
```

You can then query the app to ensure it is working with the following command:

```bash
# other useful commands
sbt clean

sbt reload

sbt compile
```

### Running the test suite

To run the unit tests:

```bash
sbt test
```

To run the integration tests:

```bash
sbt it/test
```

### Before you commit

This service leverages scalaFmt to ensure that the code is formatted correctly.

Before you commit, please run the following commands to check that the code is formatted correctly:

```bash
# checks all source and sbt files are correctly formatted
sbt prePrChecks

# if checks fail, you can format with the following commands

# formats all source files
sbt scalafmtAll

# formats all sbt files
sbt scalafmtSbt

# formats just the main source files (excludes test and configuration files)
sbt scalafmt
```
# Stubbed Endpoints:

## NPS submit monthly return

- This endpoint is used to submit ISA monthly reporting data to NPS.

### Endpoint:
```bash
POST /nps/submit/:zReference
```

### Z Reference Based Responses:

| Z_REF | Status |        Type         |
|:-----:|:------:|:-------------------:|
| Z1400 |  400   |     BAD REQUEST     |
| Z1503 |  503   | SERVICE UNAVAILABLE |
|  Any  |  204   |     NO CONTENT      |

## NPS Retrieve Reconciliation Report

- This endpoint is used to retrieve reconciliation report from NPS.

### Endpoint:
```bash
GET /monthly/:zReference/:taxYear/:month/results
```

- This endpoint requires a report to be generated either via the stub setup endpoint or disa-returns-test-support-api.
- If no report is generated then any Z_REF other than Z1500 will return 404 NOT_FOUND

### Z Reference Based Responses:

| Z_REF | Status  |         Type          |
|:-----:|:-------:|:---------------------:|
| Z1500 |   500   | INTERNAL SERVER ERROR |
|  Any  | 200/404 | NO CONTENT/NOT FOUND  |

## Create Monthly Return

- This endpoint creates a monthly return in the stubbed returns submission service and returns a generated submission ID.

### Endpoint:
```bash
POST /disa-returns-submission/monthly/:zReference/:taxYear/:month
```

### Responses:

| Scenario | Status | Type |
|:---------|:------:|:----:|
| Monthly return created | 201 | CREATED |

## Store Monthly Return Submission

- This endpoint simulates storing monthly return submission data.

### Endpoint:
```bash
PUT /disa-returns-submission/monthly/:zReference/:taxYear/:month/submissions/:submissionId
```

### Responses:

| Scenario | Status | Type |
|:---------|:------:|:----:|
| Submission stored | 200 | OK |

## Declare Monthly Return

- This endpoint simulates declaring a monthly return.

### Endpoint:
```bash
POST /disa-returns-submission/monthly/:zReference/:taxYear/:month/declarations
```

### Responses:

| Scenario | Status | Type |
|:---------|:------:|:----:|
| Monthly return declared | 200 | OK |

## Retrieve Reporting Window Status

- Returns the reporting-window status for a Z-reference. The path value is case-normalized and must be `Z` followed by four digits.

### Endpoint:
```bash
GET /disa-returns-submission/reporting-window/status/:zReference
```

### Responses:

| Scenario | Status | Type |
|:---------|:------:|:----:|
| Valid Z-reference | 200 | OK |
| Invalid Z-reference | 400 | BAD REQUEST |


## ETMP Retrieve Obligation Status

- This endpoint is used to check the obligation status in ETMP.
- If the supplied zReference is not found in mongo, then it will store the obligation as open.
- If the supplied zReference is found in mongo, the store obligation status will be returned.

### Endpoint:
```bash
GET /etmp/check-obligation-status/:zReference
```
### Responses:

|                         Scenario                         | Status |   Type    |
|:--------------------------------------------------------:|:------:|:---------:|
|          Successfully returns obligation status          |  200   |    OK     |

## ETMP Retrieve Reporting Window Status

- This endpoint is used to check the reporting window status in ETMP.

### Endpoint:
```bash
GET /etmp/check-reporting-window
```

### Responses:

|                   Scenario                   | Status |    Type    |
|:--------------------------------------------:|:------:|:----------:|
| Successfully returns reporting window status |  204   | NO CONTENT |
|          Reporting window not found          |  404   | NOT FOUND  |

## ETMP Submit Updated Obligation Status

- This endpoint is used to update the obligation status to closed/already met in ETMP.

### Endpoint:
```bash
POST /etmp/declaration/:zReference
```

### Responses:

|                   Scenario                   | Status |   Type    |
|:--------------------------------------------:|:------:|:---------:|
|         Successful         |  204   | NO CONTENT |

## Upscan Initiate

- This endpoint proxies the upscan `v2/initiate` request to `upscan-stub`, rewriting the returned `uploadRequest.href` so that the subsequent upload is routed back through this stub's `/upscan/upload` endpoint.

### Endpoint:
```bash
POST /upscan/v2/initiate
```

### Responses:

|                                  Scenario                                   | Status | Type |
|:----------------------------------------------------------------------------:|:------:|:----:|
| Returns the upscan-stub response, with `uploadRequest.href` rewritten to this stub |  200   |  OK  |

## Upscan Upload

- This endpoint proxies file uploads to `upscan-stub`, allowing `disa-returns-frontend` to test its upscan integration locally.
- Most uploads are forwarded to `upscan-stub` unchanged, and the resulting redirect (`success_action_redirect`/`error_action_redirect`) is passed back to the browser.
- Certain scenarios that `upscan-stub` does not support are intercepted by this stub directly. Other scenarios are simulated by `upscan-stub` based on the **uploaded filename**, allowing different upscan error and processing outcomes to be triggered locally.

### Endpoint:
```bash
POST /upscan/upload
```

### Filename Based Scenarios:

|                Scenario                | Filename Convention                                       |             Example             | Behaviour                                                                                                |
|:----------------------------------------:|:------------------------------------------------------------|:----------------------------------:|:------------------------------------------------------------------------------------------------------------|
|     No file selected / empty file       | filename contains `empty`, or no `file` part is provided    |          `empty-return.csv`         | Intercepted by this stub and the request is redirected to `error_action_redirect` with `errorCode=EntityTooSmall` |
|         Disallowed MIME type            | the uploaded file's `Content-Type` is not `text/csv` or `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet` | a `.pdf` file uploaded with `Content-Type: application/pdf` | Intercepted by this stub, which mimics real upscan: the request is redirected to `success_action_redirect` (with `key`), and a `FAILED`/`REJECTED` callback is sent asynchronously to `x-amz-meta-callback-url` |
| Reject with a specific S3 error code    | `reject.<S3_ERROR_CODE>.<EXT>`                               |   `reject.UnexpectedContent.png`    | Forwarded to upscan-stub, which redirects to `error_action_redirect` with `errorCode=<S3_ERROR_CODE>`     |
|         File flagged as infected        | `infected.<VIRUS_NAME>.<EXT>`                                |        `infected.MyDoom.jpeg`       | Forwarded to upscan-stub, which processes the upload normally but reports the file as quarantined with the given virus name in the callback notification |
|         File rejected by upscan         | `invalid.<REASON>.<EXT>`                                     |       `invalid.ZipInDisguise.txt`   | Forwarded to upscan-stub, which processes the upload normally but reports the file as rejected with the given reason in the callback notification |
|    File fails for an unknown reason     | `unknown.<REASON>.<EXT>`                                     |        `unknown.SpookyCookie.pdf`   | Forwarded to upscan-stub, which processes the upload normally but reports the file as failed with the given reason in the callback notification |
|               Any other file            | -                                                             |          `valid-return.csv`         | Forwarded to upscan-stub and processed normally                                                          |

- For the full list of valid `S3_ERROR_CODE` values for the `reject.*` scenario, see the [AWS S3 ErrorCodeList](https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#ErrorCodeList).
- The `reject`, `infected`, `invalid` and `unknown` scenarios are simulated by `upscan-stub` itself - see its [`UploadController`](https://github.com/hmrc/upscan-stub/blob/main/app/uk/gov/hmrc/upscanstub/controller/UploadController.scala) for implementation details.

# Stub Setup Endpoints

Test-only endpoints require the service to run with
`-Dapplication.router=testOnlyDoNotUseInAppConf.Routes`.

## Clean Reconciliation Report Data

Deletes report events and their currently associated report issues for the supplied Z-references. It must not run
during active traffic for those Z-references.

### Endpoint:
```bash
POST /test-only/reconciliation-report-data/cleanup
Content-Type: application/json
```

### Request Body Example:
```json
{
  "zReferences": ["Z1000", "Z1001"]
}
```

### Responses:

| Scenario | Status | Type |
|:---------|:------:|:----:|
| Data deleted | 204 | NO CONTENT |
| Empty or invalid request body | 400 | BAD REQUEST |

Z-references are case-normalized and deduplicated before deletion.

## Clean Reporting Window Overrides

Deletes reporting-window overrides for the supplied Z-references. It must not run during active traffic for those
Z-references.

### Endpoint:
```bash
POST /test-only/reporting-window-overrides/cleanup
Content-Type: application/json
```

### Request Body Example:
```json
{
  "zReferences": ["Z5000", "Z5001"]
}
```

### Responses:

| Scenario | Status | Type |
|:---------|:------:|:----:|
| Data deleted | 204 | NO CONTENT |
| Empty or invalid request body | 400 | BAD REQUEST |

Z-references are case-normalized and deduplicated before deletion.

## Set Reporting Window Override

- Stores a temporary reporting-window override for a Z-reference. The normalized Z-reference is used as the record ID.

### Endpoint:
```bash
PUT /reporting-window-override/:zReference
```

### Responses:

| Scenario | Status | Type |
|:---------|:------:|:----:|
| Override stored | 204 | NO CONTENT |
| Invalid Z-reference or request body | 400 | BAD REQUEST |

## ETMP Open Obligation Status

- This setup endpoint is used to open the obligation status for the supplied zReference.

### Endpoint:
```bash
POST /etmp/open-obligation-status/:zReference
```

### Responses:

|               Scenario                | Status | Type |
|:-------------------------------------:|:------:|:----:|
| Successfully opened obligation status |  200   |  OK  |


## ETMP Set Reporting Window Status

- This setup endpoint is used to set the ETMP reporting window status.
- Simulates both reporting window open and closed for the stubbed ETMP reporting window status endpoint.

### Endpoint:
```bash
POST /etmp/reporting-window-state
```

### Responses:

|             Scenario              | Status |    Type     |
|:---------------------------------:|:------:|:-----------:|
|  Missing or invalid request body  |  400   | BAD REQUEST |
| Successfully set reporting window |  200   | NO CONTENT  |

## ETMP Retrieve Reporting Window Status

- This setup endpoint is used to retrieve the ETMP reporting window status.

### Endpoint:
```bash
GET /etmp/reporting-window-state
```

### Responses:

|               Scenario                | Status |   Type    |
|:-------------------------------------:|:------:|:---------:|
|      Not found reporting window       |  404   | NOT FOUND |
| Successfully returns reporting window |  200   |    OK     |

## NPS Generate Reconciliation report

- This test-support endpoint is used to generate an NPS reconciliation report for the supplied zReference.
- You can generate reports containing issues identified: traceAndMatch, oversubscribed & failedEligibility.
- The number supplied for each field in the request body determines how many issues of that type will be generated in the report.

### Endpoint:
```bash
POST /monthly/:zReference/reconciliation
```

### Request Body Example:
```bash 
json { "oversubscribed": 100000, "traceAndMatch": 100000, "failedEligibility": 100000 } 
```

### Responses:

|           Scenario            | Status |         Type          |
|:-----------------------------:|:------:|:---------------------:|
| Successfully generated report |  204   |    NO CONTENT         | 
|            failed             |  500   | INTERNAL SERVER ERROR |


### Further documentation

You can view further information regarding this service via our [service guide](#).

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EstimationQueries.R
\name{getSccsMetaEstimation}
\alias{getSccsMetaEstimation}
\title{Extract the self controlled case series (sccs) meta analysis results}
\usage{
getSccsMetaEstimation(
  connectionHandler,
  schema,
  sccsTablePrefix = "sccs_",
  cgTablePrefix = "cg_",
  esTablePrefix = "es_",
  targetIds = NULL,
  outcomeIds = NULL
)
}
\arguments{
\item{connectionHandler}{A connection handler that connects to the database and extracts sql queries.  Create a connection handler via `ResultModelManager::ConnectionHandler$new()`.}

\item{schema}{The result database schema (e.g., 'main' for sqlite)}

\item{sccsTablePrefix}{The prefix used for the cohort generator results tables}

\item{cgTablePrefix}{The prefix used for the cohort generator results tables}

\item{esTablePrefix}{The prefix used for the evidence synthesis results tables}

\item{targetIds}{A vector of integers corresponding to the target cohort IDs}

\item{outcomeIds}{A vector of integers corresponding to the outcome cohort IDs}
}
\value{
Returns a data.frame with the columns:
\itemize{
 #'  \item{databaseName the database name}
 \item{analysisId the analysis unique identifier}
 \item{description an analysis description}
 \item{targetName the target name}
 \item{targetId the target cohort id}
 \item{outcomeName the outcome name}
 \item{outcomeId the outcome cohort id}
 \item{indicationName the indicationname}
 \item{indicationId the indication cohort id}
 \item{covariateName whether main or secondary analysis}
 \item{outcomeSubjects The number of subjects with at least one outcome.}
 \item{outcomeEvents The number of outcome events.}
 \item{outcomeObservationPeriods The number of observation periods containing at least one outcome.}
 \item{covariateSubjects The number of subjects having the covariate.}
 \item{covariateDays The total covariate time in days.}
 \item{covariateEras The number of continuous eras of the covariate.}
 \item{covariateOutcomes The number of outcomes observed during the covariate time.}
 \item{observedDays The number of days subjects were observed.}
 \item{calibratedRr the calibrated relative risk}
 \item{calibratedCi95Lb the lower bound of the 95 percent confidence interval for the calibrated relative risk}
 \item{calibratedCi95Ub the upper bound of the 95 percent confidence interval for the calibrated relative risk}
 \item{calibratedP the calibrated p-value}
 \item{calibratedOneSidedP the calibrated one sided p-value}
 \item{calibratedLogRr the calibrated log of the relative risk}
 \item{calibratedSeLogRr the calibrated log of the relative risk standard error}
 \item{nDatabases The number of databases included in the estimate.}
 }
}
\description{
This function extracts any meta analysis estimation results for sccs.
}
\details{
Specify the connectionHandler, the schema and the targetoutcome cohort IDs
}
\examples{
conDet <- getExampleConnectionDetails()

connectionHandler <- ResultModelManager::ConnectionHandler$new(conDet)

sccsMeta <- getSccsMetaEstimation(
  connectionHandler = connectionHandler, 
  schema = 'main',
  targetIds = 1,
  outcomeIds = 3
)

}
\seealso{
Other Estimation: 
\code{\link{getCMEstimation}()},
\code{\link{getCmDiagnosticsData}()},
\code{\link{getCmMetaEstimation}()},
\code{\link{getSccsDiagnosticsData}()},
\code{\link{getSccsEstimation}()},
\code{\link{plotCmEstimates}()},
\code{\link{plotSccsEstimates}()}
}
\concept{Estimation}

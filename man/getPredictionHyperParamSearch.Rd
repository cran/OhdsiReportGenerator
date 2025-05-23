% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PredictionQueries.R
\name{getPredictionHyperParamSearch}
\alias{getPredictionHyperParamSearch}
\title{Extract hyper parameters details}
\usage{
getPredictionHyperParamSearch(
  connectionHandler,
  schema,
  plpTablePrefix = "plp_",
  modelDesignId = NULL,
  databaseId = NULL
)
}
\arguments{
\item{connectionHandler}{A connection handler that connects to the database and extracts sql queries.  Create a connection handler via `ResultModelManager::ConnectionHandler$new()`.}

\item{schema}{The result database schema (e.g., 'main' for sqlite)}

\item{plpTablePrefix}{The prefix used for the patient level prediction results tables}

\item{modelDesignId}{The identifier for a model design  to restrict to}

\item{databaseId}{The identifier for the development database to restrict  to}
}
\value{
Returns a data.frame with the columns:
\itemize{
 \item{metric the hyperparameter optimization metric}
 \item{fold the fold in cross validation}
 \item{value the metric value for the fold with the specified hyperparameter combination}
 } 
 plus columns for all the hyperparameters and their values
}
\description{
This function extracts the hyper parameters details
}
\details{
Specify the connectionHandler, the resultDatabaseSettings, the modelDesignId and the databaseId
}
\examples{
conDet <- getExampleConnectionDetails()

connectionHandler <- ResultModelManager::ConnectionHandler$new(conDet)

hyperParams <- getPredictionHyperParamSearch(
  connectionHandler = connectionHandler, 
  schema = 'main'
)

}
\seealso{
Other Prediction: 
\code{\link{getPredictionCohorts}()},
\code{\link{getPredictionDiagnosticTable}()},
\code{\link{getPredictionDiagnostics}()},
\code{\link{getPredictionIntercept}()},
\code{\link{getPredictionModelDesigns}()},
\code{\link{getPredictionPerformanceTable}()},
\code{\link{getPredictionPerformances}()},
\code{\link{getPredictionTopPredictors}()}
}
\concept{Prediction}

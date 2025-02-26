## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)

## ----echo=TRUE----------------------------------------------------------------
library(OhdsiReportGenerator)
# create a connection details object to 
connectionDetails <- getExampleConnectionDetails()

# create a connection handler to the results
library(ResultModelManager)
ConnectionHandler <- ResultModelManager::ConnectionHandler$new(connectionDetails)


## ----echo=TRUE----------------------------------------------------------------

cohorts <- getCohortDefinitions(
    connectionHandler = ConnectionHandler,
    schema = 'main',
    cgTablePrefix = 'cg_',
    targetIds = NULL
)

knitr::kable(
  x = cohorts %>% 
    dplyr::select(-"json", -"sqlCommand"), 
  caption = 'The data.frame extracted containing the cohort details minus the json and sqlCommand columns.'
  )


## ----echo=TRUE----------------------------------------------------------------
if(nrow(cohorts) > 0){
processedCohorts <- processCohorts(cohorts)

knitr::kable(
  x = data.frame(
    parentId = processedCohorts$parents,
    parentName= names(processedCohorts$parents)
    ), 
  caption = 'The parent cohorts.'
  )

knitr::kable(
  x = processedCohorts$cohortList[[1]] %>% 
    dplyr::select(-"json", -"sqlCommand"),
  caption = 'The children/subset cohorts for the first parent cohort.'
  )
}


## ----echo=TRUE----------------------------------------------------------------

subsets <- getCohortSubsetDefinitions(
    connectionHandler = ConnectionHandler,
    schema = 'main',
    cgTablePrefix = 'cg_'
)

knitr::kable(
  x = subsets, 
  caption = 'The subset cohort logic used in the analysis.'
  )


## ----echo=TRUE----------------------------------------------------------------

tte <- getTimeToEvent(
    connectionHandler = ConnectionHandler,
    schema = 'main'
)

knitr::kable(
  x = tte %>% dplyr::filter(.data$timeScale == 'per 365-day'), 
  caption = 'Example time-to-event results for the 365-day scale.'
  )


## ----echo=TRUE----------------------------------------------------------------

drc <- getDechallengeRechallenge(
    connectionHandler = ConnectionHandler,
    schema = 'main'
)

knitr::kable(
  x = drc, 
  caption = 'Example dechallenge-rechallenge results.'
  )


## ----echo=TRUE----------------------------------------------------------------

ir <- getIncidenceRates(
    connectionHandler = ConnectionHandler,
    schema = 'main', 
    targetIds = 1
)

knitr::kable(
  x = ir %>% dplyr::filter(
    subgroupName == 'All' &
    ageGroupName == 'Any' &	
    genderName == 'Any' &	
    startYear == 'Any'
  ), 
  caption = 'Example incidence rate results.'
  )


## ----echo=TRUE----------------------------------------------------------------

rf <- getBinaryRiskFactors(
    connectionHandler = ConnectionHandler,
    schema = 'main', 
    targetId = 1, 
    outcomeId = 3
)

knitr::kable(
  x = rf, 
  caption = 'Example risk factors for binary features results.'
  )


## ----echo=TRUE----------------------------------------------------------------

rf <- getContinuousRiskFactors(
    connectionHandler = ConnectionHandler,
    schema = 'main', 
    targetId = 1, 
    outcomeId = 3
)

knitr::kable(
  x = rf, 
  caption = 'Example risk factors for continuous features results.'
  )


## ----echo=TRUE----------------------------------------------------------------

modelDesigns <- getPredictionModelDesigns(
    connectionHandler = ConnectionHandler,
    schema = 'main', 
    targetIds = 1002, 
    outcomeIds = 3
)

knitr::kable(
  x = modelDesigns %>%
    dplyr::select(
      "modelDesignId",	
      "modelType",
      "developmentTargetId",
      "developmentTargetName",
      "developmentOutcomeId",	
      "developmentOutcomeName",	
      "timeAtRisk",
      "meanAuroc", 
      "noDevelopmentDatabases"
    ), 
  caption = 'Example model designs for the prediction results.'
  )


## ----echo=TRUE----------------------------------------------------------------

perform <- getPredictionPerformances(
    connectionHandler = ConnectionHandler,
    schema = 'main', 
    modelDesignId = 1
)

knitr::kable(
  x = perform, 
  caption = 'Example performance for the prediction results.'
  )


## ----echo=TRUE----------------------------------------------------------------

top5 <- getPredictionTopPredictors(
    connectionHandler = ConnectionHandler,
    schema = 'main', 
    targetIds = 1002, 
    outcomeIds = 3,
    numberPredictors = 5
)

knitr::kable(
  x = top5 , 
  caption = 'Example top five predictors.'
  )


## ----echo=TRUE----------------------------------------------------------------

attrition <- getPredictionPerformanceTable(
    connectionHandler = ConnectionHandler,
    schema = 'main', 
    table = 'attrition',
    performanceId = 1
)

knitr::kable(
  x = attrition , 
  caption = 'Example prediction attrition for performance 1.'
  )


## ----echo=TRUE----------------------------------------------------------------

cmEst <- getCMEstimation(
    connectionHandler = ConnectionHandler,
    schema = 'main', 
    targetIds = 1002, 
    comparatorIds = 2002,
    outcomeIds = 3
)

knitr::kable(
  x = cmEst , 
  caption = 'Example cohort method estimation results.'
  )


## ----echo=TRUE----------------------------------------------------------------

cmMe <- getCmMetaEstimation(
    connectionHandler = ConnectionHandler,
    schema = 'main'
)

knitr::kable(
  x = cmMe , 
  caption = 'Example cohort method meta analysis estimation results.'
  )


## ----echo=TRUE----------------------------------------------------------------

plotCmEstimates(
  cmData = cmEst, 
  cmMeta = NULL,
  targetName = 'Example Target', 
  comparatorName = 'Example Comp', 
  selectedAnalysisId = 1
)




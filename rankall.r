rankall <- function(outcome, num = "best")   {
    ## Read outcome data
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ##coerce the data into numeric values
    outcome[, 11] <- as.numeric(outcome[, 11])
    plot(outcome[,11])
}

best <- function(state, outcome) {
  ## Read outcome data
  fileData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  ##state
  if (!state %in% unique(fileData[, 7])) {
    stop("invalid state")
  }
  ##outcome
  switch(outcome, `heart attack` = {
    col = 11
  }, `heart failure` = {
    col = 17
  }, pneumonia = {
    col = 23
  }, stop("invalid outcome"))
  
  ## Return hospital name in that state with lowest 30-day death
  ##reduce to data from a specific state
  stateData <- fileData[fileData$State == state, c(2, col)]
  
  ##sort data so that if there is a tie, the best hospital will be chosen alphabetically
  stateData <- stateData[order(stateData$Hospital.Name),]
  
  ##find hospitals with the minimum outcome number
  stateData[which.min(stateData[, 2]), 1]
  
  ## rate
  
  
  
}
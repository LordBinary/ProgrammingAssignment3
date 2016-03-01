
## This function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state

best <- function(state, outcome) {
  ## Read outcome data
  fileData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  ##state
  if (!state %in% unique(fileData[, 7])) {
    stop("invalid state")
  }
  ##outcome
  switch(outcome, 
     "heart attack" = 
     {
       col = 11
     }, 
     "heart failure" = 
     {
       col = 17
     }, 
     "pneumonia" = 
     {
       col = 23
     }, 
     stop("invalid outcome"))
  
  ## Return hospital name in that state with lowest 30-day death
  ##reduce to data from a specific state and with the specified outcome
  stateData <- fileData[fileData$State == state, c(2, col)]
  
  ##sort data so that if there is a tie, the best hospital will be chosen alphabetically
  stateData <- stateData[order(stateData$Hospital.Name),]
  
  ##find hospitals with the minimum outcome number
  stateData[which.min(stateData[, 2]), 1]
  
  ## rate
}

## This function returns a character vector with the name
## of the hospital that has the ranking specified
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  fileData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid  ##state
  if (!state %in% unique(fileData[, 7])) {
    stop("invalid state")
  }
  
  ##outcome
  switch(outcome, 
     "heart attack" = 
     {
       col = 11
     }, 
     "heart failure" = 
     {
       col = 17
     }, 
     "pneumonia" = 
     {
       col = 23
     }, 
     stop("invalid outcome"))
  
  ##reduce to data from a specific state and with the specified outcome
  stateData <- fileData[fileData$State == state, c(2, col)]
  
  ##get rid of "Not Available"s
  stateData[, 2] <- as.numeric(stateData[, 2])
  stateData <- na.omit(stateData)
  
  ## figure out best/worst stuff from num param
  numberOfHospitals = nrow(stateData)
  switch(num, 
    "best" = {
      num <- 1
    }, 
    "worst" = {
      num <- numberOfHospitals
    })
  ##If num is higher than the number of hospitals, return NA
  if (num > numberOfHospitals) {
    return(NA)
  }
  
  ##order data by rate, then by name alphabetically respectively
  ord = order(stateData[, 2], stateData[, 1])
  
  ## Return hospital name in that state with the given rank
  stateData[ord, ][num, 1]
  
  ## 30-day death rate
}

## returns a 2-column data frame
## containing the hospital in each state that has the ranking specified 
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  fileData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##outcome
  switch(outcome, 
     "heart attack" = 
     {
       col = 11
     }, 
     "heart failure" = 
     {
       col = 17
     }, 
     "pneumonia" = 
     {
       col = 23
     }, 
     stop("invalid outcome"))
  
  ##reduce to data
  fileData[, col] <- as.numeric(fileData[, col])
  fileData <- fileData[, c(2, 7, col)]  
  fileData <- na.omit(fileData)
  
  ## For each state, find the hospital of the given rank
  
  ##no state parameter, so make a list of unique states
  states = unique(fileData[, 2])
  
  
      ##sub function that returns the hospitals with the lowest rate
      getLowestHospitalbyState <- function(state) {
        d = fileData[fileData[, 2] == state, ]
        ## figure out best/worst stuff from num param
        numberOfHospitals = nrow(d)
        switch(num, 
               "best" = {
                 num <- 1
               }, 
               "worst" = {
                 num <- numberOfHospitals
               })
        ord = order(d[, 3], d[, 1])
        result = d[ord, ][num, 1]
        c(result, state)
      }
  
      output = do.call(rbind, lapply(states, getLowestHospitalbyState))
      output = output[order(output[, 2]), ]
      
      ## Return a data frame with the hospital names and the
      ## (abbreviated) state name
      rownames(output) = output[, 2]
      colnames(output) = c("hospital", "state")
      data.frame(output)
      
      

}


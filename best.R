##This function get the outcome column number from the file based 
##on Hospital_Revised_Flatfiles.pdf
getOutcomeColumnNumberOfTheFile <- function (outcome){
  outcomeColumnNumber <- NULL
  if (outcome == "heart attack"){
    outcomeColumnNumber <- 11
  }else if (outcome == "heart failure"){
    outcomeColumnNumber <- 17
  }else if (outcome == "pneumonia"){
    outcomeColumnNumber <- 23
  }else{
    stop("invalid outcome")
  }
  return (outcomeColumnNumber)
}
##If valid state return the frame corresponding with the state if not throws error
getOutcomeState <- function (fileFrame, state){
  ##Column number where the 2 letters state code is located in
  ##outcome-of-care-measures.csv
  twoLettersCodeColumnNumber <- 7
  stateOutcomeFrame <- subset(fileFrame,fileFrame[,twoLettersCodeColumnNumber]==state)
  if(nrow(stateOutcomeFrame) == 0){
    stop("invalid state")
  }
  return (stateOutcomeFrame)
}
getDataAsNumericVector <- function (frame,dataColumn){
  ##NA literal "Not Available"
  notAvailable <- "Not Available"
  frameWihoutNotAvailable <- subset(frame,frame[,dataColumn]!=notAvailable)
  ##Gets only numeric values as vector
  outputVector <- as.numeric(as.vector(frameWihoutNotAvailable[,dataColumn]))
  return (outputVector) 
}
best <- function(state, outcome) {
  ##Column where hospital names are located in the outcome-of-care-measures.csv file
  hospitalNameColumnNumber <- 2
  ## Read outcome data
  fileFrame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  outcomeStateFrame <- getOutcomeState(fileFrame,state)
  outcomeColumnNumber <- getOutcomeColumnNumberOfTheFile(outcome)
  ## Return hospital name in that state with lowest 30-day death rate
  outcomeDataAsNumericVector <- getDataAsNumericVector(outcomeStateFrame,outcomeColumnNumber)
  hospitalVector <- subset(outcomeStateFrame,outcomeStateFrame[,outcomeColumnNumber] == as.character(min(outcomeDataAsNumericVector)))[,hospitalNameColumnNumber]
  return(hospitalVector[1])
}
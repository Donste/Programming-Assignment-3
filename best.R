##This function get the outcome column number from the file based 
##on Hospital_Revised_Flatfiles.pdf
getOutcomeColumnNumberOfTheFile <- function (outcome){
  outcomeColumnNumber <- NULL
  if (outcome == "heart attack"){
    outcomeColumnNumber <- 13
  }else if (outcome == "heart failure"){
    outcomeColumnNumber <- 19
  }else if (outcome == "pneumonia"){
    outcomeColumnNumber <- 25
  }else{
    stop("invalid outcome")
  }
  return (outcomeColumnNumber)
}
##If valid state return the frame corresponding with the state if not throws error
getOutcomeState <- function (fileFrame, state){
  stateOutcomeFrame <- subset(fileFrame,fileFrame[,twoLettersCodeColumnNumber]==state)
  if(nrow(stateOutcomeFrame == 0)){
    stop("invalid state")
  }
  return (stateOutcomeFrame)
}
best <- function(state, outcome) {
  ##Column number where the 2 letters state code is located in
  ##outcome-of-care-measures.csv
  twoLettersCodeColumnNumber <- 7
  ## Read outcome data
  fileFrame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  outcomeColumnNumber <- getOutcomeColumnNumberOfTheFile(outcome)
  outcomeStateFrame <- getOutcomeState(fileFrame,state)
  ## Return hospital name in that state with lowest 30-day death 
  ## rate
  outcomeStateWithoutNotAvailable <- subset(outcomeStateFrame[outcomeStateFrame[,outcomeColumnNumber]!="Not Available"]) 
  return(min(outcomeStateFrame[,outcomeColumnNumer]))
}
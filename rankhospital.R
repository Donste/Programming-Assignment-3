getOutcomeColumnNumber <- function (outcome){
  if (outcome == "heart attack") {
    columnNumber <- 11
  } else if (outcome == "heart failure") {
    columnNumber <- 17
  } else if (outcome == "pneumonia") {
    columnNumber <- 23
  } else {
    stop("invalid outcome")
  }
  return (columnNumber)
}
getStateOutcomeWithoutNotAvailable <- function (data, state, stateColumnNumber, outcomeColumnNumber){
  if (state %in% data$State){
    stateOutcome <- subset(data,data[,stateColumnNumber]==state & data[,outcomeColumnNumber]!="Not Available")
  }else{
    stop("invalid state")
  }
  return (stateOutcome)
}
getRankNumber <- function (num,data){
  if (num=="best") {
    rankNumber <- 1
  } else if (num =="worst") {
    rankNumber <- nrow(data)
  } else if (num <= nrow(data) ) {
    rankNumber <- num
  } else {
    return(NA)
  }
  return (rankNumber)
}
rankhospital <- function(state, outcome, num = "best" ){
  ##column number where state 2 character name is located in the file
  ##outcome-of-care-measures.csv
  shortStateColumnNumber <- 7
  ##column number where hospital name is located in the file
  ##outcome-of-care-measures.csv
  hospitalNameColumnNumber <- 2
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  ##column number where outcome is located into the file
  ##outcome-of-care-measures.csv
  columnNumber <- getOutcomeColumnNumber(outcome)
  stateData <<- getStateOutcomeWithoutNotAvailable (data,state,shortStateColumnNumber,columnNumber)[,c(hospitalNameColumnNumber,columnNumber)]
  ## Return hospital name in that state with the given rank ## 30-day death rate
  stateDataRank <- within(stateData,rank <- rank(as.numeric(stateData[,2]),ties.method="min"))
  stateDataRank <- stateDataRank[order(as.numeric(stateDataRank$rank),stateDataRank[,1]),]
  rankNumber <- getRankNumber(num,stateDataRank)
  if(is.na(rankNumber)){
    return (rankNumber)
  }else{
    return (stateDataRank[rankNumber,1])
  }
}

#' Sample lines in a windfarm area
#'
#' @description A function to sample n number of lines from a full set of 
#' polylines and get the proportion of those lines that intersect the windfarm.
#'
#' @param maskedlines An sf object. The full set of lines sampled within the 
#' migration corridor for a species
#' @param samplesize An integer. The number of lines to sample from the full 
#' set of lines
#' @param WFarea An sf object. The polygon of the wind farm area of interest
#' @return a numeric value representing the proportion of lines that overlap 
#' the specific wind farm area
#' @export

GetSampleProp <- function(maskedlines,samplesize,WFarea){
  testsample <- sample(length(maskedlines[[1]]),samplesize,replace=T)
  testsample <- maskedlines[[1]][testsample]
  tt <- testsample[WFarea]
  return(tt)
}

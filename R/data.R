#' Start up values for the mCRM
#'
#' A dataset of startup values for the app to run
#'
#' @format A nested list with 3 upper levels 
#' \describe{
#'   \item{turbinepars}{Parameters for the wind farm turbines}
#'   \item{windfarmpars}{Parameters for the wind farms themselves}
#'   \item{speciespars}{Parameters for species}
#'   ...
#' }
"startUpValues"


#' Default parameters for all species
#'
#' A list of all species and their parameters for loading into the app
#'
#' @format A nested list of 25 species
#' \describe{
#'   \item{speciesname}{the name of the species, under which a nested list of parameters is found}
#'   ...
#' }
"dfltSpecSizeAndSpeed"


#' Temporary template table for windfarm parameters by month
#'
#' Temporary template table for windfarm parameters by month
#'
#' @format A dataframe with 12 variables (for months)
#' \describe{
#'   \item{monthname}{Rows for wind availability, mean downtime and sd of downtime}
#'   ...
#' }
"turbineOpTempTable"



#' Start up template for bootstrap outputs
#'
#' A template for what bootstrap outputs should be formatted as
#'
#' @format A dataframe with 201 columns 
#' \describe{
#'   \item{bootId_n,...}{bootstrapped height of a bird for nth bootstrap}
#'   ...
#' }
"template_FHD"


#' Template for the bootstrapped monthly densities
#'
#' A template dataset
#'
#' @format A dataframe with 12 variables (for months)
#' \describe{
#'   \item{monthname}{Rows for wind availability, mean downtime and sd of downtime}
#'   ...
#' }
"template_monthDens_samples"

#' UK Offshore wind farms
#'
#' A spatial polygon dataframe of all the UK Offshore windfarms from EMODNet accessed July 2021
#' https://www.emodnet-humanactivities.eu/search-results.php?dataname=Wind+Farms+%28Polygons%29
#'
#' @format A spatial polygon dataframe
#' \describe{
#'   \item{COUNTRY}{Country that wind farm is associated with (should be UK only)}
#'   \item{NAME}{The name of the OSWF}
#'   \item{N_TURBINES}{Number of planned or operational turbines at a site}
#'   \item{POWER_MW}{Approximate power production of site}
#'   \item{STATUS}{Legal status of site (Planned, Approved, Production, or Dismantled)}
#'   \item{YEAR}{Year windfarm went into production}
#'   \item{COAST_DIST}{Distance to coast of polygon centroid}
#'   \item{AREA_SQKM}{Total windfarm area in kilometers squared}
#'   \item{NOTES}{Additional notes}
#'   \item{Shape_Leng}{Longest length of the polygon}
#'   \item{Shape_Area}{Area in hectares}
#' }
"EMOD_OSWFs_WGS84_UK"





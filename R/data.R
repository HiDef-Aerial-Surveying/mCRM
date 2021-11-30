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
"defaultSpeciesValues"



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

#' Scotwind lease areas
#'
#' A spatial polygon of Scotwind offshore lease areas
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
"SCOTWIND_SHAPES"


#' Scotwind lease areas merged to EMOD
#'
#' A spatial polygon of Scotwind offshore lease areas
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
"Scotwind_Merged"



#' Polygons of Migration corridors
#'
#' The polygons of the migration corridors for each species as per the BTO report. This is saved as a list and accessed
#' by way of a call to the object.
#'
#' @format A list of sf polygons for all species in the database
"all_polygons"


#' Line samples of migration corridors
#'
#' 10,000 lines randomly generated inside of the migration corridors linking non-UK regions to UK regions
#' to sample the site for possible migrating birds
#'
#' @format A list of sf polylines for all species in the database
"all_lines"


#' Species data with wingspans and lengths
#'
#' A dataframe with species names, wingspans and wing lengths
#'
#' @format A dataframe
#' \describe{
#'   \item{Voous}{Species Voous}
#'   \item{Common}{Common English name of species}
#'   \item{Scientific}{Scientific name of species}
#'   \item{Length}{Mean length of species (cm)}
#'   \item{Wingspan}{Mean wingspan of species (cm)}
#' }
"species_data"


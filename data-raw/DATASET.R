## code to prepare `DATASET` dataset goes here
##################################
### Use this script to build the rda tables in the data folder
### the usethis package converts to rdas for use in the app
library(tidyverse)

dfltSpecSizeAndSpeed <- data.table::fread("data/build_data/default species dimensions and flightspeeds.csv") %>%
  dplyr::mutate(specLabel = stringr::str_replace_all(Species, pattern = "\\s|-", replacement = "_")) %>%
  split(.$specLabel) %>%
  purrr::map(function(x){
    list(
      bodyLt_E = x$length_midpoint_m,
      bodyLt_SD = x$length_SD_m,
      bodyLt_ref = x$lenght_ref,
      wngSpan_E = x$wingspan_midpoint_m,
      wngSpan_SD = x$wingspan_SD_m,
      wngSpan_ref = x$wingspan_ref, 
      flSpeed_E = x$flightspeed_minpersec, 
      flSpeed_ref = x$flightspeed_ref
    )
  })

usethis::use_data(dfltSpecSizeAndSpeed,overwrite = T)

#### Start up values for the app

startUpValues <- list(
  turbinePars = list(
    turbPower = 6, numBlades = 3, rotorRadius = 80, airGap = 26.5, maxBladeWdth = 5.5,
    windAvail = c(96.28, 96.53, 95.83, 92.78, 90.86, 92.22, 89.11, 89.92, 93.71, 96.14, 97.14, 96.41),
    windSpeed_E = 7.74, windSpeed_SD = 3.2, rotnSpeed_E = 10, rotnSpeed_SD = 0.5, bladePitch_E = 2, bladePitch_SD = 0.1,
    rotationVsWind_df = dplyr::tibble(windSpeed = 0:29, rotationSpeed=c(rep(0,3), rep(6.8, 5), 8.1, 9.1, 9.3, 9.4, 
                                                                        9.5, rep(9.7, 2), 9.9, rep(10.2, 14))),
    pitchVsWind_df = dplyr::tibble(windSpeed = 0:29, bladePitch=c(rep(90, 3), rep(0, 8), 4, 7, 9, 11, 13, 15, 
                                                                  16, 18, 19, 20, 22, 23, 24, 25, 26, 27, 28, 29, 30)),
    meanDownTime = rep(6.3, 12), 
    sdDownTime = rep(2, 12)
  ),
  
  windFarmPars = list(
    windfarmPars_nTurbines = 100, windfarmPars_Latitude = 55.8, windfarmPars_width = 10,tidalOffset = 2.5,
    upWindOffset_perc = 50
  ),
  
  speciesPars = list(
    Black_legged_Kittiwake = list(
      meanDensity = c(0.97, 1.04, 1.15, 0.48, 0.56, 0.63, 0.68, 0.64, 0.53, 1.20, 1.02, 0.99),
      sdDensity = c(0.67, 0.75, 0.78, 0.36, 0.58, 0.45, 0.47, 0.47, 0.39, 0.78, 0.61, 0.7),
      trucDensity = c(0,2), bodyLt_E = 0.39, bodyLt_SD = 0.005, wngSpan_E = 1.08, wngSpan_SD = 0.04,
      flSpeed_E = 7.26,  flSpeed_SD = 1.5, noctAct_E = 0.033, noctAct_SD = 0.0045, CRHeight_E = 0.06,
      CRHeight_SD = 0.009, basicAvoid_E = 0.989, basicAvoid_SD = 0.001, extAvoid_E = 0.967, extAvoid_SD = 0.002,
      flType = "Flapping"
    ), 
    Arctic_Skua = dfltSpecSizeAndSpeed$Arctic_Skua, 
    Atlantic_Puffin = dfltSpecSizeAndSpeed$Atlantic_Puffin, 
    Black_throated_Diver = dfltSpecSizeAndSpeed$Black_throated_Diver,
    Common_Guillemot = dfltSpecSizeAndSpeed$Common_Guillemot,
    Common_Gull = dfltSpecSizeAndSpeed$Common_Gull,
    Common_Tern = dfltSpecSizeAndSpeed$Common_Tern,
    Cormorant = dfltSpecSizeAndSpeed$Cormorant,
    European_Shag = dfltSpecSizeAndSpeed$European_Shag,
    Great_Black_backed_Gull = dfltSpecSizeAndSpeed$Great_Black_backed_Gull,
    Great_Skua = dfltSpecSizeAndSpeed$Great_Skua,
    Herring_Gull = dfltSpecSizeAndSpeed$Herring_Gull,
    Lesser_Black_Backed_Gull = dfltSpecSizeAndSpeed$Lesser_Black_Backed_Gull,
    Little_Gull = dfltSpecSizeAndSpeed$Little_Gull,
    Manx_Shearwater = dfltSpecSizeAndSpeed$Manx_Shearwater,
    Northern_Fulmar = dfltSpecSizeAndSpeed$Northern_Fulmar,
    Northern_Gannet = dfltSpecSizeAndSpeed$Northern_Gannet,
    Razorbill = dfltSpecSizeAndSpeed$Razorbill,
    Red_throated_Diver = dfltSpecSizeAndSpeed$Red_throated_Diver
  )
)


usethis::use_data(startUpValues,overwrite = T)


# template data sets
template_FHD <- data.frame(Height_m = 1:500, matrix(0, nrow = 500, ncol = 200, dimnames = list(NULL,  paste0("bootId_", 1:200))))
usethis::use_data(template_FHD,overwrite = T)

template_monthDens_summaries <- data.frame(referencePoints = c("Minimum", "2.5th %tile", "5th %tile", "10th %tile", "25th %tile", "50th %tile", "75th %tile", 
                                                               "90th %tile", "95th %tile", "97.5th %tile", "Maximum"), 
                                           matrix(0, nrow = 11, ncol = 12, dimnames = list(NULL,  month.name)))
usethis::use_data(template_monthDens_summaries,overwrite = T)


template_monthDens_samples <- data.frame(matrix(0, nrow = 1000, ncol = 12, dimnames = list(NULL,  month.name)))
usethis::use_data(template_monthDens_samples,overwrite = T)


turbineOpTempTable <- data.frame(array(0, dim = c(3, 12), dimnames = list(c("Wind Availability (%)", "Mean Downtime (%)", "SD Downtime (%)"), month.name)),
                                 stringsAsFactors = FALSE)
usethis::use_data(turbineOpTempTable,overwrite = T)




#usethis::use_data(DATASET, overwrite = TRUE)

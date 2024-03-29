## code to prepare `DATASET` dataset goes here
##################################
### Use this script to build the rda tables in the data folder
### the usethis package converts to rdas for use in the app
library(tidyverse)

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
  )
)


usethis::use_data(startUpValues,overwrite = T,internal = T)


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

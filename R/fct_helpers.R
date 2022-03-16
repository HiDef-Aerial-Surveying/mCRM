
#' @import shinyBS
#' @export
NormNumericInput <- function(paramID,  varName, infoId="foo", infoText ="", #specID,
                             E_value=50, E_min=1, E_max=100, E_step=1, 
                             SD_value=50, SD_min=1, SD_max=100, SD_step=1,
                             via_InsertUI = FALSE,Button=TRUE,cellWidths="30%"){
  
  if(via_InsertUI==FALSE){
    
    toolTip <- shinyBS::bsTooltip(
      id = infoId,
      title = infoText,
      options = list(container = "body"),
      placement = "right", trigger = "hover")
    
  }else{
    toolTip <- NULL
  }
  div(
    splitLayout(
      cellWidths = cellWidths,
      numericInput(inputId = paste0(paramID, "_E_numInput"),
                   label = label.help(varName, infoId), 
                   min = E_min, max = E_max, step = E_step,
                   value = E_value, width = '90%'),
      
      numericInput(inputId = paste0(paramID, "_SD_numInput"),
                   label = paste0("SD of ", varName),
                   min = SD_min, max = SD_max, step = SD_step,
                   value = SD_value, width = '90%'),
      
      if(Button==TRUE){
        
        actionButton(inputId = paramID,
                     label = "View plot", width = '90%')  
      }
    ),
    toolTip
  )
}


# Get wind farm width function ---------------------------------------------

#' Get windfarm width
#'
#' Returns the width of the windfarm in kilometers for use in the flux calculations.
#'
#' @param polyg A polygon. A polygon in WGS84 for calculating the distances between vertices.
#'
#' @return
#' The maximum distance between polygon vertices
#' 
#' @importFrom geosphere distm
#' @importFrom geosphere distGeo

get_wf_width <- function(polyg){
  #Get the vertices of the polygon and apply the distGeo funcion
  vertices <- polyg@polygons[[1]]@Polygons[[1]]@coords
  dists <- geosphere::distm(vertices, fun = geosphere::distGeo)
  #Get the maximum distance between vertices, which represents the width of the windfarm
  maxDist <- round(dists[which(dists==max(dists))][1]/1000,3)
  return(maxDist)
}


# Functions for Plotting --------------------------------------------------


#' function for Truncated Normal density plot of model input parameters
#' @importFrom hrbrthemes theme_ipsum
#' @importFrom msm dtnorm
#' @importFrom msm qtnorm
#' @importFrom data.table between
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' 
#' @export
truncNormPars_densPlots <- function(mu, stdev, lower = -Inf, upper = Inf, fill="olivedrab", xlab){
  req(mu, stdev)
  yaxixUpLimBands <- data.frame(start = c(0.0, 0.25, 0.5, 1, 2, 4, 6), end = c(0.25, 0.5, 1, 2, 4, 6, 10))
  if(stdev == 0){
    if(mu <= lower){
      NULL
    }else{
      data.frame(x = seq(mu-500, mu+500, by = 1), y = 0) %>% #%>% mutate(y = if_else(x == mu, 0.5, 0)) %>%
        ggplot(aes(x=x, y=y)) +
        geom_path() +
        geom_segment(aes(x = mu, xend = mu, y = 0, yend = Inf), size = 1) +
        coord_cartesian(xlim = c(0, mu*1.65), ylim = c(0, 0.5)) + hrbrthemes::theme_ipsum(axis_title_size = 14)+
        scale_y_continuous(expand=c(0,0)) +
        labs(y="Density", x = xlab)  
    }
  }else{
    if(stdev>0){
      distTails <- msm::qtnorm(c(0.00001, 0.999999), mean = mu, sd = stdev, lower = lower, upper = upper)
      xaxisUpLim <- distTails[2]*1.1
      xaxisLowLim <- ifelse(distTails[1]*0.5 > mu/2.5, distTails[1]*0.7, 0)
      yaxisMax <- max(msm::dtnorm(seq(mu-500, mu+500, by = 1), mean = mu, sd = stdev, lower = lower, upper = upper))
      yaxisUpLim <- yaxixUpLimBands %>%
        dplyr::mutate(yaxisMax = yaxisMax) %>%
        dplyr::filter(data.table::between(yaxisMax, start, end)) %>%
        dplyr::select(end)
      if(nrow(yaxisUpLim) == 0){
        yaxisUpLim <- data.frame(end = yaxisMax)
      }
      data.frame(qtls = msm::qtnorm(c(0.0001, 0.9999), mean = mu, sd=stdev, lower = lower, upper = upper))  %>%
        ggplot(aes(qtls)) +
        stat_function(fun=msm::dtnorm, args = list(mean = mu, sd = stdev, lower = lower, upper = upper), col = "black", size = 1) +
        stat_function(fun=msm::dtnorm, args = list(mean = mu, sd = stdev, lower = lower,  upper = upper), geom="area", 
                      fill = fill, col = "black", alpha = 0.3)+ hrbrthemes::theme_ipsum(axis_title_size = 14)+
        scale_y_continuous(expand=c(0,0)) +
        labs(y="Density", x = xlab) +
        coord_cartesian(xlim = c(xaxisLowLim, max(0, xaxisUpLim, na.rm=TRUE)), ylim = c(0, yaxisUpLim$end))
    }else{
      NULL
    }
  }
}


#' Sum the standard deviations
#' @export
sum.stdevs <- function(x){
  return(sqrt(sum(sapply(x,function(x) x^2),na.rm=T)))
}



# Customised TRUE-FALSE switch button for Rshiny
# Only sing CSS3 code (No javascript)
#
# Sébastien Rochette
# http://statnmap.com/en/
# April 2016
#
# CSS3 code was found on https://proto.io/freebies/onoff/
# For CSS3 customisation, refer to this website.

#' A function to change the Original checkbox of rshiny
#' into a nice true/false or on/off switch button
#' No javascript involved. Only CSS code.
#' 
#' To be used with CSS script 'button.css' stored in a 'www' folder in your Shiny app folder
#' 
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value (TRUE or FALSE).
#' @param col Color set of the switch button. Choose between "GB" (Grey-Blue) and "RG" (Red-Green)
#' @param type Text type of the button. Choose between "TF" (TRUE - FALSE), "OO" (ON - OFF) or leave empty for no text.

switchButton <- function(inputId, label, value=FALSE, col = "GB", type="TF") {
  
  # color class
  if (col != "RG" & col != "GB") {
    stop("Please choose a color between \"RG\" (Red-Green) 
      and \"GB\" (Grey-Blue).")
  }
  if (!type %in% c("OO", "TF", "YN")){
    warning("No known text type (\"OO\", \"TF\" or \"YN\") have been specified, 
     button will be empty of text") 
  }
  if(col == "RG"){colclass <- "RedGreen"}
  if(col == "GB"){colclass <- "GreyBlue"}
  if(type == "OO"){colclass <- paste(colclass,"OnOff")}
  if(type == "TF"){colclass <- paste(colclass,"TrueFalse")}
  if(type == "YN"){colclass <- paste(colclass,"YesNo")}
  
  # No javascript button - total CSS3
  # As there is no javascript, the "checked" value implies to
  # duplicate code for giving the possibility to choose default value
  
  if(value){
    tagList(
      tags$div(class = "form-group shiny-input-container",
               tags$div(class = colclass,
                        tags$label(label, class = "control-label"),
                        tags$div(class = "onoffswitch",
                                 tags$input(type = "checkbox", name = "onoffswitch", class = "onoffswitch-checkbox",
                                            id = inputId, checked = ""
                                 ),
                                 tags$label(class = "onoffswitch-label", `for` = inputId,
                                            tags$span(class = "onoffswitch-inner"),
                                            tags$span(class = "onoffswitch-switch")
                                 )
                        )
               )
      )
    )
  } else {
    tagList(
      tags$div(class = "form-group shiny-input-container",
               tags$div(class = colclass,
                        tags$label(label, class = "control-label"),
                        tags$div(class = "onoffswitch",
                                 tags$input(type = "checkbox", name = "onoffswitch", class = "onoffswitch-checkbox",
                                            id = inputId
                                 ),
                                 tags$label(class = "onoffswitch-label", `for` = inputId,
                                            tags$span(class = "onoffswitch-inner"),
                                            tags$span(class = "onoffswitch-switch")
                                 )
                        )
               )
      )
    ) 
  }
}






#' @import shinyBS
#' @export
NormNumericInput <- function(paramID,  varName, infoId="foo", infoText ="", #specID,
                             E_value=50, E_min=1, E_max=100, E_step=1, 
                             SD_value=50, SD_min=1, SD_max=100, SD_step=1,
                             via_InsertUI = FALSE,Button=TRUE){
  
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
      cellWidths = "30%",
      numericInput(inputId = paste0("numInput_", paramID, "_E_"),
                   label = label.help(varName, infoId), 
                   min = E_min, max = E_max, step = E_step,
                   value = E_value, width = '90%'),
      
      numericInput(inputId = paste0("numInput_",  paramID, "_SD_"),
                   label = paste0("SD of ", varName),
                   min = SD_min, max = SD_max, step = SD_step,
                   value = SD_value, width = '90%'),
      if(Button==TRUE){
        actionButton(inputId = paste0("actButt_",  paramID, "_SD_"),
                     label = "View plot", width = '90%')  
      }
    ),
    toolTip
  )
}



#' @export
divUI_monthDens_PctlsAndSample_DownButtons <- function(introText, fileInputId, fileInputPopUpText, downButtOutputId){
  fluidRow(
    column(12, helpText(introText)),
    br(),
    column(3,
           tipify(      
             fileInput(inputId = fileInputId,
                       label = "Upload file",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
             title = fileInputPopUpText,
             placement = "left", trigger = "hover", options = list(container = "body")
           )
    ),
    column(1,
           style = "margin-top: 25px; margin-left: -10px",
           downloadButton(outputId = downButtOutputId, label = NULL, class = "butt"),
           tags$head(tags$style(".butt{background-color:#4570a5;
                                    color: #efecec}
                                    .butt:hover{
                                    background-color:#4570a5;
                                    color: #efecec}"
           ))
    ),
    
    shinyBS::bsTooltip(id = downButtOutputId,
              title = "Template dataset. Fill in, save locally and upload on the adjacent field",
              options = list(container = "body"),
              placement = "right", trigger = "hover")
  )
}



# Functions for Plotting --------------------------------------------------



#' function for Normal density plot of model input parameters
#' @export
normDens_ParsPlots <- function(mu, stdev, fill="olivedrab", xlab, refValue_E = mu, refValue_SD = stdev){
  
  req(mu, stdev)
  data.frame(qtls = qnorm(c(0.0001, 0.9999), mean = mu, sd=stdev))  %>%
    ggplot(aes(qtls)) +
    stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), col = "black", size =1) +
    stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), geom="area", fill = fill, col = "black", alpha = 0.3) +
    labs(y="Density", x = xlab) + 
    coord_cartesian(xlim = qnorm(c(0.000001, 0.999999), mean = refValue_E, sd = refValue_SD))
}


#' function for Normal density quantiles of model input parameters
#' @export
normDens_ParsQtls <- function(mu, stdev, varTag, decPlaces = 3){
  
  req(mu, stdev)
  data.frame(pctile = paste0(varTag, "|"), t(round(qnorm(p = c(0.025, 0.25, 0.5, 0.75, 0.975), mean = mu, sd = stdev), decPlaces))) %>%
    dplyr::rename(`%tile|` = pctile, `2.5th` = X1, `25th` = X2, `50th` = X3, `75th` = X4, `97.5th` = X5) %>%
    dplyr::mutate_at(.vars = vars(`2.5th`:`97.5th`), list(~sprintf(fmt = paste0("%.", decPlaces, "f"), .)))%>%
    dplyr::mutate(dummy = "") %>%
    column_to_rownames("dummy")
}





#' function for Truncated Normal density plot of model input parameters
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
        coord_cartesian(xlim = c(0, mu*1.65), ylim = c(0, 0.5)) +
        labs(y="Density", x = xlab)  
    }
  }else{
    if(stdev>0){
      
      distTails <- qtnorm(c(0.00001, 0.999999), mean = mu, sd = stdev, lower = lower, upper = upper)
      xaxisUpLim <- distTails[2]*1.1
      xaxisLowLim <- ifelse(distTails[1]*0.5 > mu/2.5, distTails[1]*0.7, 0)
      
      yaxisMax <- max(dtnorm(seq(mu-500, mu+500, by = 1), mean = mu, sd = stdev, lower = lower, upper = upper))
      yaxisUpLim <- yaxixUpLimBands %>%
        dplyr::mutate(yaxisMax = yaxisMax) %>%
        dplyr::filter(data.table::between(yaxisMax, start, end)) %>%
        dplyr::select(end)
      if(nrow(yaxisUpLim) == 0){
        yaxisUpLim <- data.frame(end = yaxisMax)
      }
      
      data.frame(qtls = qtnorm(c(0.0001, 0.9999), mean = mu, sd=stdev, lower = lower, upper = upper))  %>%
        ggplot(aes(qtls)) +
        stat_function(fun=dtnorm, args = list(mean = mu, sd = stdev, lower = lower, upper = upper), col = "black", size = 1) +
        stat_function(fun=dtnorm, args = list(mean = mu, sd = stdev, lower = lower,  upper = upper), geom="area", 
                      fill = fill, col = "black", alpha = 0.3)+
        labs(y="Density", x = xlab) +
        coord_cartesian(xlim = c(xaxisLowLim, max(0, xaxisUpLim, na.rm=TRUE)), ylim = c(0, yaxisUpLim$end))
    }else{
      NULL
    }
  }
}



#' function for Beta density plot of model input parameters
#' @export
betaInputPars_densPlots <- function(p, stdev, fill="olivedrab", xlab){
  
  req(p, stdev)
  
  yaxixUpLimBands <- data.frame(start = c(0.0, 0.5, 1, 2, 4, 6, 10), end = c(0.5, 1, 2, 4, 6, 10, 20))
  
  betaMeanVarCond <- stdev^2 < p*(1-p)
  
  if(p >= 0 & p <= 1){
    if(stdev == 0){
      
      data.frame(x = seq(0, 1, length.out = 100), y = 0) %>%
        ggplot(aes(x=x, y=y)) +
        geom_path() +
        geom_segment(aes(x = p, xend = p, y = 0, yend = Inf), size = 1) +
        coord_cartesian(xlim = c(0,1), ylim = c(0, 0.5)) +
        labs(y="Density", x = xlab)  
    }else{
      if(stdev > 0){
        if(betaMeanVarCond){
          
          eta <- p*(1-p)/stdev^2 - 1
          alpha <- eta*p
          beta <- eta*(1-p)
          
          distTails <- qbeta(c(0.00001, 0.999999), shape1 = alpha, shape2 = beta)
          xaxisUpLim <- ifelse(distTails[2] > 0.6, 1, ifelse(distTails[2] > 0.3, 0.6, 0.3))
          xaxisLowLim <- ifelse(distTails[1] < 0.6, 0, ifelse(distTails[1] < 0.85, 0.5, 0.86))
          
          yaxisMax <- max(dbeta(seq(0, 1, by = 0.001), shape1 = alpha, shape2 = beta))
          
          yaxisUpLim <- yaxixUpLimBands %>%
            dplyr::mutate(yaxisMax = yaxisMax) %>%
            dplyr::filter(data.table::between(yaxisMax, start, end)) %>%
            dplyr::select(end)
          if(nrow(yaxisUpLim) == 0){
            yaxisUpLim <- data.frame(end = yaxisMax)
          }
          
          p1 <- data.frame(qtls = qbeta(c(0.0001, 0.9999), shape1 = alpha, shape2 = beta))  %>%
            ggplot(aes(qtls)) +
            stat_function(fun=dbeta, args = list(shape1 = alpha, shape2 = beta), col = "black", size = 1) +
            stat_function(fun=dbeta, args = list(shape1 = alpha, shape2 = beta), geom="area", 
                          fill = fill, col = "black", alpha = 0.3) +
            labs(y="Density", x = xlab)
          
          if(is.infinite(yaxisUpLim$end)){
            p1
          }else{
            p1 + coord_cartesian(xlim = c(xaxisLowLim, xaxisUpLim), ylim = c(0, yaxisUpLim$end))
          }
        }else{
          NULL
        }
      }else{
        NULL
      }
    }
  }else{
    NULL
  }
}



# Functions for outputing quantiles   -------------------------------------

# function for Truncated Normal density quantiles of model input parameters
truncNormPars_qtlTbl <- function(mu, stdev, lower = -Inf, upper = Inf, varTag, decPlaces = 3){
  
  req(mu, stdev)
  
  if(stdev == 0 & mu <= lower){
    #NULL
    invisible()
  }else{
    if(stdev >= 0){
      data.frame(pctile = paste0(varTag, "|"), t(round(qtnorm(p = c(0.025, 0.25, 0.5, 0.75, 0.975), mean = mu, sd = stdev, lower = lower, 
                                                              upper = upper), decPlaces))) %>%
        dplyr::rename(`%tile|` = pctile, `2.5th` = X1, `25th` = X2, `50th` = X3, `75th` = X4, `97.5th` = X5) %>%
        mutate_at(.vars = vars(`2.5th`:`97.5th`), list(~sprintf(fmt = paste0("%.", decPlaces, "f"), .))) %>%
        mutate(dummy = "") %>%
        column_to_rownames("dummy")   
    }else{
      invisible()
    }
  }
  
}



# function for beta quantiles of model input parameters
betaInputPars_qtlTbl <- function(p, stdev, varTag, decPlaces = 3){
  
  req(p, stdev)
  
  eta <- p*(1-p)/stdev^2 - 1
  alpha <- eta*p
  beta <- eta*(1-p)
  
  betaMeanVarCond <- stdev^2 < p*(1-p)
  
  if(p >= 0 & p <= 1){
    if(stdev == 0){   # if sd == 0, fixe sampled values to chosen p/mu (default qbeta generates fixed 0.5, as alpha = beta = Inf => p = 0.5)
      data.frame(pctile = paste0(varTag, "|"), X1 = p, X2 = p, X3 = p, X4 = p, X5 = p) %>%
        dplyr::rename(`%tile|` = pctile, `2.5th` = X1, `25th` = X2, `50th` = X3, `75th` = X4, `97.5th` = X5) %>%
        dplyr::mutate_at(.vars = vars(`2.5th`:`97.5th`), list(~sprintf(fmt = paste0("%.", decPlaces, "f"), .))) %>%
        dplyr::mutate(dummy = "") %>%
        column_to_rownames("dummy")
    }else{
      if(stdev > 0){
        if(betaMeanVarCond){
          data.frame(pctile = paste0(varTag, "|"), t(round(qbeta(p = c(0.025, 0.25, 0.5, 0.75, 0.975), shape1 = alpha, shape2 = beta), decPlaces))) %>%
            dplyr::rename(`%tile|` = pctile, `2.5th` = X1, `25th` = X2, `50th` = X3, `75th` = X4, `97.5th` = X5) %>%
            dplyr::mutate_at(.vars = vars(`2.5th`:`97.5th`), list(~sprintf(fmt = paste0("%.", decPlaces, "f"), .))) %>%
            dplyr::mutate(dummy = "") %>%
            column_to_rownames("dummy")
        }else{
          invisible()
        }
      }else{
        invisible()
      }
    }
  }else{
    invisible()
  }
}



# Functions for Alerts on Parameter Validation  ---------------------------

# General function for alerts for parameters with truncated normal
tnormParamsAlert <- function(expVal, stdv, varName, varTag, session){
  
  c_alertId_E <- paste0("alertInput_", varTag, "_E")
  c_alertId_SD <- paste0("alertInput_", varTag, "_SD")
  
  if(!is.na(expVal) & !is.na(stdv)){
    if(stdv == 0 & expVal <= 0){
      createAlert(session, anchorId = "alert", alertId = c_alertId_E, title = "Oops...",
                  content = paste0("<b>", varName, ":</b> needs Mean > 0<br/> when SD = 0"), append = TRUE, style = "danger")
    }else{
      closeAlert(session, alertId = c_alertId_E)
    }
  }
  
  if(!is.na(stdv)){
    if(stdv < 0){
      createAlert(session, anchorId = "alert", alertId = c_alertId_SD, title = "Oops...",
                  content = paste0("<b>", varName, ":</b> needs SD >= 0"), append = TRUE, style = "danger")
    }else{
      closeAlert(session, alertId = c_alertId_SD)
    }
  }
}



# General function for alerts for validation of Beta distributed parameters
betaParamsAlert <- function(p, stdv, varName, varTag, session){
  
  c_alertId_E <- paste0("alertInput_", varTag, "_E")
  c_alertId_SD <- paste0("alertInput_", varTag, "_SD")
  
  #if(p == 0.2 & stdv == 0.2){ browser() }  
  
  betaMeanVarCond <- stdv^2 < p*(1-p)
  
  # input validation for p
  if(!is.na(p)){
    if(p < 0 | p > 1){
      createAlert(session, anchorId = "alert", alertId = c_alertId_E, title = "Oops...",
                  content = paste0("<b>", varName, ":</b> needs Mean <br/> between 0 and 1"), append = TRUE, style = "danger")
    }else{
      closeAlert(session, alertId = c_alertId_E)
    }
  }
  
  # input validation for SD 
  if(!is.na(stdv) & !is.na(p)){
    if(p >= 0 & p <= 1){
      if(stdv > 0){
        if(p == 0 | p == 1){
          createAlert(session, anchorId = "alert", alertId = c_alertId_SD, title = "Oops...",
                      content = paste0("<b>", varName, ":</b> <br/> needs SD = 0 when Mean is 0 or 1"), append = TRUE, style = "danger")
        }else{
          closeAlert(session, alertId = c_alertId_SD)
        }
        
        if(!betaMeanVarCond){
          condLimit <- round(sqrt(p*(1-p)), digits = 3)
          createAlert(session, anchorId = "alert", alertId = c_alertId_SD, title = "Oops...",
                      content = paste0("<b>", varName, ":</b> <br/> needs SD < ", condLimit, " for the <br/> Mean p = ", round(p, digits = 3)), append = TRUE, style = "danger")
        }else{
          closeAlert(session, alertId = c_alertId_SD)
        }
      }else{
        if(stdv < 0){
          createAlert(session, anchorId = "alert", alertId = c_alertId_SD, title = "Oops...",
                      content = paste0("<b>", varName, ":</b> needs SD >= 0"), append = TRUE, style = "danger")
        }else{
          closeAlert(session, alertId = c_alertId_SD)
        }
      }
    }
  }
  
}



# Function for sampling lines in a WF area.  ------------------------------

GetSampleProp <- function(maskedlines,samplesize,WFarea){
  testsample <- sample(length(maskedlines[[1]]),samplesize,replace=T)
  testsample <- maskedlines[[1]][testsample]
  tt <- testsample[WFarea,]
  return(tt)
}


# Functions for customized output from distribution functions  ------------

# generate random samples from a beta distribution, parameterized as mean and sd, and returning NAs if conditions are not met
rbeta_dmp <- function(n, p, sd){
  
  eta <- p*(1-p)/sd^2 - 1
  alpha <- eta*p
  beta <- eta*(1-p)
  
  betaMeanVarCond <- sd^2 < p*(1-p)
  
  if(is.na(p) | is.na(sd)){
    
    out <- rep(NA, n)
    warning("NA values for p and/or stdev - NAs produced")
    
  }else{
    
    if(p >= 0 & p <= 1){
      
      if(sd < 0){
        out <- rep(NA, n)
        warning("stdev < 0 - NAs produced")
      }
      
      if(sd == 0){
        out <- rep(p, n)
      }
      
      if(sd > 0){
        if(betaMeanVarCond){
          out <- rbeta(n, shape1 = alpha, shape2 = beta)
        }else{
          out <- rep(NA, n)
          warning("condition var < p*(1 - p) not met - NAs produced")
        }
      }
    }else{
      out <- rep(NA, n)
      warning("p < 0 | p > 1 - NAs produced")
    }
  }
  return(out)
}



# generate random samples from a truncated normal, returning NAs if conditions are not met
rtnorm_dmp <- function(n, mean=0, sd=1, lower=-Inf, upper=Inf){
  
  if(is.na(mean)|is.na(sd)){
    out <- rep(NA, n)
    warning("NA values for mu and/or stdev - NAs produced")
  }else{
    if(sd >= 0){
      if(sd == 0 & mean == lower){
        out <- rep(lower, n)
      }
      if(sd == 0 & mean < lower){
        out <- rep(NA, n)
        warning("mu < lower & SD = 0 - NAs produced")
      }
      if(sd == 0 & mean > lower){
        #out <- qtnorm(p, mean = mean, sd = sd, lower = lower, upper = upper)
        out <- rep(mean, n)
      }
      if(sd > 0){
        out <- rtnorm(n, mean = mean, sd = sd, lower = lower, upper = upper)
      }
    }else{
      warning("SD < 0 - NAs produced")
      out <- rep(NA, n)
    }
  }
  
  return(out)
}



qtnorm_dmp <- function(p, mean=0, sd=1, lower=-Inf, upper=Inf){
  
  if(is.na(mean)|is.na(sd)){
    out <- rep(NA, length(p))
    warning("NA values for mu and/or stdev - NAs produced")
  }else{
    if(sd >= 0){
      if(sd == 0 & mean == lower){
        out <- rep(lower, length(p))
      }
      if(sd == 0 & mean < lower){
        out <- rep(NA, length(p))
        warning("mu < lower & SD = 0 - NAs produced")
      }
      if(sd == 0 & mean > lower){
        out <- rep(mean, length(p))
      }
      if(sd > 0){
        out <- qtnorm(p, mean = mean, sd = sd, lower = lower, upper = upper)
      }
    }else{
      warning("SD < 0 - NAs produced")
      out <- rep(NA, length(p))
    }
  }
  return(out)
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





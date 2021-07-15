#' WindFarmFeats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import shinyWidgets
#' @import shinyBS
#'  
mod_WindFarmFeats_ui <- function(id){
  idv <- stringr::str_replace_all(id," ", "_")
  ns <- NS(idv)
  tagList(
    # --- Turbine features box
    box(title = paste(id,"Features"), 
        width = 2, 
        status = "primary", 
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        selectInput(ns("selectInput_builtin_turbine"),
                    label = "Select turbine type",
                    width = "85%",
                    choices = c("Turbine 1" = "tb1", "Turbine 2" = "tb2", "Turbine 3" = "tb3"),
                    selectize = TRUE
        ),
        
        # --- Turbine Power
        numericInput(width = "85%", 
                     inputId = ns("numInput_windfarmPars_nTurbines"), 
                     label = label.help("Number of Turbines", ns("lbl_windfarmnTurbines")),
                     value = startUpValues$windFarmPars$windfarmPars_nTurbines, min = 1, step = 1),
        shinyBS::bsTooltip(id = ns("lbl_windfarmnTurbines"), 
                           title = paste0("Total number of turbines in the wind farm array.", 
                                          " Used in conjunction with turbine model to calculate the target power of the wind farm."),
                           options = list(container = "body"), placement = "right", trigger = "hover"),
        
        
        # ---  Latitude
        numericInput(width = "85%", 
                     inputId = ns("numInput_windfarmPars_Latitude"), 
                     label = label.help("Latitude (deg)", ns("lbl_windfarmLatitude")), #"Latitude (deg)", 
                     value = startUpValues$windFarmPars$windfarmPars_Latitude, min = -90, max = 90, step = 0.01),
        shinyBS::bsTooltip(id = ns("lbl_windfarmLatitude"), 
                           title = paste0("Latitude of the wind farm in decimal degrees. Used to calculate day length at the site over the year."),
                           options = list(container = "body"), placement = "right", trigger = "hover"),
        
        # --- Width
        numericInput(width = "85%", 
                     inputId = ns("numInput_windfarmPars_width"), 
                     label = label.help("Width (Km)", ns("lbl_windfarmWidth")), #"Width (Km)", 
                     value = startUpValues$windFarmPars$windfarmPars_width, min = 1, step = 1), 
        shinyBS::bsTooltip(id = ns("lbl_windfarmWidth"), 
                           title = paste0("The wind farm width"),
                           options = list(container = "body"), placement = "right", trigger = "hover"),
        
        # --- Tidal offset
        numericInput(width = "85%", 
                     inputId = ns("numInput_windfarmPars_tidalOffset"), 
                     label = label.help("Tidal Offset (m)", ns("lbl_tidalOffset")), #"Tidal Offset (m)", 
                     value = startUpValues$windFarmPars$tidalOffset, min = 0, step = 0.1),
        shinyBS::bsTooltip(id = ns("lbl_tidalOffset"), 
                           title = paste0("Tidal offset to correct for: (i) flight heights calculated in relation to mean sea-level",
                                          " ; and (ii) turbine dimensions calculated in relation to Highest Astronomical Tide"),
                           options = list(container = "body"), placement = "right", trigger = "hover"),
        
        
        br(),
        
        shinyWidgets::knobInput(
          inputId = ns("sldInput_windfarmPars_upWindDownWindProp"),
          label = label.help("Upwind flights (%)", ns("lbl_upWindDownWindProp")),
          value = 50, min = 0, max = 100, step = 1,
          displayPrevious = TRUE,
          thickness = 0.4,
          lineCap = "default", #"round",
          inputColor = "#333",
          fgColor = "#6B8E23",
          angleArc = 180,
          angleOffset = 270, 
          width = "80%"
          # height = "140px"
        ),
        
        shinyBS::bsTooltip(id = ns("lbl_upWindDownWindProp"), 
                           title = paste0("The percentage of upwind bird flights. Should be 50% unless direction of travel", 
                                          " is biased in a particular direction"),
                           options = list(container = "body"), placement = "right", trigger = "hover"),
        
        ),
    
  
    box(title = paste(id, "Turbine Parameters"), 
        width = 10, 
        status = "primary", 
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        fluidRow(
          box(width = 12,
              splitLayout(
                
                # --- Turbine power/model
                numericInput(width = "60%",
                             inputId = ns("numInput_turbinePars_turbinePower"),
                             label = label.help("Turbine Model (MW)", ns("lbl_turbinePower")),
                             value = startUpValues$turbinePars$turbPower, min = 1),
                
                # ---- Number of blades
                numericInput(width = "60%",
                             inputId = ns("numInput_turbinePars_numBlades"),
                             label =  label.help("No. of blades", ns("lbl_numBlades")),
                             value = startUpValues$turbinePars$numBlades, min = 1),
                
                # ---- Rotor radius
                numericInput(width = "60%",
                             inputId = ns("numInput_turbinePars_rotRadius"),
                             label =  label.help("Rotor Radius (m)", ns("lbl_rotorRadius")),
                             value = startUpValues$turbinePars$rotorRadius, min = 1, step = 0.1),
                
                # ---- Air Gap
                numericInput(width = "60%", 
                             inputId = ns("numInput_turbinePars_airGap"),
                             label =  label.help("Air Gap (m)", ns("lbl_turbineAirGap")),
                             value = startUpValues$turbinePars$airGap, min = 1, step = 0.5),
                
                # Maximum blade width
                numericInput(width = "60%", 
                             inputId = ns("numInput_turbinePars_maxBladeWdth"),
                             label =  label.help("Max blade width (m)", ns("lbl_maxBladeWdth")),
                             value = startUpValues$turbinePars$maxBladeWdth, min = 1, step = 0.1)
                
              )
          ),
          shinyBS::bsTooltip(id = ns("lbl_turbinePower"),
                             title = paste0("The power output of each turbine"),
                             options = list(container = "body"), placement = "right", trigger = "hover"),
          shinyBS::bsTooltip(id = ns("lbl_numBlades"),
                             title = paste0("Number of blades in each turbine"),
                             options = list(container = "body"), placement = "right", trigger = "hover"),
          shinyBS::bsTooltip(id = ns("lbl_rotorRadius"),
                             title = paste0("The distance from the axis of rotation to blade tip"),
                             options = list(container = "body"), placement = "right", trigger = "hover"),
          shinyBS::bsTooltip(id = ns("lbl_turbineAirGap"),
                             title = paste0("Tip clearance height, i.e. the distance between the lowest rotor tip height", 
                                            " and sealevel (measured as Highest Astronomical Tide)"),
                             options = list(container = "body"), placement = "right", trigger = "hover"),
          shinyBS::bsTooltip(id = ns("lbl_maxBladeWdth"),
                             title = paste0("The maximum width of the rotor blade"),
                             options = list(container = "body"), placement = "right", trigger = "hover")
        ),
        
        br(),
        fluidRow(
          box(width = 12, 
              
              tags$b(HTML(paste0("Monthly Operation", actionLink(ns("lbl_monthOPs"), label=NULL, 
                                                                 icon=icon('info-circle'))))),
              shinyBS::bsTooltip(id = ns("lbl_monthOPs"), 
                                 title = paste0("Information on turbine activity per month: % of wind availability (treated as constant) and % of", 
                                                " maintenance downtime (~ Normal)"),
                                 options = list(container = "body"), placement = "right", trigger = "hover"),
              
              br(),
              br(),
              
              rHandsontableOutput(ns("hotInput_turbinePars_monthOps"), width = "100%"),
              tags$style(type="text/css", "#hotInput_turbinePars_monthOps th {font-weight:bold;}"),
              br(),
              br(),
              column(6,
                     plotOutput(ns("plot_turbinePars_monthOps_windAvb"), width = "100%", height = 250)
              ),
              column(6,
                     plotOutput(ns("plot_turbinePars_monthOps_downtime"), width = "100%", height = 250)
              )
          )
        ),
        br(),
        fluidRow(
          box(width = 12,
              tags$b(HTML(paste0("Rotation Speed and Blade Pitch"))),
              br(),
              br(),
              helpText("Choose between simulating rotor speed and pitch from probability distributions OR from a relationship with wind speed"),
              fluidRow(
                column(4, 
                       radioGroupButtons(inputId = ns("radGrpInput_turbinePars_rotationAndPitchOption"),
                                         individual = TRUE,
                                         justified = TRUE, 
                                         label = NULL,
                                         choices = c("Probability distributions" = ns("probDist"),
                                                     "Wind Speed relationship" = ns("windSpeedReltn")),
                                         checkIcon = list(yes = tags$i(class = "fa fa-circle",
                                                                       style = "color: steelblue"),
                                                          no = tags$i(class = "fa fa-circle-o",
                                                                      style = "color: steelblue")))
                )),
              br(),
              br(),
              conditionalPanel(
                condition = "input.radGrpInput_turbinePars_rotationAndPitchOption == 'probDist'",
                fluidRow(
                  column(4,
                         box(width = 12,
                             NormNumericInput(paramID = ns("turbinePars_rotnSpeed"), specID = "",
                                              varName = "Rotation (rpm)",
                                              infoId = ns("lbl_rotSpeedProbDist"),
                                              infoText = paste0("Turbine rotor speed (~ Truncated Normal with lower bound at 0). ", 
                                                                "SD should be 0 unless suitable info on rotor speed variability is available"),
                                              E_value = startUpValues$turbinePars$rotnSpeed_E, SD_value = startUpValues$turbinePars$rotnSpeed_SD),
                             plotOutput(ns("plot_turbinePars_rotnSpeed"), width = 310, height = 200),
                             br(),
                             verbatimTextOutput(ns("qtls_turbinePars_rotnSpeed"))
                         )
                  ),
                  column(4,
                         box(width = 12,
                             NormNumericInput(paramID = ns("turbinePars_bladePitch"), specID = "",
                                              varName = "Pitch (deg)",
                                              infoId = ns("lbl_bladePitchProbDist"),
                                              infoText = paste0("Blade pitch, i.e. the angle of the blade relative to rotor plane (~Truncated Normal with lower bound at 0). ", 
                                                                "SD should be 0 unless suitable info on pitch variability is available"),
                                              E_value = startUpValues$turbinePars$bladePitch_E, SD_value = startUpValues$turbinePars$bladePitch_SD),
                             plotOutput(ns("plot_turbinePars_bladePitch"), width = 310, height = 200),
                             br(),
                             verbatimTextOutput(ns("qtls_turbinePars_bladePitch"))
                         )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.radGrpInput_turbinePars_rotationAndPitchOption == 'windSpeedReltn'",
                fluidRow(
                  column(4, 
                         rHandsontableOutput(ns("hotInput_turbinePars_rotationVsWind"), width = "100%"),
                         tags$style(type="text/css", "#hotInput_turbinePars_rotationVsWind th {font-weight:bold;}")
                  ),
                  column(4, 
                         rHandsontableOutput(ns("hotInput_turbinePars_pitchVsWind"), width = "100%"),
                         tags$style(type="text/css", "#hotInput_turbinePars_pitchVsWind th {font-weight:bold;}")
                  ),
                  column(4,
                         NormNumericInput(paramID = ns("miscPars_windSpeed"), specID = "",
                                          varName = "Wind Speed (m/s)",
                                          infoId = ns("lbl_winSpeed"),
                                          infoText = paste0("Wind speed (~Truncated Normal with lower bound at 0)"),
                                          E_value = startUpValues$turbinePars$windSpeed_E, SD_value = startUpValues$turbinePars$windSpeed_SD),
                         plotOutput(ns("plot_miscPars_windSpeed"), width = 310, height = 200),
                         verbatimTextOutput(ns("qtls_miscPars_windSpeed"))
                  )
                )
              )
          )
        )
    )
  )
}
    
#' WindFarmFeats Server Function
#'
#' @param id module id
#' @param data reactive dataset
#' 
#' @import shiny
#' @import ggplot2
#' @noRd 
#' 

mod_WindFarmFeats_server <- function(id, data){
  id <- stringr::str_replace_all(id," ", "_")
  moduleServer(
    id,
    function(input,output,session){
      
      # --- Create input table for turbine monthly operation parameters
      output$hotInput_turbinePars_monthOps <- renderRHandsontable({
        
        data$turbinePars_monthOps_df %>%
          rhandsontable(rowHeaderWidth = 140) %>%
          hot_cols(colWidths = 85, 
                   renderer = "function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.NumericRenderer.apply(this, arguments);
               if (value == null || value.length === 0) {
               td.style.background = 'pink';
               }}") %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      
      # -- Turbine Monthly Operations - Downtime
      output$plot_turbinePars_monthOps_downtime <- renderPlot({
        
        req(input$hotInput_turbinePars_monthOps)
        
        #browser()
        
        hot_to_r(input$hotInput_turbinePars_monthOps) %>% 
          rownames_to_column(var="Variable") %>% 
          slice(2:3) %>%
          gather(month, windAvb, -Variable) %>%
          mutate(month = factor(month, levels=unique(month), labels = month.abb)) %>%
          spread(Variable, windAvb) %>%
          dplyr::rename(meanDwnTm = `Mean Downtime (%)`, sdDwnTm = `SD Downtime (%)`) %>%
          mutate(lwBound = qnorm(p=0.025, mean = meanDwnTm, sd = sdDwnTm),
                 upBound = qnorm(p=c(0.975), mean = meanDwnTm, sd = sdDwnTm)) %>%
          ggplot(aes(x=month, y = meanDwnTm, group=month)) +
          geom_pointrange(aes(ymin=lwBound, ymax=upBound), col = "olivedrab", size =0.8) +
          labs(y = "Downtime (%)", x = "", title = "Monthly turbine downtime (Means & 95% CIs)")
      })  
      
      
      
      output$plot_turbinePars_monthOps_windAvb <- renderPlot({
        
        req(input$hotInput_turbinePars_monthOps)
        
        hot_to_r(input$hotInput_turbinePars_monthOps) %>% rownames_to_column(var="Variable") %>% slice(1) %>%
          gather(month, windAvb, -Variable) %>% select(-Variable) %>%
          mutate(month = factor(month.abb, levels = month.abb)) %>%
          ggplot(aes(x=month, y=windAvb)) +
          geom_col(fill= "olivedrab", alpha = 0.7, width = 0.4) +
          labs(x="", y = "Wind Availability (%)", title = "Monthly wind availability")
        
      })
      
      
      #turbineData_Operation <- hot_to_r(input$hotInput_turbinePars_monthOps) %>%  
        #rownames_to_column(var="Variable") %>%
        #gather(month, Value, -Variable) %>%
        #mutate(Variable = gsub(" \\(%\\)", "", Variable),
               #Variable = gsub(" ", "_", Variable)) %>%
        #unite(Variable, month, col = "Variable") %>%
        #mutate(TurbineModel = input$numInput_turbinePars_turbinePower, 
               #VariableMasden = paste0(rep(month.abb, each=3), "Op", c("", "Mean", "SD")),
               #VariableMasden = factor(VariableMasden, levels = VariableMasden)
        #) %>%
        #select(-Variable) %>%
        #spread(VariableMasden, Value)
      
      
    }
  )
}


    
## To be copied in the UI
# mod_WindFarmFeats_ui("WindFarmFeats_ui_1")
    
## To be copied in the server
# callModule(mod_WindFarmFeats_server, "WindFarmFeats_ui_1")
 

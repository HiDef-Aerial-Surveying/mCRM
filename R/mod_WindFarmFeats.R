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
#' @importFrom DT renderDT
#' @importFrom DT DTOutput
#'  
mod_WindFarmFeats_ui <- function(id){
  idv <- stringr::str_replace_all(id," ", "_")
  ns <- NS(idv)
  tagList(
    # --- Turbine features box
    box(title = "Features", 
        width = 2, 
        status = "primary", 
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        #selectInput(ns("selectInput_builtin_turbine"),
        #            label = "Select turbine type",
        #            width = "85%",
        #            choices = c("Turbine 1" = "tb1", "Turbine 2" = "tb2", "Turbine 3" = "tb3"),
        #            selectize = TRUE
        #),
        
        # --- Number of Turbines
        numericInput(width = "85%", 
                     inputId = ns("numInput_windfarmPars_nTurbines"), 
                     label = label.help("Number of Turbines", ns("lbl_windfarmnTurbines")),
                     value = 1, min = 1, step = 1),
        shinyBS::bsTooltip(id = ns("lbl_windfarmnTurbines"), 
                           title = paste0("Total number of turbines in the wind farm array.", 
                                          " Used in conjunction with turbine model to calculate the target power of the wind farm."),
                           options = list(container = "body"), placement = "right", trigger = "hover"),
        
        # ---  Latitude
        numericInput(width = "85%", 
                     inputId = ns("numInput_windfarmPars_Latitude"), 
                     label = label.help("Latitude (deg)", ns("lbl_windfarmLatitude")), #"Latitude (deg)", 
                     value = 1, min = -90, max = 90, step = 0.01),
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
        
        # --- % upwind flights
        numericInput(width = "85%", 
                     inputId = ns("numInput_windfarmPars_upWindDownWindProp"), 
                     label = label.help("Upwind flights (%)", ns("lbl_upWindDownWindProp")), #"Tidal Offset (m)", 
                     value = 50, min = 0,max=100, step = 1),
        shinyBS::bsTooltip(id = ns("lbl_upWindDownWindProp"), 
                           title = paste0("The percentage of upwind bird flights. Should be 50% unless direction of travel", 
                                          " is biased in a particular direction"),
                           options = list(container = "body"), placement = "right", trigger = "hover"),
        
        ),
    
  
    box(title = "Turbine Parameters", 
        width = 10, 
        status = "primary", 
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        fluidRow(
          box(width = 12,
              splitLayout(
                
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
                
                # Maximum blade width
                numericInput(width = "60%", 
                             inputId = ns("numInput_turbinePars_maxBladeWdth"),
                             label =  label.help("Max blade width (m)", ns("lbl_maxBladeWdth")),
                             value = startUpValues$turbinePars$maxBladeWdth, min = 1, step = 0.1)
                
              )
          ),
          shinyBS::bsTooltip(id = ns("lbl_numBlades"),
                             title = paste0("Number of blades in each turbine"),
                             options = list(container = "body"), placement = "right", trigger = "hover"),
          shinyBS::bsTooltip(id = ns("lbl_rotorRadius"),
                             title = paste0("The distance from the axis of rotation to blade tip"),
                             options = list(container = "body"), placement = "right", trigger = "hover"),
          shinyBS::bsTooltip(id = ns("lbl_maxBladeWdth"),
                             title = paste0("The maximum width of the rotor blade"),
                             options = list(container = "body"), placement = "right", trigger = "hover")
        ),
        
        br(),
          fluidRow(
            column(6,
                   box(width = 12,
                       NormNumericInput(paramID = ns("turbinePars_rotnSpeed"),Button = FALSE,cellWidths="50%",
                                        varName = "Rotation (rpm)",
                                        infoId = ns("lbl_rotSpeedProbDist"),
                                        infoText = paste0("Turbine rotor speed (~ Truncated Normal with lower bound at 0). ", 
                                                          "SD should be 0 unless suitable info on rotor speed variability is available"),
                                        E_value = startUpValues$turbinePars$rotnSpeed_E, SD_value = startUpValues$turbinePars$rotnSpeed_SD),
                       br(),
                       verbatimTextOutput(ns("qtls_turbinePars_rotnSpeed"))
                   )
            ),
            column(6,
                   box(width = 12,
                       NormNumericInput(paramID = ns("turbinePars_bladePitch"), Button = FALSE,cellWidths="50%", 
                                        varName = "Pitch (deg)",
                                        infoId = ns("lbl_bladePitchProbDist"),
                                        infoText = paste0("Blade pitch, i.e. the angle of the blade relative to rotor plane (~Truncated Normal with lower bound at 0). ", 
                                                          "SD should be 0 unless suitable info on pitch variability is available"),
                                        E_value = startUpValues$turbinePars$bladePitch_E, SD_value = startUpValues$turbinePars$bladePitch_SD),
                       br(),
                       verbatimTextOutput(ns("qtls_turbinePars_bladePitch"))
                   )
            )
          
        ),
        br(),
        fluidRow(
          box(width = 12, 
              
              tags$b(HTML(paste0("Monthly Operation. Double click to edit table, CTRL+Enter when finished", actionLink(ns("lbl_monthOPs"), label=NULL, 
                                                                 icon=icon('info-circle'))))),
              shinyBS::bsTooltip(id = ns("lbl_monthOPs"), 
                                 title = paste0("Information on turbine activity per month: % of wind availability (treated as constant) and % of", 
                                                " maintenance downtime (~ Normal)."),
                                 options = list(container = "body"), placement = "right", trigger = "hover"),
              
              br(),
              br(),
              
              DT::DTOutput(ns("hotInput_turbinePars_monthOps")),
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
  nid <- id
  nid <- stringr::str_replace_all(id,"_", " ")
  moduleServer(
    id,
    function(input,output,session){
    
      observe({
        ## Calculate centroid latitude of wind farm and put in input
        shp <- data$WFshapes[data$WFshapes$NAME == nid,]#Scotwind_Merged[Scotwind_Merged$NAME == nid,]
        latval <- round(mean(shp@bbox[2],shp@bbox[4]),1)
        updateNumericInput(inputId="numInput_windfarmPars_Latitude",value=latval)
      })
      
      observe({
        ## Get the number of turbines and put into the tool
        if(data$WF_shape_choice == "existWindFarms"){
          updateNumericInput(inputId = "numInput_windfarmPars_nTurbines",value=Scotwind_Merged$N_TURBINES[Scotwind_Merged$NAME == nid])    
        }else if(data$WF_shape_choice == "customWindFarms"){
          updateNumericInput(inputId = "numInput_windfarmPars_nTurbines",value=100)
        }
      })
      
      observe({
        try(updateNumericInput(inputId = "numInput_windfarmPars_width",value=get_wf_width(data$WFshapes[data$WFshapes$NAME == nid,])))
      })
      
      # --- Create input table for turbine monthly operation parameters
      output$hotInput_turbinePars_monthOps <- renderDT({
        datatable(data$turbinePars_monthOps_df,
                  editable='all',
                  options=list(dom='t',scrollX = TRUE))
      })
      
      
      # # -- Turbine Monthly Operations - Downtime
      # output$plot_turbinePars_monthOps_downtime <- renderPlot({
      # 
      #   req(input$hotInput_turbinePars_monthOps)
      # 
      #   hot_to_r(input$hotInput_turbinePars_monthOps) %>%
      #     rownames_to_column(var="Variable") %>%
      #     slice(2:3) %>%
      #     gather(month, windAvb, -Variable) %>%
      #     mutate(month = factor(month, levels=unique(month), labels = month.abb)) %>%
      #     spread(Variable, windAvb) %>%
      #     dplyr::rename(meanDwnTm = `Mean Downtime (%)`, sdDwnTm = `SD Downtime (%)`) %>%
      #     mutate(lwBound = qnorm(p=0.025, mean = meanDwnTm, sd = sdDwnTm),
      #            upBound = qnorm(p=c(0.975), mean = meanDwnTm, sd = sdDwnTm)) %>%
      #     ggplot(aes(x=month, y = meanDwnTm, group=month)) +
      #     geom_pointrange(aes(ymin=lwBound, ymax=upBound), col = "olivedrab", size =0.8) +
      #     labs(y = "Downtime (%)", x = "", title = "Monthly turbine downtime (Means & 95% CIs)")
      # })
      # 
      # 
      # 
      # output$plot_turbinePars_monthOps_windAvb <- renderPlot({
      # 
      #   req(input$hotInput_turbinePars_monthOps)
      # 
      #   hot_to_r(input$hotInput_turbinePars_monthOps) %>% rownames_to_column(var="Variable") %>% slice(1) %>%
      #     gather(month, windAvb, -Variable) %>% select(-Variable) %>%
      #     mutate(month = factor(month.abb, levels = month.abb)) %>%
      #     ggplot(aes(x=month, y=windAvb)) +
      #     geom_col(fill= "olivedrab", alpha = 0.7, width = 0.4) +
      #     labs(x="", y = "Wind Availability (%)", title = "Monthly wind availability")
      # 
      # })
      # 
      
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


#' bird_features UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bird_features_ui <- function(id){
  ns <- NS(id)
  
  specStartVals <- defaultSpeciesValues[stringr::str_replace_all(defaultSpeciesValues$Scientific_name," ","_") == id,]
  
  tagList(
    fluidRow(
      column(width=12,
             box(width=6,
                 title = "Migration parameters",
                 status = "primary", 
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 
                 fluidRow(
                   column(12,
                          p("Would you like to use the default migration route or upload your own migration pathway?"),
                          
                          radioGroupButtons(inputId = ns("radGrpInput_spshape_builtin_or_userinput"),
                                            individual = TRUE,
                                            justified = TRUE, 
                                            label = NULL,
                                            choices = c("Default migration routes" = "existMigration",
                                                        "Custom migration routes" = "customMigration"),
                                            checkIcon = list(yes = tags$i(class = "fa fa-circle",
                                                                          style = "color: steelblue"),
                                                             no = tags$i(class = "fa fa-circle-o",
                                                                         style = "color: steelblue"))),
                          
                          uiOutput(ns("spShape_upload_shape"))
                   )  
                 ),
                 fluidRow(
                   column(12,
                          p("Next select which migration seasons you would like population estimates for. These
                            will be calculated in the next step when you generate your scenarios. You will have the option
                            of inputting these values manually as well"),
                          br()
                   ),
                   column(4,
                          switchButton(inputId = ns("switch_pre_breeding_migration"),
                                       label = "Pre-breeding migration", 
                                       value = FALSE, col = "GB", type = "OO"),
                          uiOutput(ns('pre_breed_dates'))
                   ),
                   column(4,
                          switchButton(inputId = ns("switch_post_breeding_migration"),
                                       label = "Post-breeding migration", 
                                       value = FALSE, col = "GB", type = "OO"),
                          uiOutput(ns('post_breed_dates'))),
                   column(4,
                          switchButton(inputId = ns("switch_other_migration"),
                                       label = "Other migration", 
                                       value = FALSE, col = "GB", type = "OO"),
                          uiOutput(ns('other_dates')))
                 )
                 
                 
             ),
             
             box(width=6,
                 title = "Migration corridor",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 column(12,
                        leaflet::leafletOutput(ns("MigMap"),width="100%") %>% withSpinner(color="#6794d5")
                 )
             )
      )
      
    ),
    
    fluidRow(
      column(width=12,
             box(width = 6,
                 title = "Species parameters",
                 status = "primary", 
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 radioGroupButtons(inputId = ns("slctInput_biomPars_flType_tp"), #paste0("slctInput_biomPars_flType_tp_", specLabel), 
                                   #label = label.help("Flight Type", paste0("lbl_flType_", specLabel)), 
                                   choices = c("Flapping", "Gliding"), 
                                   selected = ifelse(specStartVals$Flap_or_Glide == "Flap", "Flapping", "Gliding"),
                                   individual = TRUE, justified = FALSE,
                                   checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                    no = icon("remove", lib = "glyphicon"))
                 ),
                 NormNumericInput(paramID = ns("biomPars_bodyLt"),# specID = id, #,specLabel, 
                                  varName = "Body Length (m)",
                                  #infoId = paste0("lbl_bodyLt_", specLabel),
                                  via_InsertUI = TRUE,
                                  E_value = ifelse(is.na(specStartVals$Body_length), 1, specStartVals$Body_length), 
                                  E_min=0, E_max=5, E_step = 0.01,
                                  SD_value = ifelse(is.na(specStartVals$Body_length_SD), 0, specStartVals$Body_length_SD), 
                                  SD_min = 0, SD_step = 0.001),
                 
                 NormNumericInput(paramID = ns("biomPars_wngSpan"),# specID = id, #specID = specLabel, 
                                  varName = "Wing Span (m)",
                                  #infoId = paste0("lbl_wngSpan_", specLabel),
                                  via_InsertUI = TRUE,
                                  E_value = ifelse(is.na(specStartVals$Wing_span), 1, specStartVals$Wing_span), 
                                  E_min=0, E_step = 0.01,
                                  SD_value = ifelse(is.na(specStartVals$Wing_span_SD), 0, specStartVals$Wing_span_SD), 
                                  SD_min = 0, SD_step = 0.001),
                 
                 NormNumericInput( paramID = ns("biomPars_flSpeed"), #specID = id, #specID = specLabel, 
                                   varName = "Flight Speed (m/s)",
                                   #infoId = paste0("lbl_flSpeed_", specLabel),
                                   via_InsertUI = TRUE,
                                   E_value = ifelse(is.na(specStartVals$Flight_speed), 1, specStartVals$Flight_speed), 
                                   E_min=0, E_step = 0.01,
                                   SD_value = ifelse(is.na(specStartVals$Flight_speed_SD), 0, specStartVals$Flight_speed_SD), 
                                   SD_min = 0, SD_step = 0.01),
                 
                 # NormNumericInput(paramID = "biomPars_noctAct", specID = "test4",#specLabel, 
                 #                  varName = "Nocturnal Activity",
                 #                  #infoId = paste0("lbl_noctAct_", specLabel),
                 #                  via_InsertUI = TRUE,
                 #                  E_value = 1,#ifelse(is.null(specStartVals$noctAct_E), 1, specStartVals$noctAct_E), 
                 #                  E_min=0, E_step = 0.001,
                 #                  SD_value = 0,#ifelse(is.null(specStartVals$noctAct_SD), 0, specStartVals$noctAct_SD), 
                 #                  SD_min = 0, SD_step = 0.001),
                 
                 NormNumericInput(paramID = ns("biomPars_basicAvoid"), #specID = id ,#specLabel, 
                                  varName = "Avoidance Rate",
                                  #infoId = paste0("lbl_basicAvoid_", specLabel),
                                  via_InsertUI = TRUE,
                                  E_value = ifelse(is.na(specStartVals$Avoidance_rate), 1, specStartVals$Avoidance_rate),  
                                  E_min=0, E_step = 0.001,
                                  SD_value = ifelse(is.na(specStartVals$Avoidance_rate_SD), 0, specStartVals$Avoidance_rate_SD),
                                  SD_min = 0, SD_step = 0.001),
                 
                 numericInput(inputId = ns("biomPars_CRHeight"),
                              label = "Proportion at CRH",
                              value = ifelse(is.na(specStartVals$Prop_CRH), 1, specStartVals$Prop_CRH),
                              min=0,max=1,step=0.01,width="33%"),
                 
                 numericInput(inputId = ns("biomPars_biogeographic_pop"),
                              label = "Biogeographic population",
                              value = ifelse(is.na(specStartVals$biogeographic_pop), 1, specStartVals$biogeographic_pop),
                              min=0,step=1,width="33%"),
                 
                 numericInput(inputId = ns("biomPars_prop_uk"),
                              label = "Proportion of population in UK",
                              value = ifelse(is.na(specStartVals$prop_uk_waters), 1, specStartVals$prop_uk_waters),
                              min=0,max=1,step=0.01,width="33%"),
                 
                 numericInput(inputId = ns("biomPars_uk_population"),
                              label = "UK Population",
                              value = 1,
                              min=0,max=1,step=1,width="33%")
                 
                 
             ),
             
             box(width = 6,
                 title = "Density plots",
                 status = "primary", 
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 plotOutput(ns("Density_Plot_Space"))
             )
      )
      
      
    )
    
    
    
  )
}

#' bird_features Server Function
#'
#' @noRd 
mod_bird_features_server <- function(id,data){
  moduleServer(
    id,
    function(input,output,session){
      
      ns <- session$ns
      
      # Observers for the migration buttons to display the migratory periods
      observeEvent(input$switch_pre_breeding_migration,{
        output$pre_breed_dates <- renderUI({p("")})
        if(input$switch_pre_breeding_migration == TRUE){
          migp <- defaultSpeciesValues$Pre_breed_mig_months[stringr::str_replace_all(defaultSpeciesValues$Scientific_name," ","_") == id]
          output$pre_breed_dates <- renderUI({
            p(migp)
          })
        }else{
          output$pre_breed_dates <- renderUI({p("")})
        }
      })
      observeEvent(input$switch_post_breeding_migration,{
        output$post_breed_dates <- renderUI({p("")})
        migp <- defaultSpeciesValues$Post_breed_mig_months[stringr::str_replace_all(defaultSpeciesValues$Scientific_name," ","_") == id]
        if(input$switch_post_breeding_migration == TRUE){
          output$post_breed_dates <- renderUI({
            p(migp)
          })  
        }else{
          output$post_breed_dates <- renderUI({p("")})
        }
      })
      observeEvent(input$switch_other_migration,{
        output$other_dates <- renderUI({p("")})
        migp <- defaultSpeciesValues$Other_mig_months[stringr::str_replace_all(defaultSpeciesValues$Scientific_name," ","_") == id]
        if(input$switch_other_migration){
          output$other_dates <- renderUI({
            p(migp)
          })  
        }else{
          output$other_dates <- renderUI({p("")})
        }
      })
      
      # Migratory Map control ---------------------------------------------------
      Popvals <- reactive({
        data.frame(pop=input$biomPars_biogeographic_pop,prop_uk=input$biomPars_prop_uk)
      })
      
      # Control for the UK population calculation
      observe({
        Popvals <- Popvals()
        updateNumericInput(inputId = "biomPars_uk_population",value= ceiling(Popvals$pop * Popvals$prop_uk))
        shinyjs::disable("biomPars_uk_population")
      })
      
      
      # Gets the migratory pathway polygon for the species
      SpPoly <- reactive({
        sppc <- defaultSpeciesValues$Sp_code[stringr::str_replace_all(defaultSpeciesValues$Scientific_name," ","_") == id]
        SpPoly <- all_polygons[[sppc]]
        SpPoly <- sf::st_transform(SpPoly,4326)
      })
      
      # Draws the migratory pathway
      output$MigMap <- renderLeaflet({
        cur.popup <- paste0("<strong>Name: </strong>", data@data$NAME)
        leaflet::leaflet() %>%
          leaflet::addProviderTiles(providers$Esri.OceanBasemap,
                                    options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
          setView(-4, 55, zoom = 5) %>%
          addPolygons(data=data,weight = 1, fillColor = "red", popup=cur.popup, fillOpacity = 1) %>%
          addPolygons(data=SpPoly(),fillColor="green",fillOpacity=0.6)
      })
      
      # Creates the popup for the windfarms so they are also plotted 
      observeEvent(input$button_update_Windfarm_tabs, {
        cur.popup <- paste0("<strong>Name: </strong>", data$NAME)
        leaflet::leafletProxy("MigMap",data=data) %>% clearShapes() %>%
          addPolygons(weight = 1, fillColor = "red", popup=cur.popup, fillOpacity = 1) %>%
          addPolygons(data=SpPoly(),fillColor="green",fillOpacity=0.7)
      })
      

      # Controls for plotting density histograms --------------------------------
      
      observeEvent(input$biomPars_bodyLt,{
        mu <- input$biomPars_bodyLt_E_numInput
        stdev <- input$biomPars_bodyLt_SD_numInput
        output$Density_Plot_Space <- renderPlot({truncNormPars_densPlots(mu = mu,
                                                                         stdev = stdev,
                                                                         xlab="Body length (m)")})
      })
      observeEvent(input$biomPars_wngSpan,{
        mu <- input$biomPars_wngSpan_E_numInput
        stdev <- input$biomPars_wngSpan_SD_numInput
        output$Density_Plot_Space <- renderPlot({truncNormPars_densPlots(mu = mu,
                                                                         stdev = stdev,
                                                                         xlab="Wing span (m)")})
      })
      observeEvent(input$biomPars_flSpeed,{
        mu <- input$biomPars_flSpeed_E_numInput
        stdev <- input$biomPars_flSpeed_SD_numInput
        output$Density_Plot_Space <- renderPlot({truncNormPars_densPlots(mu = mu,
                                                                         stdev = stdev,
                                                                         xlab="Flight speed (m/s)")})
      })
      observeEvent(input$biomPars_basicAvoid,{
        mu <- input$biomPars_basicAvoid_E_numInput
        stdev <- input$biomPars_basicAvoid_SD_numInput
        output$Density_Plot_Space <- renderPlot({truncNormPars_densPlots(mu = mu,
                                                                         stdev = stdev,
                                                                         xlab="Avoidance rate")})
      })
      
    })
}

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
  tagList(
    fluidRow(
      column(width=12,
             box(width=6,
                 title = "Migration parameters",
                 status = "primary", 
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 column(12,
                        
                        radioGroupButtons(inputId = ns("radGrpInput_Densities_userinput_or_lines"),
                                          individual = TRUE,
                                          justified = TRUE, 
                                          label = NULL,
                                          choices = c("Manual input" = "manualDensities",
                                                      "Simulate counts" = "simulateDensities"),
                                          checkIcon = list(yes = tags$i(class = "fa fa-circle",
                                                                        style = "color: steelblue"),
                                                           no = tags$i(class = "fa fa-circle-o",
                                                                       style = "color: steelblue"))),
                        
                        uiOutput(ns("spShape_builtin_or_userinput_radio")),
                        
                        uiOutput(ns("spShape_upload_shape")),
                        
                        switchButton(inputId = ns("switch_pre_breeding_migration"),
                                     label = "Pre-breeding migration", 
                                     value = TRUE, col = "GB", type = "OO"),
                        
                        switchButton(inputId = ns("switch_post_breeding_migration"),
                                     label = "Post-breeding migration", 
                                     value = FALSE, col = "GB", type = "OO"),
                        
                        switchButton(inputId = ns("switch_other_migration"),
                                     label = "Other migration", 
                                     value = FALSE, col = "GB", type = "OO")
                        
                        
                        
                 )  
                 
             ),
             
            box(width=6,
                 title = "Migration corridor",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 column(12,
                        leaflet::leafletOutput(ns("MigMap"),width="100%") %>% withSpinner(color="#6794d5")
                        #h3("Migratory map will appear here")
                 )

             ),

      )
    ),
    
    fluidRow(
      column(width=12,
             box(width=12,
                 title = "Counts per windfarm",
                 status = "primary", 
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 uiOutput(ns("BreedingDensities"))
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
                 radioGroupButtons(inputId = "test1", #paste0("slctInput_biomPars_flType_tp_", specLabel), 
                                   #label = label.help("Flight Type", paste0("lbl_flType_", specLabel)), 
                                   choices = c("Flapping", "Gliding"), 
                                   selected = "Flapping",#ifelse(is.null(specStartVals$flType), "Flapping", specStartVals$flType),
                                   individual = TRUE, justified = FALSE,
                                   checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                    no = icon("remove", lib = "glyphicon"))
                 ),
                 NormNumericInput(paramID = "biomPars_bodyLt", specID = "test2", #,specLabel, 
                                  varName = "Body Length (m)",
                                  #infoId = paste0("lbl_bodyLt_", specLabel),
                                  via_InsertUI = TRUE,
                                  E_value = 1,#ifelse(is.null(specStartVals$bodyLt_E), 1, specStartVals$bodyLt_E), 
                                  E_min=0, E_max=5, E_step = 0.01,
                                  SD_value = 0,#ifelse(is.null(specStartVals$bodyLt_SD), 0, specStartVals$bodyLt_SD), 
                                  SD_min = 0, SD_step = 0.001),
                 
                 NormNumericInput(paramID = "biomPars_wngSpan", specID = "test3", #specID = specLabel, 
                                  varName = "Wing Span (m)",
                                  #infoId = paste0("lbl_wngSpan_", specLabel),
                                  via_InsertUI = TRUE,
                                  E_value = 1,#ifelse(is.null(specStartVals$wngSpan_E), 1, specStartVals$wngSpan_E), 
                                  E_min=0, E_step = 0.01,
                                  SD_value = 0,#ifelse(is.null(specStartVals$wngSpan_SD), 0, specStartVals$wngSpan_SD), 
                                  SD_min = 0, SD_step = 0.001),
                 
                 NormNumericInput( paramID = "biomPars_flSpeed", specID = "test4", #specID = specLabel, 
                                   varName = "Flight Speed (m/s)",
                                   #infoId = paste0("lbl_flSpeed_", specLabel),
                                   via_InsertUI = TRUE,
                                   E_value = 1,#ifelse(is.null(specStartVals$flSpeed_E), 1, specStartVals$flSpeed_E), 
                                   E_min=0, E_step = 0.01,
                                   SD_value = 0,#ifelse(is.null(specStartVals$flSpeed_SD), 0, specStartVals$flSpeed_SD), 
                                   SD_min = 0, SD_step = 0.01),
                 
                 # NormNumericInput(paramID = "biomPars_noctAct", specID = "test4",#specLabel, 
                 #                  varName = "Nocturnal Activity",
                 #                  #infoId = paste0("lbl_noctAct_", specLabel),
                 #                  via_InsertUI = TRUE,
                 #                  E_value = 1,#ifelse(is.null(specStartVals$noctAct_E), 1, specStartVals$noctAct_E), 
                 #                  E_min=0, E_step = 0.001,
                 #                  SD_value = 0,#ifelse(is.null(specStartVals$noctAct_SD), 0, specStartVals$noctAct_SD), 
                 #                  SD_min = 0, SD_step = 0.001),
                 
                 NormNumericInput(paramID = "biomPars_basicAvoid", specID = "test5" ,#specLabel, 
                                  varName = "Basic Avoidance",
                                  #infoId = paste0("lbl_basicAvoid_", specLabel),
                                  via_InsertUI = TRUE,
                                  E_value = 1,#ifelse(is.null(specStartVals$basicAvoid_E), 1, specStartVals$basicAvoid_E),  
                                  E_min=0, E_step = 0.001,
                                  SD_value = 0,#ifelse(is.null(specStartVals$basicAvoid_SD), 0, specStartVals$basicAvoid_SD),
                                  SD_min = 0, SD_step = 0.001),
                 
                 NormNumericInput(paramID = "biomPars_CRHeight", specID = "test6", #specLabel, 
                                  varName = "Proportion at CRH",
                                  #infoId = paste0("lbl_CRHeight_", specLabel),
                                  via_InsertUI = TRUE,
                                  E_value = 1,#ifelse(is.null(specStartVals$CRHeight_E), 1, specStartVals$CRHeight_E),
                                  E_min=0, E_step = 0.01,
                                  SD_value = 0,#ifelse(is.null(specStartVals$CRHeight_SD), 0, specStartVals$CRHeight_SD), 
                                  SD_min = 0, SD_step = 0.001)
                 
             ),
             
             box(width = 6,
                 title = "Plotting space",
                 status = "primary", 
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 h3("Press button to view plot of distribution")
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
        
        
        observeEvent(input$radGrpInput_Densities_userinput_or_lines,{
          if(input$radGrpInput_Densities_userinput_or_lines == "simulateDensities"){
            output$spShape_builtin_or_userinput_radio <- renderUI(
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
            )
          }else if(input$radGrpInput_Densities_userinput_or_lines == "manualDensities"){
            removeUI(paste0("#",ns("radGrpInput_spshape_builtin_or_userinput")),immediate = TRUE)
          }
        })
        
        
        
        
        ### Observe the pre breeding switch - if it's turned on, create the UI for the bird densities
        observe({
          
          output$BreedingDensities <- renderUI({
            lapply(data@data$NAME,function(x){
              print(x)
              fluidRow(
                column(2,
                conditionalPanel(condition=paste0(ns("input.switch_pre_breeding_migration")," == true"),
                                 column(3,
                                        mod_bird_densities_ui(x,"Goose","Pre breeding")
                                 )
                ),
                conditionalPanel(condition="input.switch_post_breeding_migration == true",
                                 column(3,
                                        mod_bird_densities_ui(x,"Goose","Post breeding")
                                 )
                ),
                conditionalPanel(condition="input.switch_other_migration == true",
                                 column(3,
                                        mod_bird_densities_ui(x,"Goose","Other")
                                 )
                )
              ))
              
            })
          })
        })
        
        
        observeEvent(input$switch_pre_breeding_migration,{
          if(input$switch_pre_breeding_migration == TRUE){
            output$bird_migration_densities <- renderUI({
              lapply(data@data$NAM,function(x){
                mod_bird_densities_ui(x,"Goose","Pre breeding")
                
              })
            })  
          }else if(input$switch_pre_breeding_migration == FALSE){
            lapply(WFshapes()$NAME,function(x){
              idtoremove <- paste(x,"Goose","Pre breeding")
              idtoremove <- stringr::str_replace_all(idtoremove," ","_")
              idtoremove <- stringr::str_replace_all(idtoremove,"-","_")
              idtoremove <- paste0("#",idtoremove,"-counts")
              removeUI(idtoremove,immediate = TRUE)
            })
          }
        })
        
        ### Observe the post breeding switch - if it's turned on, create the UI for the bird densities
        observeEvent(input$switch_post_breeding_migration,{
          if(input$switch_post_breeding_migration == TRUE){
            output$bird_migration_densities <- renderUI({
              lapply(WFshapes()$NAME,function(x){
                mod_bird_densities_ui(x,"Goose","Post breeding")
                
              })
            })  
          }else if(input$switch_post_breeding_migration == FALSE){
            lapply(WFshapes()$NAME,function(x){
              idtoremove <- paste(x,"Goose","Post breeding")
              idtoremove <- stringr::str_replace_all(idtoremove," ","_")
              idtoremove <- stringr::str_replace_all(idtoremove,"-","_")
              idtoremove <- paste0("#",idtoremove,"-counts")
              removeUI(idtoremove,immediate = TRUE)
            })
          }
        })
        
        ### Observe the other migration switch - if it's turned on, create the UI for the bird densities
        observeEvent(input$switch_other_migration,{
          if(input$switch_other_migration == TRUE){
            output$bird_migration_densities <- renderUI({
              lapply(WFshapes()$NAME,function(x){
                mod_bird_densities_ui(x,"Goose","Other")
              })
            })  
          }else if(input$switch_other_migration == FALSE){
            lapply(WFshapes()$NAME,function(x){
              idtoremove <- paste(x,"Goose","Other")
              idtoremove <- stringr::str_replace_all(idtoremove," ","_")
              idtoremove <- stringr::str_replace_all(idtoremove,"-","_")
              idtoremove <- paste0("#",idtoremove,"-counts")
              removeUI(idtoremove,immediate = TRUE)
            })
          }
        })
        
        
        
        
        
        # Migratory Map control ---------------------------------------------------
        
        
        
        SpPoly <- reactive({
          sppc <- "Anser_anser"
          SpPoly <- all_polygons[[sppc]]
          SpPoly <- sf::st_transform(SpPoly,4326)
          
        })
        WFshapes <- reactive({
          EMOD_OSWFs_WGS84_UK[EMOD_OSWFs_WGS84_UK$NAME %in% input$selectInput_builtin_wfList,]
        })
        
        
        output$MigMap <- renderLeaflet({
          cur.popup <- paste0("<strong>Name: </strong>", WFshapes()$NAME)
          leaflet::leaflet() %>%
            leaflet::addProviderTiles(providers$Esri.OceanBasemap,
                                      options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
            setView(-4, 55, zoom = 5) %>%
            addPolygons(data=WFshapes(),weight = 1, fillColor = "red", popup=cur.popup, fillOpacity = 1) %>%
            addPolygons(data=SpPoly(),fillColor="green",fillOpacity=0.7)
        })
        
        
        
        observe({
          cur.popup <- paste0("<strong>Name: </strong>", WFshapes()$NAME)
          
          leaflet::leafletProxy(ns("MigMap"),data=WFshapes()) %>% clearShapes() %>%
            addPolygons(weight = 1, fillColor = "red", popup=cur.popup, fillOpacity = 1) %>%
            addPolygons(data=SpPoly(),fillColor="green",fillOpacity=0.7)
        })
        
        
        
        
        
        
  
      })
  
}
    
## To be copied in the UI
# mod_bird_features_ui("bird_features_ui_1")
    
## To be copied in the server
# callModule(mod_bird_features_server, "bird_features_ui_1")
 

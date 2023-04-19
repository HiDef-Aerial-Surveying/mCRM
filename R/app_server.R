#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyBS
#' @import shinyWidgets
#' @import ggplot2
#' @import leaflet
#' @import tibble
#' @import stochLAB
#' @import officedown
#' @import sp
#' @import sf
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom purrr walk2
#' @importFrom plyr ldply
#' @importFrom reshape2 melt
#' @importFrom raster shapefile
#' @importFrom shinyalert shinyalert
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_xlsx
#' @importFrom tools file_ext
#' @importFrom DT datatable
#' @importFrom DT editData
#' @importFrom DT renderDT
#' @importFrom DT DTOutput
#' @importFrom stringr str_replace_all
#' @importFrom foreach foreach
#' @importFrom foreach %do%
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx writeData
#' @importFrom openxlsx saveWorkbook
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_xlsx
#' 
#' 
app_server <- function( input, output, session ) {

  # ----------------------------------------------------
  # ----         setting session specifics          ----
  # ----------------------------------------------------
  
  # --- session's "global" Variables
  
  # --- Initiate session's reactive variables
  WF_shape_choice <- reactive({input$selectInput_wfshape_builtin_or_userinput})
  
  rv <- reactiveValues(
    WFshapes = NULL,
    WF_shape_choice = NULL,
    customWFshapes = NULL,
    WFsSelected = NULL,
    turbinePars_monthOps_df = data.frame(
      matrix(
        c(startUpValues$turbinePars$windAvail, startUpValues$turbinePars$meanDownTime, startUpValues$turbinePars$sdDownTime),
        nrow = 3, ncol = 12, byrow = TRUE, 
        dimnames = list(c("Wind Availability (%)", "Mean Downtime (%)", "SD Downtime (%)"), month.name)
      ),
      stringsAsFactors = FALSE
    ),
    pitchVsWind_df = startUpValues$turbinePars$pitchVsWind_df,
    rotationVsWind_df = startUpValues$turbinePars$rotationVsWind_df
  )
  
  # Map display controls ----------------------------------------------------

  ## Loads the base leaflet map with just the tiles
  output$map <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(providers$Esri.OceanBasemap,
                                options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
      setView(-4, 55, zoom = 5)
  })
  
  ## Stores the names of the Wind farms selected from the UI
  WFshapes <- reactive({
    WF_shape_choice <- WF_shape_choice()
    if(WF_shape_choice == "existWindFarms"){
      Scotwind_Merged[Scotwind_Merged$NAME %in% input$selectInput_builtin_wfList,]  
    }else if(WF_shape_choice == "customWindFarms"){
      rv$customWFshapes[rv$customWFshapes$NAME %in% input$selectInput_custom_wf_header,]
    }
  })
  
  ## Stores a vector of the windfarm shapes which is used to update the shapefiles visible
  ## In the map after the button has been clicked
  WFshapelist <- reactiveValues(wfs=vector())

  ## When the button has been clicked to update the windfarm tabs, we update the map
  ## As well as update the tab list (appendTab function) and load in the windfarmfeats_ui module
  ## The windfarmfeats server module is also called here
  observeEvent(input$button_update_Windfarm_tabs, {
    rv$WF_shape_choice <- WF_shape_choice()
    rv$WFshapes <- WFshapes()
    cur.popup <- paste0("<strong>Name: </strong>", WFshapes()$NAME)
    leaflet::leafletProxy("map",data=WFshapes()) %>% clearShapes() %>%
      addPolygons(weight = 1, fillColor = "red", popup=cur.popup, fillOpacity = 1) 
    if(length(WFshapelist$wfs) == 0){
      lapply(WFshapes()$NAME,function(x) {
        id_name <- stringr::str_replace_all(x," ","_")
        appendTab("windfarm_Tabs",tabPanel(x,mod_WindFarmFeats_ui(id_name)),select = TRUE)
        mod_WindFarmFeats_server(id_name,data=rv)
      })
      WFshapelist$wfs <- WFshapes()$NAME
    }else if(length(WFshapelist$wfs) > 0){
      for(j in WFshapelist$wfs){
        if(j %!in% WFshapes()$NAME){
          removeTab("windfarm_Tabs",j)
        }
      }
      for(i in WFshapes()$NAME){
        if(i %!in% WFshapelist$wfs){
          id_name <- stringr::str_replace_all(i," ","_")
          appendTab("windfarm_Tabs",tabPanel(i,mod_WindFarmFeats_ui(id_name)),select=TRUE)
          mod_WindFarmFeats_server(id_name,data = rv)
        }
      }
      WFshapelist$wfs <- WFshapes()$NAME
    }
  })
  
  
 # Control for adding bird parameters module to the UI based on use --------
  ## Stores the species names from the input list selected in the UI
  BirdNames <- reactive({
    input$selectInput_builtin_speciesList
  })
  
  ## Stores the species list names for updating the tab list when the user selects
  ## different species
  birdspecieslist <- reactiveValues(birdspcs=vector())
  
  # This will add bird species tabs when the action button is clicked
  # It should only add tabs if they aren't already on the list
  observeEvent(input$button_update_Species_tabs, {
    
    if(length(birdspecieslist$birdspcs) == 0){
      lapply(BirdNames(),function(x) {
        
        id_name <- defaultSpeciesValues$Sp_code[which(defaultSpeciesValues$Common_name == x)] 
        #id_name <- stringr::str_replace_all(id_name," ","_")
        
        appendTab("specTabs",tabPanel(x,mod_bird_features_ui(id_name)))
        mod_bird_features_server(id_name,WFshapes())
        
      })
      birdspecieslist$birdspcs <- BirdNames()
    }else if(length(birdspecieslist$birdspcs) > 0){
      for(j in birdspecieslist$birdspcs){
        if(j %!in% BirdNames()){
          removeTab("specTabs",j)
        }
      }
      for(i in BirdNames()){
        if(i %!in% birdspecieslist$birdspcs){
          id_name <- defaultSpeciesValues$Sp_code[which(defaultSpeciesValues$Common_name == i)] 
          #id_name <- stringr::str_replace_all(id_name," ","_")
          
          appendTab("specTabs",tabPanel(i,mod_bird_features_ui(id_name)))
          mod_bird_features_server(id_name,WFshapes())
        }
      }
      birdspecieslist$birdspcs <- BirdNames()
    }
  })
  
  observeEvent(input$selectInput_builtin_speciesList,{
    count <- length(BirdNames())
    output$Species_Count <- renderUI(
      if(count == 1){
        p(paste(count,"species has been selected"))  
      }else{
        p(paste(count,"species have been selected"))  
      }
    )
  })
  

# Generate scenarios server actions ---------------------------------------
  ## For all birds selected, will access the list of inputs from the module 
  ## into a data frame, which is then used to present as an R Hands on table on the
  ## Front end UI
  bird.data.rvs <- reactive({
    sapply(BirdNames(),
           function(x){
             id_name <- defaultSpeciesValues$Sp_code[which(defaultSpeciesValues$Common_name == x)]
             #id_name <- stringr::str_replace_all(id_name," ","_")
             data.frame(
               flying = eval(parse(text=paste0("input$`",id_name,"-slctInput_biomPars_flType_tp`"))),
               bdlenE = eval(parse(text=paste0("input$`",id_name,"-biomPars_bodyLt_E_numInput`"))),
               bdlenSD = eval(parse(text=paste0("input$`",id_name,"-biomPars_bodyLt_SD_numInput`"))),
               wnspnE = eval(parse(text=paste0("input$`",id_name,"-biomPars_wngSpan_E_numInput`"))),
               wnspnSD = eval(parse(text=paste0("input$`",id_name,"-biomPars_wngSpan_SD_numInput`"))),
               flSpdE = eval(parse(text=paste0("input$`",id_name,"-biomPars_flSpeed_E_numInput`"))),
               flSpdSD = eval(parse(text=paste0("input$`",id_name,"-biomPars_flSpeed_SD_numInput`"))),
               AvoidE = eval(parse(text=paste0("input$`",id_name,"-biomPars_basicAvoid_E_numInput`"))),
               AvoidSD = eval(parse(text=paste0("input$`",id_name,"-biomPars_basicAvoid_SD_numInput`"))),
               PCH = eval(parse(text=paste0("input$`",id_name,"-biomPars_CRHeight`"))),
               BioGpop = eval(parse(text=paste0("input$`",id_name,"-biomPars_biogeographic_pop`"))),
               BioGprop = eval(parse(text=paste0("input$`",id_name,"-biomPars_prop_uk`"))),
               Totalpop = eval(parse(text=paste0("input$`",id_name,"-biomPars_uk_population`"))),
               PreBM = eval(parse(text=paste0("input$`",id_name,"-switch_pre_breeding_migration`"))),
               PostBM = eval(parse(text=paste0("input$`",id_name,"-switch_post_breeding_migration`"))),
               OthBM = eval(parse(text=paste0("input$`",id_name,"-switch_other_migration`")))
             )
           },simplify=FALSE,USE.NAMES=TRUE)
  })
  
  ## As per the bird.data.rvs only for the wind farm data
  wf.data.rvs <- reactive({
    sapply(WFshapes()$NAME,
           function(x){
             x <- stringr::str_replace_all(x," ","_")
             tt <- data.frame(
               Latitude =  input[[paste0(x,"-numInput_windfarmPars_Latitude")]], 
               wfWidth = eval(parse(text=paste0("input$`",x,"-numInput_windfarmPars_width`"))),
               PropUpwind = eval(parse(text=paste0("input$`",x,"-numInput_windfarmPars_upWindDownWindProp`"))),
               nTurbines = eval(parse(text=paste0("input$`",x,"-numInput_windfarmPars_nTurbines`"))),
               nBlades = eval(parse(text=paste0("input$`",x,"-numInput_turbinePars_numBlades`"))),
               rRadius = eval(parse(text=paste0("input$`",x,"-numInput_turbinePars_rotRadius`"))),
               bWidth = eval(parse(text=paste0("input$`",x,"-numInput_turbinePars_maxBladeWdth`"))),
               RotnSpdE = eval(parse(text=paste0("input$`",x,"-turbinePars_rotnSpeed_E_numInput`"))),
               RotnSpdSD = eval(parse(text=paste0("input$`",x,"-turbinePars_rotnSpeed_SD_numInput`"))),
               BldPitchE = eval(parse(text=paste0("input$`",x,"-turbinePars_bladePitch_E_numInput`"))),
               BldPitchSD = eval(parse(text=paste0("input$`",x,"-turbinePars_bladePitch_SD_numInput`")))
             )
             #winddatatable <- eval(parse(text=paste0(
             #   "rhandsontable::hot_to_r(input$`",x,"-hotInput_turbinePars_monthOps`)"
             #)))
             if(length(input[[paste0(x,"-hotInput_turbinePars_monthOps_cell_clicked")]]) > 0){
               winddatatable <- DT::editData(rv$turbinePars_monthOps_df,
                            input[[paste0(x,"-hotInput_turbinePars_monthOps_cell_edit")]],
                            paste0(x,"-hotInput_turbinePars_monthOps")
               )
             }else{
               winddatatable <- rv$turbinePars_monthOps_df  
             }
             meanWA <- winddatatable[1,]
             meanDT <- winddatatable[2,]
             names(meanDT) <- paste0(month.abb," mean down time")
             SDDT <- winddatatable[3,]
             names(SDDT) <- paste0(month.abb," SD down time")
             tt <- do.call("cbind",list(tt,meanWA,meanDT,SDDT))
             return(tt)
           },simplify=FALSE,USE.NAMES=TRUE)
  })


# Observe if the wind turbine parameters have been edited -----------------


  observeEvent(input$button_generate_scenarios  ,{
    
    BirdTest <- tryCatch(bird.data.rvs()[[1]],
                         error=function(e) NULL)
    if(!is.null(BirdTest)){
      BirdTest <- tryCatch(BirdTest[[1]],
                           error=function(e) NULL)
    }
    
    if(is.null(BirdTest)){
      shinyalert::shinyalert(
        title = "Error",
        text = "No birds have been selected, return to Step 2",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      req(BirdTest)
    }
    
    wfTest <- tryCatch(wf.data.rvs(),
                       error=function(e) NULL)
    if(is.null(wfTest)){
      shinyalert::shinyalert(
        title = "Error",
        text = "No wind farms have been selected, return to Step 1",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      req(wfTest)
    }
    
    BirdNames <- tryCatch(BirdNames(),
                          error=function(e) NULL)
    WFNames <- tryCatch(WFshapes()$NAME,
                        error=function(e) NULL)
    bird_scenario_table <- tibble(Species = BirdNames)
    bird_scenario_table <- foreach(i=1:nrow(bird_scenario_table),.combine='rbind') %do% {
      xx <- cbind(bird_scenario_table[i,],bird.data.rvs()[[bird_scenario_table$Species[i]]])
      if(xx$PreBM == TRUE){
        xx$PreBM <- defaultSpeciesValues$Pre_breed_mig_months[defaultSpeciesValues$Common_name == xx$Species]
      }else{
        xx$PreBM <- "NA" 
      }
      if(xx$PostBM == TRUE){
        xx$PostBM <- defaultSpeciesValues$Post_breed_mig_months[defaultSpeciesValues$Common_name == xx$Species]
      }else{
        xx$PostBM <- "NA" 
      }
      if(xx$OthBM == TRUE){
        xx$OthBM <- defaultSpeciesValues$Other_mig_months[defaultSpeciesValues$Common_name == xx$Species]
      }else{
        xx$OthBM <- "NA" 
      }
      return(xx)
    }
    
    output$hotInput_output_bird_scenarios <- renderRHandsontable(
      bird_scenario_table %>%
        rhandsontable(selectCallback = TRUE,rowHeaders=NULL, colHeaders = c("Species",  
                                                                            "Flight","Body Length",
                                                                            "Body Length SD", "Wingspan", "Wingspan SD",
                                                                            "Flight speed", "Flight speed SD",
                                                                            "Avoidance", "Avoidance SD", "PCH","Biogeographic population",
                                                                            "Proportion in UK", "Total population in UK", 
                                                                            "Pre-breeding migration","Post-breeding migration",
                                                                            "Other migration")) %>%
        hot_cols() %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    )
    
    wind_scenario_table <- tibble(WindFarm = WFNames)
    
    wind_scenario_table <- foreach(i=1:nrow(wind_scenario_table),.combine='rbind') %do% {
      xx <- cbind(wind_scenario_table[i,],wf.data.rvs()[[wind_scenario_table$WindFarm[i]]])
      return(xx)
    }
    
  
    output$hotInput_output_wf_scenarios <- renderRHandsontable(
      wind_scenario_table %>%
        rhandsontable(selectCallback = TRUE,rowHeaders=NULL, colHeaders = c("Wind farm","Latitude","Width","Proportion upwind flight",
                                                                            "Number of turbines","Number of blades",
                                                                            "Rotor radius", "Blade width",
                                                                            "Rotation Speed", "Rotation Speed SD", "Blade Pitch", "Blade Pitch SD",
                                                                            paste0(month.abb," wind available"),paste0(month.abb," mean downtime"),
                                                                            paste0(month.abb," SD downtime"))) %>%
        hot_cols() %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    )
    
    ### 10000 lines, 2000 samples with 500 bootstraps from sensitivity analysis Sept 2021
    ### This samples each windfarm x species combination to generation populations
    
    boot.iters <- 100
    samplesize <- 2000
    
    WFshapes <- as(WFshapes(), "sf")
    WFshapes <- sf::st_transform(WFshapes,sf::st_crs(all_lines$batgo))
    
    Population_estimates <- expand.grid(WFshapes()$NAME,BirdNames)
    names(Population_estimates) <- c("Wind farm", "Species")
    Population_estimates$Estimate <- NA
    Population_estimates$EstimateSD <- NA
    ### Add mean proportion of lines selected
    Population_estimates$Line_Proportion <- NA
    
    withProgress(message = "Generating population estimates",value=0,{
      for(k in 1:nrow(Population_estimates)){
        incProgress(1/nrow(Population_estimates),detail=paste("running", Population_estimates$Species[k], "in", Population_estimates$`Wind farm`[k] ))
        Specnm <- stringr::str_replace_all(defaultSpeciesValues$Scientific_name[defaultSpeciesValues$Common_name == Population_estimates$Species[k]]," ","_")
        btonm <- stringr::str_replace_all(defaultSpeciesValues$Sp_code[defaultSpeciesValues$Common_name == Population_estimates$Species[k]]," ","_")
        speclines <- eval(parse(text=paste0("all_lines$",btonm)))
        ## Population is multiplied by proportion in UK waters
        popSizemn <- bird.data.rvs()[Population_estimates$Species[k]][[1]]$Totalpop
        Estimates <- vector(length=boot.iters)
        
        withProgress(message = "bootstrapping", value=0, {
          for(j in 1:boot.iters){
            incProgress(1/boot.iters)
            PopVal <- popSizemn
            Estimates[j] <- ceiling(length(GetSampleProp(speclines,samplesize,WFshapes[Population_estimates$`Wind farm`[k],]))/samplesize * PopVal)
          }
        })
        Population_estimates$Estimate[k] <- ceiling(mean(Estimates,na.rm=TRUE))
        Population_estimates$EstimateSD[k] <- ceiling(sd(Estimates,na.rm=TRUE))
        Population_estimates$Line_Proportion[k] <- round(mean(Estimates,na.rm=TRUE)/popSizemn, 3)
      }
    })
    
    output$hotInput_output_population_scenarios <- renderRHandsontable(
      Population_estimates %>%
        rhandsontable(selectCallback = TRUE,rowHeaders=NULL, colHeaders = c("Wind farm","Species", "Population estimate","Population estimate (SD)","Proportion at-risk flight lines")) %>%
        hot_cols() %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    )
    
    
  })
  
  

# Download scenarios controls ---------------------------------------------

  # Set output from windfarm table, bird table, and scenario table to 
  # R objects in reactive forms
  
  WindFarmsData <- reactive({
    Df <- hot_to_r(input$hotInput_output_wf_scenarios)
    names(Df) <- c("Wind farm","Latitude","Width","Proportion upwind flight",
                   "Number of turbines","Number of blades",
                   "Rotor radius", "Blade width",
                   "Rotation Speed", "Rotation Speed SD", "Blade Pitch", "Blade Pitch SD",
                   paste0(month.abb," wind available"),paste0(month.abb," mean downtime"),
                   paste0(month.abb," SD downtime"))
    
    return(Df)
  })
  BirdsData <- reactive({
    Df <- hot_to_r(input$hotInput_output_bird_scenarios)
    names(Df) <- c("Species",
                   "Flight","Body Length",
                   "Body Length SD", "Wingspan", "Wingspan SD",
                   "Flight Speed", "Flight Speed SD",
                   "Avoidance", "Avoidance SD", "PCH","Biogeographic population",
                   "Proportion in UK", "Total population in UK",
                   "PrBMigration","PoBMigration",
                   "OMigration")
    Df[is.na(Df)] <- "NA"
    return(Df)
  })
  
  ScenariosData <- reactive({
    Df <- hot_to_r(input$hotInput_output_population_scenarios)
    names(Df) <- c("Wind farm","Species", "Population estimate","Population estimate (SD)","Proportion at-risk flight lines")
    return(Df)
  })
  
  
  ## Handler for the modal to input the filename
  observeEvent(input$button_download_scenarios_modal,{
    
    BirdDat <- tryCatch(BirdsData(),
                        error=function(e) NULL)
    TurbineDat <- tryCatch(WindFarmsData(),
                           error=function(e) NULL)
    CountDat <- tryCatch(ScenariosData(),
                         error=function(e) NULL)
    
    check.dat <- TRUE
    if(is.null(BirdDat)|is.null(TurbineDat)|is.null(CountDat)){
      check.dat <- FALSE
    }
    
    if(check.dat == FALSE){
      shinyalert::shinyalert(
        title = "Error",
        text = "Scenarios have not been generated, please click 'Generate Scenarios'",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }else{
      showModal(modalDialog(
        title = "Download model scenario worksheet",
        textInput("txtInput_dwnld_scenario_name",label="Input filename for download (e.g., model_scenario_1)"),
        downloadButton("button_download_scenarios","Download worksheet"),
        easyClose=TRUE
      ))
    }
  })
  
  
  ## The download handler for the scenarios
  output$button_download_scenarios <- downloadHandler(
    
    filename = function() {
      paste(input$txtInput_dwnld_scenario_name,".xlsx", sep = "")
    },
    content = function(file) {
        wb <- openxlsx::createWorkbook()
        BirdDat <- tryCatch(BirdsData(),
                            error=function(e) NULL)
        TurbineDat <- tryCatch(WindFarmsData(),
                               error=function(e) NULL)
        CountDat <- tryCatch(ScenariosData(),
                             error=function(e) NULL)
        
        openxlsx::addWorksheet(wb,sheetName="BirdData")
        openxlsx::addWorksheet(wb,sheetName="TurbineData")
        openxlsx::addWorksheet(wb,sheetName="CountData")
        
        openxlsx::writeData(wb,sheet="BirdData",BirdDat)
        openxlsx::writeData(wb,sheet="TurbineData",TurbineDat)
        openxlsx::writeData(wb,sheet="CountData",CountDat)
        openxlsx::saveWorkbook(wb,file,overwrite=TRUE)
      }
  )

  

# Upload scenario handlers  -----------------------------------------------

  ## Function for the upload scenarios modal
  observeEvent(input$button_upload_scenarios_modal,{
    showModal(modalDialog(
      title = "Upload model scenario",
      fileInput("worksheet", "Choose XLSX File", accept = ".xlsx"),
      actionButton("button_upload_scenarios","Upload Scenarios", class="btn btn-primary"),
      easyClose=TRUE
    ))
  })
  ## Function for opening the data file - will also do data checks
  observeEvent(input$button_upload_scenarios, {
    dat <- input$worksheet
    ext <- tools::file_ext(dat$datapath)
    req(dat)
    
    ### Check extension to make sure an xlsx is loaded
    if(ext != "xlsx"){
      shinyalert::shinyalert(
        title = "Error",
        text = "Data must be an XLSX",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      validate(need(ext=="xlsx","Please upload an xlsx"))
    }
    #browser()
    
    list_all <- lapply(readxl::excel_sheets(dat$datapath),function(x){
      as.data.frame(readxl::read_xlsx(dat$datapath,sheet=x))
    })
    
    ### Check Bird parameter table names
    BDnames <- c("Species",
                 "Flight","Body Length",
                 "Body Length SD", "Wingspan", "Wingspan SD",
                 "Flight Speed", "Flight Speed SD",
                 "Avoidance", "Avoidance SD", "PCH","Biogeographic population",
                 "Proportion in UK", "Total population in UK",
                 "PrBMigration","PoBMigration",
                 "OMigration")
    if(!identical(names(list_all[[1]]),BDnames)){
      shinyalert::shinyalert(
        title = "Error",
        text = "Bird parameter sheet names are not correct. Please check documentation and try again.\
        TIP: Check capitalization and spacing in your column names. Best practice is to download the worksheet \
        and then fill it in without changing column names. You could also download the worksheet then copy and paste \
        column names from that worksheet into your own.",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      validate(need(identical(names(list_all[[1]]),BDnames),"BD names not identical"))
    }
    
    ### Check Wind Farm table names
    WFnames <- c("Wind farm","Latitude","Width","Proportion upwind flight",
                 "Number of turbines","Number of blades",
                 "Rotor radius", "Blade width",
                 "Rotation Speed", "Rotation Speed SD", "Blade Pitch", "Blade Pitch SD",
                 paste0(month.abb," wind available"),paste0(month.abb," mean downtime"),
                 paste0(month.abb," SD downtime"))
    
    if(!identical(names(list_all[[2]]),WFnames)){
      shinyalert::shinyalert(
        title = "Error",
        text = "Wind farm parameter sheet names are not correct. Please check documentation and try again.\
        TIP: Check capitalization and spacing in your column names. Best practice is to download the worksheet \
        and then fill it in without changing column names. You could also download the worksheet then copy and paste \
        column names from that worksheet into your own.",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      validate(need(identical(names(list_all[[2]]),WFnames),"WF names not identical"))
    }
    
    ### Check names on count data / population estimates
    CTnames <- c("Wind farm","Species", "Population estimate","Population estimate (SD)","Proportion at-risk flight lines")
    if(!identical(names(list_all[[3]]),CTnames)){
      shinyalert::shinyalert(
        title = "Error",
        text = "Count/Population parameter sheet names are not correct. Please check documentation and try again.\
        TIP: Check capitalization and spacing in your column names. Best practice is to download the worksheet \
        and then fill it in without changing column names. You could also download the worksheet then copy and paste \
        column names from that worksheet into your own.",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      validate(need(identical(names(list_all[[3]]),CTnames),"CT names not identical"))
    }
    
    ## If the validation passes, load the data
    
    output$hotInput_output_bird_scenarios <- renderRHandsontable(
      list_all[[1]] %>%
        rhandsontable(selectCallback = TRUE,rowHeaders=NULL, colHeaders = c("Species",  
                                                                            "Flight","Body Length",
                                                                            "Body Length SD", "Wingspan", "Wingspan SD",
                                                                            "Flight speed", "Flight speed SD",
                                                                            "Avoidance", "Avoidance SD", "PCH","Biogeographic population",
                                                                            "Proportion in UK", "Total population in UK", 
                                                                            "Pre-breeding migration","Post-breeding migration",
                                                                            "Other migration")) %>%
        hot_cols() %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    )
    
    
    output$hotInput_output_wf_scenarios <- renderRHandsontable(
      list_all[[2]] %>%
        rhandsontable(selectCallback = TRUE,rowHeaders=NULL, colHeaders = c("Wind farm","Latitude","Width","Proportion upwind flight",
                                                                            "Number of turbines","Number of blades",
                                                                            "Rotor radius", "Blade width",
                                                                            "Rotation Speed", "Rotation Speed SD", "Blade Pitch", "Blade Pitch SD",
                                                                            paste0(month.abb," wind available"),paste0(month.abb," mean downtime"),
                                                                            paste0(month.abb," SD downtime"))) %>%
        hot_cols() %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    )
    
    
    output$hotInput_output_population_scenarios <- renderRHandsontable(
      list_all[[3]] %>%
        rhandsontable(selectCallback = TRUE,rowHeaders=NULL, colHeaders = c("Wind farm","Species", "Population estimate","Population estimate (SD)")) %>%
        hot_cols() %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    )
  })
  


   # Download worksheet ------------------------------------------------------

  output$button_download_blank_worksheet <- downloadHandler(
    
    filename = function() {
      paste(input$txtInput_dwnld_scenario_name,".xlsx", sep = "")
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      
      TurbineDat <- data.frame(matrix(nrow=1,ncol=48))
      names(TurbineDat) <- c("Wind farm","Latitude","Width","Proportion upwind flight",
                      "Number of turbines","Number of blades",
                      "Rotor radius", "Blade width",
                      "Rotation Speed", "Rotation Speed SD", "Blade Pitch", "Blade Pitch SD",
                      paste0(month.abb," wind available"),paste0(month.abb," mean downtime"),
                      paste0(month.abb," SD downtime"))
      
      BirdDat <- data.frame(matrix(nrow=1,ncol=17))
      names(BirdDat) <- c("Species",
                      "Flight","Body Length",
                      "Body Length SD", "Wingspan", "Wingspan SD",
                      "Flight Speed", "Flight Speed SD",
                      "Avoidance", "Avoidance SD", "PCH","Biogeographic population",
                      "Proportion in UK", "Total population in UK",
                      "PrBMigration","PoBMigration",
                      "OMigration")
      
      CountDat <- data.frame(matrix(nrow=1,ncol=4))
      names(CountDat) <- c("Wind farm","Species", "Population estimate","Population estimate (SD)")
      
      openxlsx::addWorksheet(wb,sheetName="BirdData")
      openxlsx::addWorksheet(wb,sheetName="TurbineData")
      openxlsx::addWorksheet(wb,sheetName="CountData")
      
      openxlsx::writeData(wb,sheet="BirdData",BirdDat)
      openxlsx::writeData(wb,sheet="TurbineData",TurbineDat)
      openxlsx::writeData(wb,sheet="CountData",CountDat)
      openxlsx::saveWorkbook(wb,file,overwrite=TRUE)
    }
  )
  
  
  
  
  # Create reactive values list for storing model output --------------------

  mcrmOut <- reactiveValues(
    mCRM_output_ls = NULL,
    mCRM_boots_ls = NULL
  )
  
  
  # Run compiled simulation -------------------------------------------------
  
  observeEvent(input$actButtonInput_simulPars_GO,{
    
    #input$actButtonInput_simulPars_GO
      
    BirdDat <- tryCatch(BirdsData(),
                        error=function(e) NULL)
    TurbineDat <- tryCatch(WindFarmsData(),
                           error=function(e) NULL)
    CountDat <- tryCatch(ScenariosData(),
                         error=function(e) NULL)
    
    check.dat <- TRUE
    if(is.null(BirdDat)|is.null(TurbineDat)|is.null(CountDat)){
      check.dat <- FALSE
    }
    
    if(check.dat == FALSE){
      shinyalert::shinyalert(
        title = "Error",
        text = "Scenarios have not been generated, please return to Step 3 and click 'Generate Scenarios'",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }else{
      
      #output$cons <- renderUI({
      
      names(BirdDat) <- stringr::str_replace_all(names(BirdDat)," ","")
      names(TurbineDat) <- stringr::str_replace_all(names(TurbineDat)," ","")
      names(CountDat) <- stringr::str_replace_all(names(CountDat)," ","")
      
      outputs <- matrix(nrow=nrow(CountDat),ncol=11)
      
      withProgress(message = "Running mCRM",value=0,{
        for(i in 1:nrow(CountDat)){
          incProgress(1/nrow(CountDat),detail=paste0("Running scenario ", i,"/",nrow(CountDat) ))
          spp_name <- as.character(CountDat$Species[i])
          wf_name <- as.character(CountDat$Windfarm[i])
          BirdData <- BirdDat %>% dplyr::filter(Species == spp_name)
          TurbineData <- TurbineDat %>% dplyr::filter(Windfarm == wf_name)
          CountData <- CountDat[i,]
          ### Split the months to get start and end months for the season_specs table
          ssPrB <- strsplit(BirdData$PrBMigration," - ")[[1]]
          if(length(ssPrB)>1){
            PrBSt <- ssPrB[1]
            PrBEn <- ssPrB[2]
          }else{
            PrBSt <- NA
            PrBEn <- NA
          }
          ssPoB <- strsplit(BirdData$PoBMigration," - ")[[1]]
          if(length(ssPoB)>1){
            PoBSt <- ssPoB[1]
            PoBEn <- ssPoB[2]
          }else{
            PoBSt <- NA
            PoBEn <- NA
          }
          ssO <- strsplit(BirdData$OMigration," - ")[[1]]
          if(length(ssO)>1){
            OSt <- ssO[1]
            OEn <- ssO[2]
          }else{
            OSt <- NA
            OEn <- NA
          }
          season_specs <- data.frame(
            season_id = c("PrBMigration", "PoBMigration", "OMigration"),
            start_month = c(PrBSt, PoBSt, OSt), end_month = c(PrBEn, PoBEn, OEn)
          )
          
          ## Create wind availability table
          windavb <- data.frame(reshape2::melt(TurbineData %>% select(Janwindavailable:Decwindavailable)))
          names(windavb) <- c("month","pctg")
          windavb$month <- month.abb
          
          DTmn <- reshape2::melt(TurbineData %>% select(Janmeandowntime:Decmeandowntime)) %>% mutate(variable=month.abb)
          DTsd <- reshape2::melt(TurbineData %>% select(JanSDdowntime:DecSDdowntime)) %>% mutate(variable=month.abb)
          dwntm <- DTmn %>% left_join(DTsd,by="variable")
          names(dwntm) <- c("month","mean","sd")
          
          
          #browser()
          ### Make use of built in error handling in the stochLAB package
          ### Will output console errors as a notification
            tryCatch({
              outs <- mig_stoch_crm(
                wing_span_pars = data.frame(mean = BirdData$Wingspan, sd = BirdData$WingspanSD),      # Wing span in m,
                flt_speed_pars = data.frame(mean = BirdData$FlightSpeed, sd = BirdData$FlightSpeedSD),       # Flight speed in m/s
                body_lt_pars = data.frame(mean = BirdData$BodyLength, sd = BirdData$BodyLengthSD),       # Body length in m,
                prop_crh_pars = data.frame(mean = BirdData$PCH, sd = 0),                              # Proportion of birds at CRH
                avoid_bsc_pars = data.frame(mean = BirdData$Avoidance, sd = BirdData$AvoidanceSD),     # avoidance rate
                n_turbines = TurbineData$Numberofturbines,
                n_blades = TurbineData$Numberofblades,
                rtn_speed_pars = data.frame(mean = TurbineData$RotationSpeed, sd = TurbineData$RotationSpeedSD),         # rotation speed in m/s of turbine blades
                bld_pitch_pars = data.frame(mean = TurbineData$BladePitch, sd = TurbineData$BladePitchSD),          # pitch in degrees of turbine blades
                rtr_radius_pars = data.frame(mean = TurbineData$Rotorradius, sd = 0),          # sd = 0, rotor radius is fixed
                bld_width_pars = data.frame(mean = TurbineData$Bladewidth, sd = 0),            # sd = 0, blade width is fixed
                wf_width = TurbineData$Width,
                wf_latitude = TurbineData$Latitude,
                prop_upwind = TurbineData$Proportionupwindflight/100,
                flight_type = tolower(BirdData$Flight),
                popn_estim_pars = data.frame(mean = CountData$Populationestimate, sd = CountData$`Populationestimate(SD)`),    # population flying through windfarm,
                season_specs = season_specs,
                chord_profile = stochLAB::chord_prof_5MW,
                trb_wind_avbl = windavb,
                trb_downtime_pars = dwntm,
                n_iter = input$sldInput_simulPars_numIter,
                LargeArrayCorrection = TRUE,
                log_file = NULL,
                seed = 1234,
                verbose = FALSE)
              
              ## Send outputs to reactive Values list so they can be accessed
              mcrmOut$mCRM_boots_ls[[wf_name]][[spp_name]] <- outs
              outs$collisions <- data.frame(outs$collisions)
              ## Send outputs to matrix
              outputs[i,1] <- spp_name
              outputs[i,2] <- wf_name
              outputs[i,3] <- paste(round(mean(outs$collisions[,1],na.rm=T),3), "\u00B1", round(sd(outs$collisions[,1],na.rm=T),3))
              outputs[i,4] <- paste(round(mean(outs$collisions[,2],na.rm=T),3), "\u00B1", round(sd(outs$collisions[,2],na.rm=T),3))
              outputs[i,5] <- paste(round(mean(outs$collisions[,3],na.rm=T),3), "\u00B1", round(sd(outs$collisions[,3],na.rm=T),3))
              ## Set raw values to matrix as well so they can be used for cumulative assessments
              outputs[i,6] <- round(mean(outs$collisions[,1],na.rm=T),3)
              outputs[i,7] <- round(sd(outs$collisions[,1],na.rm=T),3)  
              outputs[i,8] <- round(mean(outs$collisions[,2],na.rm=T),3) 
              outputs[i,9] <- round(sd(outs$collisions[,2],na.rm=T),3) 
              outputs[i,10] <- round(mean(outs$collisions[,3],na.rm=T),3)
              outputs[i,11] <- round(sd(outs$collisions[,3],na.rm=T),3)
              
            },
            #warning = function(warn){
            #  mess <- paste0("warning in scenario ",spp_name," x ",wf_name,": ",warn$message)
            #  showNotification(mess,type='warning',duration = NULL)
            #},
            error = function(err){
              mess <- paste0("error in scenario ",spp_name," x ",wf_name, ": ",err$message)
              showNotification(mess,type='err',duration = NULL)
            }
            )
        }
      })
      try({
        outputs <- data.frame(outputs)
        names(outputs)[1:5] <- c('Species',"windfarm","PrBMigration","PoBMigration","OMigration")
        
        PreBreedout <- reshape2::dcast(outputs[,c(1:3)],formula = Species ~windfarm)
        PostBreedout <- reshape2::dcast(outputs[,c(1,2,4)],formula = Species ~windfarm)
        Otherout <- reshape2::dcast(outputs[,c(1,2,5)],formula = Species ~windfarm)
        
        ## Create summary table
        cumulTab <- outputs %>%
          group_by(Species) %>%
          dplyr::summarise(PrBsum = sum(as.numeric(X6),na.rm=TRUE),
                           PrBsd = sum_stdevs(as.numeric(X7)),
                           PoBsum = sum(as.numeric(X8),na.rm=TRUE),
                           PoBsd = sum_stdevs(as.numeric(X9)),
                           Osum = sum(as.numeric(X10),na.rm=TRUE),
                           Osd = sum_stdevs(as.numeric(X11))) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            'Pre-breeding total' = paste(PrBsum, "\u00B1", round(PrBsd,3)),
            'Post-breeding total' = paste(PoBsum, "\u00B1", round(PoBsd,3)),
            'Other total' = paste(Osum, "\u00B1", round(Osd,3)),
            'Total' = paste(sum(dplyr::c_across(c(PrBsum,PoBsum,Osum))),"\u00B1",
                            round(sum_stdevs(dplyr::c_across(c(PrBsd,PoBsd,Osd))),3))
          ) %>%
          dplyr::select(-PrBsum,-PrBsd,-PoBsum,-PoBsd,-Osum,-Osd)
        
        
        mcrmOut$mCRM_output_ls[['PreBreedout']] <- PreBreedout
        mcrmOut$mCRM_output_ls[['PostBreedout']] <- PostBreedout
        mcrmOut$mCRM_output_ls[['Otherout']] <- Otherout
        mcrmOut$mCRM_output_ls[['cumulTab']] <- cumulTab
        
        ### Renders the UI for the results 
        output$summTables <- renderUI({
          lapply(WFshapelist$wfs,
                 function(x){
                   titlenm <- paste0(x," Outputs")
                   summtab <- stringr::str_replace_all(x,pattern=" ",replacement="_")
                   dtName <- paste0("summTable_",summtab)
                   box(title = titlenm, width = 12, status = "primary", solidHeader = TRUE,
                       DT::DTOutput(dtName)
                   )
                 })
        })
        ### formats the data for each wind farm and renders it to the appropriate data tables
        lapply(WFshapelist$wfs,
               function(x){
                 summtab <- stringr::str_replace_all(x,pattern=" ",replacement="_")
                 dtName <- paste0("summTable_",summtab)
                 PreB <- mcrmOut$mCRM_output_ls[['PreBreedout']] %>% dplyr::select(Species,contains(x))
                 PosB <- mcrmOut$mCRM_output_ls[['PostBreedout']] %>% dplyr::select(Species,contains(x))
                 OthM <- mcrmOut$mCRM_output_ls[['Otherout']] %>% dplyr::select(Species,contains(x))
                 Final <- tibble(Species = PreB$Species,
                                 "Pre-Breeding" = PreB[,2],
                                 "Post-Breeding" = PosB[,2],
                                 "Other migration" = OthM[,2]
                 )
                 
                 output[[dtName]]<- DT::renderDT({
                   datatable(Final,rownames=FALSE, extensions="Buttons",
                             options=list(buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
                 })
               })
        output$summTable_cumulative <- DT::renderDT({
          datatable(cumulTab,rownames=FALSE,extensions="Buttons",
                    options=list(buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
        })
        
        
        output$Report_Download <- renderUI({
          tagList(
            fluidRow(
              column(12,
                     shinyWidgets::downloadBttn(
                       "dwnld_Report",
                       label = "Generate report",
                       color = "primary",
                       style = "bordered",
                       size = "sm",
                       block = TRUE
                     ) %>%
                       bsplus::bs_embed_tooltip(
                         title = "Generate report", 
                         placement = "bottom")
              ),style="margin-bottom:10px"
            )
            )
            
        }) ## End output$Simulation_Download
        
        output$Download_Tables <- renderUI({
          tagList(
            fluidRow(
              column(12,
                     shinyWidgets::downloadBttn(
                       "dwnld_Tables",
                       label = "Download tables",
                       color = "primary",
                       style = "bordered",
                       size = "sm",
                       block = TRUE
                     ) %>%
                       bsplus::bs_embed_tooltip(
                         title = "Download output tables", 
                         placement = "bottom")
              )
            )
            )
          
        }) ## End output$Download_Tables

      }) ## End try
    } ## End if/else
  })  ## End observeEvent(input$actButtonInput_simulPars_GO) 
  
  
  
  # ----------------------------------------------------------------
  #  ----              Download model outputs                    ----
  # ----------------------------------------------------------------

  output$dwnld_Report <- downloadHandler(
    filename = "modelOut.docx",
    content = function(file) {
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Report.Rmd")
      file.copy("Report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      #testdf <- data.frame(col1=c(1,2,3,4,5),col2=c(2,3,4,5,6))
      #browser()
      params <- list(prebreedtable = mcrmOut$mCRM_output_ls[['PreBreedout']],
                     postbreedtable = mcrmOut$mCRM_output_ls[['PostBreedout']],
                     othertable = mcrmOut$mCRM_output_ls[['Otherout']],
                     cumultable = mcrmOut$mCRM_output_ls[['cumulTab']],
                     bootdata = mcrmOut$mCRM_boots_ls)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    },
  )
  
  output$dwnld_Tables <- downloadHandler(
    filename = "mCRM_Output_tables.xlsx",
    content = function(file) {
      
      #browser()
      params <- list(prebreedtable = mcrmOut$mCRM_output_ls[['PreBreedout']],
                     postbreedtable = mcrmOut$mCRM_output_ls[['PostBreedout']],
                     othertable = mcrmOut$mCRM_output_ls[['Otherout']],
                     cumultable = mcrmOut$mCRM_output_ls[['cumulTab']],
                     bootdata = mcrmOut$mCRM_boots_ls)
      
      wb <- openxlsx::createWorkbook()
      BirdDat <- tryCatch(BirdsData(),
                          error=function(e) NULL)
      TurbineDat <- tryCatch(WindFarmsData(),
                             error=function(e) NULL)
      CountDat <- tryCatch(ScenariosData(),
                           error=function(e) NULL)
      
      openxlsx::addWorksheet(wb,sheetName="BirdData_Input")
      openxlsx::addWorksheet(wb,sheetName="TurbineData_Input")
      openxlsx::addWorksheet(wb,sheetName="CountData_Input")
      openxlsx::addWorksheet(wb,sheetName="Pre_Breeding_Output")
      openxlsx::addWorksheet(wb,sheetName="Post_Breeding_Output")
      openxlsx::addWorksheet(wb,sheetName="Other_Migration_Output")
      openxlsx::addWorksheet(wb,sheetName="Cumulative_Output")
      
      openxlsx::writeData(wb,sheet="BirdData_Input",BirdDat)
      openxlsx::writeData(wb,sheet="TurbineData_Input",TurbineDat)
      openxlsx::writeData(wb,sheet="CountData_Input",CountDat)
      openxlsx::writeData(wb,sheet="Pre_Breeding_Output",params$prebreedtable)
      openxlsx::writeData(wb,sheet="Post_Breeding_Output",params$postbreedtable)
      openxlsx::writeData(wb,sheet="Other_Migration_Output",params$othertable)
      openxlsx::writeData(wb,sheet="Cumulative_Output",params$cumultable)
      
      lapply(names(params$bootdata),function(x){
        shortnm <- substr(x,1,25)
        openxlsx::addWorksheet(wb,sheetName=paste0(shortnm,"_Boot"))
        datf <- plyr::ldply(names(params$bootdata[[x]]),function(y){
          df <- data.frame(params$bootdata[[x]][[y]])
          df$Species <- y
          return(df)
        })
        openxlsx::writeData(wb,sheet=paste0(shortnm,"_Boot"),datf)
      })
      
      
      
      openxlsx::saveWorkbook(wb,file,overwrite=TRUE)
    },
  )
  
  

# Controls for uploading windfarm shapefiles ------------------------------
  
  observe({
    WF_shape_choice <- WF_shape_choice()
    if(WF_shape_choice == "existWindFarms"){
      output$Windfarm_Shapes <- renderUI({
        selectizeInput("selectInput_builtin_wfList",
                       label = "Select wind farms",
                       choices = Scotwind_Merged$NAME[order(Scotwind_Merged$NAME)],
                       options = list(maxItems = 50L)
        )      
      })
    }else if(WF_shape_choice == "customWindFarms"){
      output$Windfarm_Shapes <- renderUI({
        tagList(
          shiny::fileInput("custom_WF_shapes", "Choose polygon shape file (ensure a NAME field exists, select all files)",
                           multiple = TRUE, accept=c(".shp",".dbf",".sbn",".sbx",".shx",".prj")),
          
          shinyWidgets::actionBttn(
            "btn_load_custom_WF_shape",
            label = "Load shapefile",
            icon = icon("cog"),
            style="stretch",
            color="danger",
            no_outline=FALSE
          )%>%
            bsplus::bs_embed_tooltip(
              title = "Select ALL files associated with the shapefile,
              create a column called 'NAME' with the name of the wind farm.", 
              placement = "bottom"),
          uiOutput("selectInput_custom_Windfarm_name_header")
        )
        })
    }
    
  })
  
  ### Some code borrowed from:  https://github.com/richpauloo/shp_oswcr/blob/master/mod_shpPoly.R
  
  userShp <- reactive({
    input$custom_WF_shapes
  })
  
  ### Load shapes button for custom wind farm polygons
  observeEvent(input$btn_load_custom_WF_shape, {
    
    req(input$custom_WF_shapes)
    infiles <- userShp()$datapath
    dirn <- unique(dirname(infiles))
    outfiles <- file.path(dirn, userShp()$name)
    purrr::walk2(infiles, outfiles, ~file.rename(.x, .y))
    shpnm <- outfiles[grep(userShp()$name,pattern=".shp$")]
    
    tryCatch({
      x <- validate_shape(shpnm,rv)
    },
    error = function(err){
      shinyalert::shinyalert(
        title = "Error",
        text = err$message,  
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
    )
    
  })
  
  
  ######## Validate custom shape function ####
  validate_shape <- function(shpnm,rv){
    tryCatch({
      x <- raster::shapefile(shpnm)
    },
    error = function(err){
      stop(paste0("Error opening shapefile: ",err$message,". Check to ensure
                      all files associated with the .shp have been selected. 
                      E.G., the *.dbf, *.shp, *.shx, and *.prj files at minimum"))
    })
    
    if("NAME" %!in% names(x)){
      stop("'NAME' column is not available in the shapefile, please ensure unique names are specified in a NAME field")
    }else{
      if(is.na(sp::proj4string(x))){
        valid_proj <- FALSE
      } else {
        valid_proj <- TRUE
      }
      if(valid_proj){
        x <- sp::spTransform(x,sf::st_crs(4326)$proj4string)
        output$selectInput_custom_Windfarm_name_header <- renderUI({
          selectizeInput("selectInput_custom_wf_header",
                         label = "Select wind farms",
                         choices = x$NAME,
                         options = list(maxItems = 20L)
          )
        })
        rv$customWFshapes <<- x
        
        return(x)
      }else{
        stop("Shapefile does not have a valid projection")
      }
    }
  }
  
  
  
  
  
  
  
  
  
  
  
  
  



}

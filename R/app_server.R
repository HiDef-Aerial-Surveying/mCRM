#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyBS
#' @import shinyWidgets
#' @import dplyr
#' @import stringr
#' @import leaflet
#' @import tidyr
#' @import tibble
#' @import foreach
#' @noRd
#' 
app_server <- function( input, output, session ) {

  #' ----------------------------------------------------
  #  ----         setting session specifics          ----
  #' ----------------------------------------------------
  
  # --- session's "global" Variables
  #load("data/startUpValues.rda")
  
  prevSlctSpecs <- c()
  inputs_BiomParsPlotted <-  NULL
  birdDensPars_plotted <- list(NULL)
  flgtHghDstInputs_ls_Latest <- list(NULL)
  storedSlctSpecLabel <- NULL
  
  
  # --- Initiate session's reactive variables
  rv <- reactiveValues(
    WFsSelected = NULL,
    addedSpec = NULL,
    biomParsInputs_ls = NULL,
    birdDensParsInputs_ls = NULL,
    summaryTables_ls = NULL,
    sCRM_output_ls = NULL,
    turbinePars_monthOps_df = data.frame(
      matrix(
        c(startUpValues$turbinePars$windAvail, startUpValues$turbinePars$meanDownTime, startUpValues$turbinePars$sdDownTime),
        nrow = 3, ncol = 12, byrow = TRUE, 
        dimnames = list(c("Wind Availability (%)", "Mean Downtime (%)", "SD Downtime (%)"), month.name)
      ),
      stringsAsFactors = FALSE
    ),
    pitchVsWind_df = startUpValues$turbinePars$pitchVsWind_df,
    rotationVsWind_df = startUpValues$turbinePars$rotationVsWind_df,
    startSpecMonthDens_df = map(startUpValues$speciesPars, ~data.frame(meanDensity = .$"meanDensity", sdDensity = .$"sdDensity")),
    restoringChain =FALSE, restoringChain_2 = FALSE,
    simCodeTrigger = 0, 
    birdata_model = data.frame(), monthDensData_model = data.frame(), 
    monthDensOpt_model = data.frame(), turbineData_model = data.frame(), windfarmData_model = data.frame(),
    densityDataPresent = NULL, NAsFreeData = NULL, FHD_acceptable = NULL, speciesFieldFilled= NULL
  )
  
  
  
  # --- Generate temporary paths and folders for storing session's specific data 
  sessTempOutFolder <- getTempFolderName()
  path2ShinyOut_Inputs <- file.path("shinyOutputs", sessTempOutFolder, "inputs")
  path2ShinyOut_Outputs <- file.path("shinyOutputs", sessTempOutFolder, "outputs")
  path2Outputs_results <- file.path("results", sessTempOutFolder)
  
  dir.create(path2ShinyOut_Inputs, recursive = TRUE)
  dir.create(path2ShinyOut_Outputs, recursive = TRUE)
  dir.create(path2Outputs_results, recursive = TRUE)
  
  
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
    Scotwind_Merged[Scotwind_Merged$NAME %in% input$selectInput_builtin_wfList,]
  })
  
  ## Stores a vector of the windfarm shapes which is used to update the shapefiles visble
  ## In the map after the button has been clicked
  WFshapelist <- reactiveValues(wfs=vector())

  ## When the button has been clicked to update the windfarm tabs, we update the map
  ## As well as update the tab list (appendTab function) and load in the windfarmfeats_ui module
  ## The windfarmfeats server module is also called here
  observeEvent(input$button_update_Windfarm_tabs, {
    cur.popup <- paste0("<strong>Name: </strong>", WFshapes()$NAME)
    leaflet::leafletProxy("map",data=WFshapes()) %>% clearShapes() %>%
      addPolygons(weight = 1, fillColor = "red", popup=cur.popup, fillOpacity = 1) 
    if(length(WFshapelist$wfs) == 0){
      lapply(WFshapes()$NAME,function(x) {
        id_name <- stringr::str_replace_all(x," ","_")
        appendTab("windfarm_Tabs",tabPanel(x,mod_WindFarmFeats_ui(id_name)))
        mod_WindFarmFeats_server(id_name,data=rv)
      })
      WFshapelist$wfs <- WFshapes()$NAME
    }else if(length(WFshapelist$wfs) > 0){
      for(j in WFshapelist$wfs){
        print(j)
        if(j %!in% WFshapes()$NAME){
          removeTab("windfarm_Tabs",j)
        }
      }
      for(i in WFshapes()$NAME){
        if(i %!in% WFshapelist$wfs){
          id_name <- stringr::str_replace_all(i," ","_")
          appendTab("windfarm_Tabs",tabPanel(i,mod_WindFarmFeats_ui(id_name)))
          mod_WindFarmFeats_server(id_name,data  = rv)
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
        
        id_name <- defaultSpeciesValues$Scientific_name[which(defaultSpeciesValues$Common_name == x)] 
        id_name <- stringr::str_replace_all(id_name," ","_")
        
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
          id_name <- defaultSpeciesValues$Scientific_name[which(defaultSpeciesValues$Common_name == i)] 
          id_name <- stringr::str_replace_all(id_name," ","_")
          
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
             
             id_name <- defaultSpeciesValues$Scientific_name[which(defaultSpeciesValues$Common_name == x)]
             id_name <- stringr::str_replace_all(id_name," ","_")
             

             data.frame(
               #species = id_name,
               flying = eval(parse(text=paste0("input$`",id_name,"-slctInput_biomPars_flType_tp`"))),
               bdlenE = eval(parse(text=paste0("input$`numInput_",id_name,"-biomPars_bodyLt_E_`"))),
               bdlenSD = eval(parse(text=paste0("input$`numInput_",id_name,"-biomPars_bodyLt_SD_`"))),
               wnspnE = eval(parse(text=paste0("input$`numInput_",id_name,"-biomPars_wngSpan_E_`"))),
               wnspnSD = eval(parse(text=paste0("input$`numInput_",id_name,"-biomPars_wngSpan_SD_`"))),
               flSpdE = eval(parse(text=paste0("input$`numInput_",id_name,"-biomPars_flSpeed_E_`"))),
               flSpdSD = eval(parse(text=paste0("input$`numInput_",id_name,"-biomPars_flSpeed_SD_`"))),
               AvoidE = eval(parse(text=paste0("input$`numInput_",id_name,"-biomPars_basicAvoid_E_`"))),
               AvoidSD = eval(parse(text=paste0("input$`numInput_",id_name,"-biomPars_basicAvoid_SD_`"))),
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
             #id_name <- species_data$Scientific[which(species_data$Common == x)]
             x <- stringr::str_replace_all(x," ","_")
             tt <- data.frame(
               Latitude = eval(parse(text=paste0("input$`",x,"-numInput_windfarmPars_Latitude`"))),
               wfWidth = eval(parse(text=paste0("input$`",x,"-numInput_windfarmPars_width`"))),
               PropUpwind = eval(parse(text=paste0("input$`",x,"-numInput_windfarmPars_upWindDownWindProp`"))),
               TPower = eval(parse(text=paste0("input$`",x,"-numInput_turbinePars_turbinePower`"))),
               nBlades = eval(parse(text=paste0("input$`",x,"-numInput_turbinePars_numBlades`"))),
               rRadius = eval(parse(text=paste0("input$`",x,"-numInput_turbinePars_rotRadius`"))),
               bWidth = eval(parse(text=paste0("input$`",x,"-numInput_turbinePars_maxBladeWdth`"))),
               RotnSpdE <- eval(parse(text=paste0("input$`numInput_",x,"-turbinePars_rotnSpeed_E_`"))),
               RotnSpdSD <- eval(parse(text=paste0("input$`numInput_",x,"-turbinePars_rotnSpeed_SD_`"))),
               BldPitchE <- eval(parse(text=paste0("input$`numInput_",x,"-turbinePars_bladePitch_E_`"))),
               BldPitchSD <- eval(parse(text=paste0("input$`numInput_",x,"-turbinePars_bladePitch_SD_`")))
             )
             
             winddatatable <- eval(parse(text=paste0(
               "rhandsontable::hot_to_r(input$`",x,"-hotInput_turbinePars_monthOps`)"
             )))
             meanWA <- winddatatable[1,]
             meanDT <- winddatatable[2,]
             names(meanDT) <- paste0(month.abb," mean down time")
             SDDT <- winddatatable[3,]
             names(SDDT) <- paste0(month.abb," SD down time")
             tt <- do.call("cbind",list(tt,meanWA,meanDT,SDDT))
             
             return(tt)
             
           },simplify=FALSE,USE.NAMES=TRUE)
    
  })


  
  observeEvent(input$button_generate_scenarios  ,{
    
    BirdNames <- BirdNames()
    WFNames <- WFshapes()$NAME
    
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
    #browser()
    
    output$hotInput_output_bird_scenarios <- renderRHandsontable(
      
      bird_scenario_table %>%
        rhandsontable(rowHeaders=NULL, colHeaders = c("Species",  #"Windfarm",
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
    
    #browser()
    output$hotInput_output_wf_scenarios <- renderRHandsontable(
      
      wind_scenario_table %>%
        rhandsontable(rowHeaders=NULL, colHeaders = c("Wind farm","Latitude","Width","Proportion upwind flight",
                                                      "Turbine power","Number of blades",
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
    
    
    
    withProgress(message = "Generating population estimates",value=0,{
      for(k in 1:nrow(Population_estimates)){
        print(k)
        incProgress(1/nrow(Population_estimates),detail=paste("running", Population_estimates$Species[k], "in", Population_estimates$`Wind farm`[k] ))
        Specnm <- stringr::str_replace_all(defaultSpeciesValues$Scientific_name[defaultSpeciesValues$Common_name == Population_estimates$Species[k]]," ","_")
        btonm <- stringr::str_replace_all(defaultSpeciesValues$Sp_code[defaultSpeciesValues$Common_name == Population_estimates$Species[k]]," ","_")
        speclines <- eval(parse(text=paste0("all_lines$",btonm)))
        ## Population is multiplied by proportion in UK waters
        popSizemn <- bird.data.rvs()[Population_estimates$Species[k]][[1]]$Totalpop
          #defaultSpeciesValues$biogeographic_pop[defaultSpeciesValues$Common_name == Population_estimates$Species[k]] * defaultSpeciesValues$prop_uk_waters
        #popSizesd <- species_data$PopulationSizeSD[defaultSpeciesValues$Common_name == Population_estimates$Species[k]]
        Estimates <- vector(length=boot.iters)
        
        withProgress(message = "bootstrapping", value=0, {
          for(j in 1:boot.iters){
            print(j)
            incProgress(1/boot.iters)
            PopVal <- popSizemn
            Estimates[j] <- ceiling(length(GetSampleProp(speclines,samplesize,WFshapes[Population_estimates$`Wind farm`[k],]))/samplesize * PopVal)
            
          }
        })
        Population_estimates$Estimate[k] <- ceiling(mean(Estimates,na.rm=TRUE))
        Population_estimates$EstimateSD[k] <- ceiling(sd(Estimates,na.rm=TRUE))
        
      }
    })
    
    
    
    output$hotInput_output_population_scenarios <- renderRHandsontable(
      
      Population_estimates %>%
        rhandsontable(rowHeaders=NULL, colHeaders = c("Wind farm","Species", "Population estimate","Population estimate (SD)")) %>%
        hot_cols() %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
      
    )
    
    
  })
  
  

  # --- render UI section for model outputs dynamically, according to the selected species.
  output$simResults_UI <- renderUI({
    
    req(rv$sCRM_output_ls)
    
    modelOutputs <- rv$sCRM_output_ls
    
    cSelSpec <- isolate(slctSpeciesTags())
    
    tabs <- map2(cSelSpec$species, cSelSpec$specLabel, results_tabPanelsBuilder)
    
    tagList(
      column(2, align = "right", offset = 10, 
             tipify(downloadButton("dwnld_ModelOuts", "Download Outputs"), 
                    title = "Download zip file with plots and tables presented below", 
                    placement = "left", trigger = "hover", options = list(container = "body"))),
      br(),
      br(),
      invoke(tabBox, tabs,  width = 12) #, height = "250px")
    )
  })
  
  
  
  
  
  
  # --- Manages downloads of data templates
  observeEvent(rv$addedSpec, {
    
    walk(rv$addedSpec, function(x){
      
      specLabel <- slctSpeciesTags() %>% dplyr::filter(species == x) %>% dplyr::pull(specLabel) #gsub(" ", "_", x)
      
      # FHD boostrap data
      downlTag_FHD <- paste0("dwnld_template_FHD_", specLabel)
      
      output[[downlTag_FHD]] <- downloadHandler(
        filename = function() {
          "FHD_bootstrapData_template.csv"
        },
        content = function(file) {
          write.csv(template_FHD, file, row.names = FALSE)
        }
      )
      
      
      # Monthly bird densities reference points stats
      downlTag_monthDens_summ <- paste0("dwnld_template_monthDens_summaries_", specLabel)
      
      output[[downlTag_monthDens_summ]] <- downloadHandler(
        filename = function() {
          "monthDensities_distRefPoints_template.csv"
        },
        content = function(file) {
          write.csv(template_monthDens_summaries, file, row.names = FALSE)
        }
      )
      
      
      # Monthly bird densities 
      downlTag_monthDens_samp <- paste0("dwnld_template_monthDens_samples_", specLabel)
      
      output[[downlTag_monthDens_samp]] <- downloadHandler(
        filename = function() {
          "monthDensities_distSamples_template.csv"
        },
        content = function(file) {
          write.csv(template_monthDens_samples, file, row.names = FALSE)
        }
      )
    })
  })
  
  
  
  #' -----------------------------------------------------------------
  #  ----                  Outputs Management                     ----
  #' -----------------------------------------------------------------
  
  # --- Generates plots for biometric variables based on input values of their hyperparameters
  observeEvent(rv$biomParsInputs_ls, {
    
    req(length(rv$biomParsInputs_ls)>0)
    
    inputs_currBiomPars_df <- rv$biomParsInputs_ls %>%
      ldply(function(x){data.frame(Value = as.character(x))}, .id = "inputTags") %>%
      mutate(inputTags_split = stringr::str_split(inputTags, "_")) %>%
      mutate(specLabel = map_chr(inputTags_split, function(x) paste(x[-c(1:4)], collapse = "_")),
             par = map_chr(inputTags_split, function(x) x[3]),
             hyper = map_chr(inputTags_split, function(x) x[4]),
             par_hyper = paste(par, hyper, sep = "_"), 
             parName = case_when(
               par == "bodyLt" ~ "Body length (m)",
               par == "wngSpan" ~ "Wing Span (m)",
               par == "flSpeed" ~ "Flight Speed (m/s)",
               par == "noctAct" ~ "Nocturnal Activity (proportion)",
               par == "basicAvoid" ~ "Basic avoidance (probability)",
               par == "extAvoid" ~ "Extended avoidance (probability)",
               par == "CRHeight" ~ "CRH (proportion)"
             ))
    
    inputs_currBiomParsPlottable <- inputs_currBiomPars_df %>%
      dplyr::filter(hyper %in% c("E", "SD")) %>%
      dplyr::select(-inputTags_split) %>%
      dplyr::mutate(plotTag = paste0("plot_biomPars_", par, "_", specLabel),
             qtTag =  paste0("qtls_biomPars_", par, "_", specLabel),
             Value = as.numeric(as.character(Value)))
    
    if(!is.null(inputs_BiomParsPlotted)){
      inputs_currBiomParsPlottable %<>% dplyr::mutate(inputTags = as.character(inputTags))
      inputs_BiomParsPlotted %<>% dplyr::mutate(inputTags = as.character(inputTags))
      inputs_BiomParsChanged <- setdiff(inputs_currBiomParsPlottable, inputs_BiomParsPlotted)
      inputs_BiomParsToPlot <- inputs_currBiomParsPlottable %>% dplyr::filter(plotTag %in% inputs_BiomParsChanged$plotTag)
      # print(inputs_BiomParsChanged)
    }else{
      inputs_BiomParsToPlot <- inputs_currBiomParsPlottable
    }
    
    #print(inputs_BiomParsToPlot)
    
    plotTagsToPlot <- unique(inputs_BiomParsToPlot$plotTag)
    
    if(length(plotTagsToPlot)>0){
      for(i in 1:length(plotTagsToPlot)){
        local({
          c_tag <- plotTagsToPlot[i] 
          cPlotData <- inputs_BiomParsToPlot %>% dplyr::filter(plotTag == c_tag)
          c_qtTag <- unique(cPlotData$qtTag)
          c_par <- unique(cPlotData$par)
          
          mu <- as.numeric(as.character(dplyr::filter(cPlotData, hyper == "E")$Value))
          stdev <- as.numeric(as.character(dplyr::filter(cPlotData, hyper == "SD")$Value))
          
          output[[c_tag]] <- renderPlot({
            #print(c_tag)
            if(c_par %in% c("bodyLt", "wngSpan", "flSpeed")){
              truncNormPars_densPlots(mu = mu, stdev = stdev, lower = 0, fill="darkorange", xlab = unique(cPlotData$parName))
            }else{
              if(c_par %in% c("noctAct", "basicAvoid", "extAvoid", "CRHeight")){
                betaInputPars_densPlots(p = mu, stdev = stdev, fill="darkorange", xlab = unique(cPlotData$parName))
              }else{
                normDens_ParsPlots(mu = mu, stdev = stdev, fill="darkorange", xlab = unique(cPlotData$parName), refValue_SD = stdev*1.5)  
              }
            }
          })
          
          output[[c_qtTag]] <- renderPrint({
            if(c_par %in% c("bodyLt", "wngSpan", "flSpeed")){
              truncNormPars_qtlTbl(mu = mu, stdev = stdev, lower = 0, varTag = c_par, decPlaces = 4) 
            }else{
              if(c_par %in% c("noctAct", "basicAvoid", "extAvoid", "CRHeight")){
                betaInputPars_qtlTbl(p = mu, stdev = stdev, varTag = c_par, decPlaces = 4)
              }else{
                normDens_ParsQtls(mu = mu, stdev = stdev, varTag = c_par, decPlaces = 4) 
              }
            }
          })
        })
      }
      # update values of currently plotted biometric parameters
      inputs_BiomParsPlotted <<- inputs_currBiomParsPlottable
    }
  })
  
  
  

  
  ## --- Generates plots for Turbine parameters
  observe({
    mu <- input$numInput_turbinePars_rotnSpeed_E_
    stdev <- input$numInput_turbinePars_rotnSpeed_SD_
    
    output$plot_turbinePars_rotnSpeed <- renderPlot({
      
      #sdRef <- ifelse(startUpValues$rotnSpeed_SD/stdev < 0.25, startUpValues$rotnSpeed_SD*10, startUpValues$rotnSpeed_SD)
      truncNormPars_densPlots(mu = mu, stdev = stdev, lower = 0, fill = "olivedrab", xlab = "Rotation Speed (rpm)")
    })
    
    output$qtls_turbinePars_rotnSpeed <- renderPrint({
      truncNormPars_qtlTbl(mu = mu, stdev = stdev, lower = 0, varTag = "RotnSpeed")
    })
  })
  
  
  observe({
    mu <- input$numInput_turbinePars_bladePitch_E_
    stdev <- input$numInput_turbinePars_bladePitch_SD_
    
    output$plot_turbinePars_bladePitch <- renderPlot({
      #sdRef <- ifelse(startUpValues$bladePitch_SD/stdev < 0.25, startUpValues$bladePitch_SD*10, startUpValues$bladePitch_SD)
      truncNormPars_densPlots(mu = mu, stdev = stdev, lower = 0, fill = "olivedrab", xlab = "Blade Pitch (degrees)")
    })
    
    output$qtls_turbinePars_bladePitch <- renderPrint({
      truncNormPars_qtlTbl(mu = mu, stdev = stdev, lower = 0, varTag = "bladePitch")
    })
  }) 
  
  
  
  observe({
    mu <- input$numInput_miscPars_windSpeed_E_
    stdev <- input$numInput_miscPars_windSpeed_SD_
    
    output$plot_miscPars_windSpeed <- renderPlot({
      sdRef <- ifelse(startUpValues$turbinePars$windSpeed_SD/stdev < 0.25, startUpValues$turbinePars$windSpeed_SD*5, 
                      startUpValues$turbinePars$windSpeed_SD)
      truncNormPars_densPlots(mu = mu, stdev = stdev, lower = 0, fill = "olivedrab", xlab = "Wind Speed (m/s)")
    })
    
    output$qtls_miscPars_windSpeed <- renderPrint({
      truncNormPars_qtlTbl(mu = mu, stdev = stdev, lower = 0, varTag = "WindSpeed")
    })
    
  })
  
  




  #' --------------------------------------------------------------------------------------------------
  #  ---- Prepare & check input data for simulation, triggered when user pushes go        ----
  #' --------------------------------------------------------------------------------------------------
  
  observeEvent(input$actButtonInput_simulPars_GO, {
    
    missingValues <- list()
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ---  Check if at least one species selected
    
    # Launch error message and block simulation if species field is empty; else, carry on
    if(is.null(slctSpeciesTags())){
      
      rv$speciesFieldFilled <- FALSE
      
      # Modal describing error to the user
      sendSweetAlert(
        session = session,
        title = "No species selected",
        text = span(
          style = "font-size: 15px; text-align: left",
          hr(),
          br(),
          tags$b("Please select at least one species in 'Step 2: Specie(s)'", style = "font-size: 15px; text-align: center")
        ),
        type = "error",
        html = TRUE
      )
      
      req(rv$speciesFieldFilled)
      
    }else{
      
      rv$speciesFieldFilled <- TRUE
    }
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # -------  bird biometric data
    
    # -- gather data
    birdBiomData <- rv$biomParsInputs_ls %>%
      ldply(function(x){data.frame(Value = as.character(x))}, .id = "inputTags") %>%
      dplyr::mutate(inputTags_split = stringr::str_split(inputTags, "_")) %>%
      dplyr::mutate(specLabel = map_chr(inputTags_split, function(x) paste(x[-c(1:4)], collapse = "_")),
             par = map_chr(inputTags_split, function(x) x[3]),
             hyper = map_chr(inputTags_split, function(x) x[4]),
             par_hyper = paste(par, hyper, sep = "_")) %>%
      dplyr::select(-c(inputTags, inputTags_split, par, hyper)) %>%
      dplyr::semi_join(., slctSpeciesTags(), by = "specLabel")  # filter for currently selected species
    
    
    # -- Check for NAs and save affected parameters
    missingValues[["birdBiom"]] <- birdBiomData %>%
      dplyr::filter(is.na(Value)) %>%
      arrange(specLabel) %>%
      dplyr::select(-Value) %>%
      mutate(specName = str_replace_all(specLabel, "_", " "))
    
    
    # Data manipulation for model
    rv$birdata_model <- birdBiomData %>%
      spread(par_hyper, Value) %>%
      dplyr::rename(Species = specLabel, AvoidanceBasic = basicAvoid_E, AvoidanceBasicSD = basicAvoid_SD,
             AvoidanceExtended = extAvoid_E, AvoidanceExtendedSD = extAvoid_SD, Body_Length = bodyLt_E, Body_LengthSD = bodyLt_SD,
             Wingspan = wngSpan_E, WingspanSD = wngSpan_SD, Flight_Speed = flSpeed_E, Flight_SpeedSD = flSpeed_SD,
             Nocturnal_Activity = noctAct_E, Nocturnal_ActivitySD = noctAct_SD, Flight = flType_tp,
             Prop_CRH_Obs = CRHeight_E, Prop_CRH_ObsSD = CRHeight_SD) %>%
      dplyr::select(Species, AvoidanceBasic, AvoidanceBasicSD, AvoidanceExtended, AvoidanceExtendedSD, Body_Length, Body_LengthSD,
             Wingspan, WingspanSD, Flight_Speed, Flight_SpeedSD, Nocturnal_Activity, Nocturnal_ActivitySD, Flight,
             Prop_CRH_Obs, Prop_CRH_ObsSD)
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # -- birds FHD
    #
    # FHD data source chosen for each species
    RVs_inputs_ls <- reactiveValuesToList(input)
    FHD_UserOptions_ls <- RVs_inputs_ls[grep("userOpts_FHD_dtSrc", names(RVs_inputs_ls))]
    
    # Check for missing FHD data for each selected species
    FHD_dataStatus <- FHD_UserOptions_ls %>%
      map2_dfr(., names(.), function(x, y){
        
        specLabel <- map_chr(stringr::str_split(y, "_"), ~ paste(.[-c(1:4)], collapse = "_"))
        
        if(x == "default"){
          tibble(specLabel = specLabel) %>%
            mutate(missingFHD = ifelse(file.exists(paste0("data/", specLabel, "_ht_dflt.csv")), FALSE, TRUE))
          
        }else{
          if(x == "other"){
            cUserFHD_LsIndice <- str_which(safe_names(flgtHghDstInputs_ls()), specLabel)
            tibble(specLabel = specLabel) %>%
              mutate(missingFHD = ifelse(length(cUserFHD_LsIndice) > 0, FALSE, TRUE))
          }
        }
      })%>%
      semi_join(., slctSpeciesTags(), by = "specLabel")  # filter for currently selected species
    
  
    
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # --------- turbine data 
  
    if(nrow(missingValues) > 0){
      
      # flag presence of NAs in data
      rv$NAsFreeData <- FALSE
      
      # Modal describing error to the user
      sendSweetAlert(
        session = session,
        title = "Found missing values in inputs",
        text = span(
          style = "font-size: 15px; text-align: left",
          hr(),
          p("NAs assigned to inputs in the following sections"),
          tags$ul(
            missingValues %>%
              split(.$dataMainType) %>%
              map(function(x){
                if(unique(x$dataMainType) == "speciesFeatures"){
                  tags$li(tags$u("Species features"),
                          tags$ul(
                            map(unique(x$specName), tags$li)
                          ), br())
                }else{
                  if(unique(x$dataMainType) == "turbineFeatures"){
                    tags$li(tags$u("Turbine and/or wind farm features"))
                  }
                }
              })
          ),
          br(),
          tags$b("Please fill in empty fields (highlighted in red) before proceeding to simulation", style = "font-size: 15px; text-align: center")
        ),
        type = "error",
        html = TRUE
      )
      
      #browser()
      
    }else{
      # flag data is free off NAs
      rv$NAsFreeData <- TRUE
    }
    
    # Flick the trigger 
    rv$simCodeTrigger <- rv$simCodeTrigger + 1
  })
  
  
 
  #' -----------------------------------------------------------------------
  #  ----            Collision Risk Simulation Model                    ----
  #' -----------------------------------------------------------------------
  
  #observeEvent(input$actButtonInput_simulPars_GO, {
  observeEvent(rv$simCodeTrigger, ignoreInit = TRUE, {
    
    #--- step 1: safety check if the various types on input data are valid for proceeding to simulation ------ #
    req(rv$speciesFieldFilled,
        rv$densityDataPresent, 
        rv$NAsFreeData,
        rv$FHD_acceptable)
    
    
    # --- step 2: house-cleanig - remove output files left from the previous run in the current session ------ #
    
    file.remove(list.files(path2ShinyOut_Inputs, full.names = TRUE))
    file.remove(list.files(path2ShinyOut_Outputs, full.names = TRUE))
    file.remove(list.files(path2Outputs_results, full.names = TRUE, recursive = TRUE))
    
    
    
    #--- step 3: write out data in files as required for the model  ----- # 
    
    rv$monthDensData_model %>%
      mutate(export = walk2(userOption, data, function(x, y){
        
        if(x == "truncNorm"){
          fwrite(y, "data/CountData.csv")
          fwrite(y, file.path(path2ShinyOut_Inputs, "birdDensityData_truncnorm.csv"))
        }
        
        if(x == "reSamp"){
          fwrite(y, "data/birdDensityData_samples.csv")
          fwrite(y, file.path(path2ShinyOut_Inputs, "birdDensityData_samples.csv"))
        }
        
        if(x == "pcntiles"){
          fwrite(y, "data/birdDensityData_refPoints.csv")
          fwrite(y, file.path(path2ShinyOut_Inputs, "birdDensityData_refPoints.csv"))
        }
        
      }))
    
    
    # --- rotor speed and pitch vs windspeed
    windPowerData <- left_join(hot_to_r(input$hotInput_turbinePars_rotationVsWind), # produces "Warning in asMethod(object) : NAs introduced by coercion"
                               hot_to_r(input$hotInput_turbinePars_pitchVsWind),    # produces "Warning in asMethod(object) : NAs introduced by coercion"
                               by = "windSpeed") %>%
      dplyr::rename(Wind = windSpeed, Rotor = rotationSpeed, Pitch = bladePitch) %>%
      drop_na()
    
    
    
    # --- simulation Options
    simOptions <- tibble(
      iterations = input$sldInput_simulPars_numIter, 
      largeArrayCorrection = ifelse(input$chkBoxInput_simulPars_largeArrarCorr==TRUE, "yes", "no")
    )
    
    
    # model function expects data to be provided in csv files, so saving them out for now to avoid code inconsistencies 
    # Should change model function to expect data.frames instead of files once it's final version is established
    fwrite(rv$birdata_model, "data/BirdData.csv")
    fwrite(rv$turbineData_model, "data/TurbineData.csv")
    fwrite(windPowerData, paste0("data/windpower_", input$numInput_turbinePars_turbinePower, ".csv"))
    
    
    # save inputs to the user's downloadable folder - repeating the previous step (except the changes to the turbine data) makes 
    # code a bit inefficient, but the previous step should be temporary
    fwrite(rv$birdata_model, file.path(path2ShinyOut_Inputs, "BirdData.csv"))
    rv$turbineData_model %>% select(-c(RotorRadiusSD, HubHeightAddSD, BladeWidthSD)) %>% dplyr::rename(airGap = HubHeightAdd) %>%
      fwrite(., file = file.path(path2ShinyOut_Inputs, "TurbineData.csv"))
    fwrite(windPowerData, file.path(path2ShinyOut_Inputs, paste0("windpower_", input$numInput_turbinePars_turbinePower, ".csv")))
    fwrite(rv$windfarmData_model, file.path(path2ShinyOut_Inputs, "windfarmData.csv"))
    fwrite(simOptions, file.path(path2ShinyOut_Inputs, "simOptions.csv"))
    
    
    # ----- step 4: Set progress bar ----- #
    
    # Create a Progress objects for species and iterations within each species
    progress_Spec <- Progress$new()
    progress_Iter <- Progress$new()
    
    progress_Spec$set(message = "Processing ", value = 0)
    progress_Iter$set(message = "Simulating...", value = 0)  # "Going through iterations"
    
    on.exit({
      progress_Iter$close()
      progress_Spec$close()
    })
    
    
    # callback functions to update progress on species.
    updateProgress_Spec <- function(value = NULL, detail = NULL) {
      progress_Spec$set(value = value, detail = detail)
    }
    
    
    # callback functions to update progress on iterations. Each time updateProgress_Iter() is called, 
    # it moves the bar 1/nth of the total distance.
    updateProgress_Iter <- function(value = NULL, detail = NULL) {
      progress_Iter$set(value = value, detail = detail)
    }
    
    
    # ----- step 5: run simulation function ----- # 
    
    if(1){
      rv$sCRM_output_ls <- stochasticBand(
        workingDirectory="sCRM/",
        results_folder = path2Outputs_results,
        BirdDataFile = "data/BirdData.csv",
        TurbineDataFile = "data/TurbineData.csv",
        CountDataFile = "data/CountData.csv",
        FlightDataFile = "data/FlightHeight.csv",
        iter = input$sldInput_simulPars_numIter, 
        CRSpecies = slctSpeciesTags()$specLabel, 
        TPower = rv$windfarmData_model$targetPower_MW, #rv$windfarmData_model$nTurbines*input$numInput_turbinePars_turbinePower,
        LargeArrayCorrection = ifelse(input$chkBoxInput_simulPars_largeArrarCorr==TRUE, "yes", "no"), 
        WFWidth = rv$windfarmData_model$width_km,
        Prop_Upwind = rv$windfarmData_model$upwindFlights_prop/100, # convert % (user input) to proportion (expected by model function)
        Latitude = rv$windfarmData_model$latitude_deg,
        TideOff = rv$windfarmData_model$TidalOffset_m,
        windSpeedMean = input$numInput_miscPars_windSpeed_E_, 
        windSpeedSD = input$numInput_miscPars_windSpeed_SD_,
        #windPowerData = windPowerData,
        updateProgress_Spec,  # pass in the updateProgress function so that it can update the progress indicator.
        updateProgress_Iter,
        DensityOpt = rv$monthDensOpt_model # pass in the user options for bird density data
      )
    }
    
    
    
    # Alternative parameterisation etc to match Band spreadsheet --------------
    
    if(0){
      
      source("BandModel_function_band_comparison.R")
      
      rv$sCRM_output_ls <- stochasticBand_compare(
        workingDirectory="sCRM/",
        results_folder = path2Outputs_results,
        BirdDataFile = "data/BirdData.csv",
        TurbineDataFile = "band_comparison_inputs/TurbineData.csv",
        CountDataFile = "band_comparison_inputs//CountData.csv",
        FlightDataFile = "data/FlightHeight.csv",
        iter = 2, 
        CRSpecies = "Black_legged_Kittiwake", 
        TPower = rv$windfarmData_model$targetPower_MW, #rv$windfarmData_model$nTurbines*input$numInput_turbinePars_turbinePower,
        LargeArrayCorrection = "yes",
        WFWidth = 10,
        Prop_Upwind = 0.5, # convert % (user input) to proportion (expected by model function)
        Latitude = 55.8,
        TideOff = 2.5,
        windSpeedMean = input$numInput_miscPars_windSpeed_E_, 
        windSpeedSD = input$numInput_miscPars_windSpeed_SD_,
        #windPowerData = windPowerData,
        updateProgress_Spec,  # pass in the updateProgress function so that it can update the progress indicator.
        updateProgress_Iter,
        DensityOpt = rv$monthDensOpt_model # pass in the user options for bird density data
      )
    }    
    
    
  })
  
  
  
  
  # -----------------------------------------------------------------------
  #  ----            Compute and display model outputs                  ----
  # -------------------------------------------------------+----------------
  
  # Arrange results data into a data.frame
  sCRM_outputDF <- eventReactive(rv$sCRM_output_ls, {
    
    req(rv$sCRM_output_ls)
    
    listLevels <- expand.grid(option = names(rv$sCRM_output_ls), specLabel = names(rv$sCRM_output_ls[[1]]), 
                              turbineModel =  names(rv$sCRM_output_ls[[1]][[1]]))
    
    pmap(list(x=as.character(listLevels$option), y=as.character(listLevels$specLabel), z=as.character(listLevels$turbineModel)), 
         function(x, y, z) {
           data.frame(option = x, specLabel = y, turbineModel = z, rv$sCRM_output_ls[[x]][[y]][[z]]) %>%
             mutate(option = str_replace(option, "monthCollsnReps_opt", "Option "),
                    turbineModel = str_replace(turbineModel, "turbModel", ""), 
                    iter = 1:nrow(.))
         }
    ) %>%
      dplyr::bind_rows()
  })
  
  
  
  
  observe({
    
    df <- sCRM_outputDF()
    
    req(nrow(df) > 0)
    
    # Set-up progress bar for the construction of plots and tables from model outputs
    progress_genModelOuts <- Progress$new()
    on.exit(progress_genModelOuts$close())
    progress_genModelOuts$set(message = "Generating outputs", value = 0)
    pbarNSteps <- 6
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~  boxplots and summary tables of collisons per month, for each option and species  ~~~ #
    
    df_monthlyColl <- df %>%
      gather(Month, Collisions, -c(option, specLabel, turbineModel, iter)) %>%
      mutate(Month = factor(Month, levels = unique(Month)),
             specLabel = as.character(specLabel)) %>%
      group_by(specLabel, option, turbineModel) %>%
      nest()
    
    # update pbar
    progress_genModelOuts$inc(1/pbarNSteps)
    
    # --- plots
    df_monthlyColl %>%
      mutate(plot = pmap(list(dt = data, spec = specLabel, opt = option), function(dt, spec, opt){
        
        dt %<>% mutate(opt = opt)
        plotTag <- paste0(spec, "_plot_monthCollisions_", str_replace(opt, " ", ""))
        # print(plotTag)
        
        p <- ggplot(dt) +
          geom_boxplot(aes(x = Month, y = Collisions), fill = "mediumpurple1", alpha = 0.8) +
          facet_wrap(~opt) +
          theme(legend.position="none") +
          labs(y="Number of Collisions", x = "") +
          theme(strip.background=element_rect(fill="grey95"))
        
        output[[plotTag]] <- renderPlot(p)
        
        # save plot externally
        p2 <- p+labs(title=str_replace_all(specLabel, "_", " "))
        ggsave(paste0(plotTag, ".png"), p2, path = path2ShinyOut_Outputs, width = 19, height = 12, units = "cm") 
        
      }))
    
    # update pbar
    progress_genModelOuts$inc(2/pbarNSteps)
    
    
    # --- summary tables
    df_monthlyColl %>%
      mutate(sumTable = pmap(list(dt = data, spec = specLabel, opt = option), function(dt, spec, opt){
        
        dt %<>% 
          group_by(Month) %>%
          summarise(Mean = mean(Collisions), 
                    SD = sd(Collisions), CV = SD/Mean, 
                    Median = median(Collisions), 
                    #IQR = IQR(Collisions), 
                    `2.5%` = quantile(Collisions, 0.025), 
                    `25%` = quantile(Collisions, 0.25),
                    `75%` = quantile(Collisions, 0.75),
                    `97.5%` = quantile(Collisions, 0.975)
          ) %>%
          dplyr::mutate_at(.vars = vars(Mean:`97.5%`), list(~sprintf(fmt = "%.3f", .)))
        
        
        #print(dt)
        
        sumTableTag <- paste0(spec, "_summTable_monthCollisions_", str_replace(opt, " ", ""))
        #print(sumTableTag)
        
        output[[sumTableTag]] <- renderDataTable({
          datatable(dt, rownames = FALSE, 
                    caption = paste0("Model ", opt), 
                    options = list(pageLength = 12, dom = 't'))
        })
        
        fwrite(dt, file.path(path2ShinyOut_Outputs, paste0(sumTableTag, ".csv")))
        
      }))
    
    # update pbar
    progress_genModelOuts$inc(3/pbarNSteps)
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~ density plots and summary tables of overall collisons, for each option and species   ~~~ #
    
    df_overallColl <- df %>%
      gather(Month, Collisions, -c(option, specLabel, turbineModel, iter)) %>%
      mutate(Month = factor(Month, levels = unique(Month)),
             specLabel = as.character(specLabel)) %>%
      group_by(option, specLabel, turbineModel, iter) %>%
      summarise(overalCollisions = sum(Collisions)) %>%
      group_by(specLabel, turbineModel) %>%
      nest()
    
    
    # update pbar
    progress_genModelOuts$inc(4/pbarNSteps)
    
    
    # -- Plots
    df_overallColl %>%
      mutate(plot=pmap(list(dt = data, spec = specLabel), function(dt, spec){
        
        plotTag <- paste0(spec, "_plot_overallCollisions")
        #print(plotTag)
        
        p <- ggplot(dt) +
          geom_density(aes(x = overalCollisions, colour = option, fill = option), alpha = 0.2) +
          labs(x="Number of Collisions", y = "Probability Density") +
          scale_colour_brewer(palette = "Set1") +
          scale_fill_brewer(palette = "Set1") +
          theme(legend.title=element_blank(), legend.position="top") 
        
        output[[plotTag]] <- renderPlot(p)
        
        # save plot externally
        p2 <- p+labs(title=str_replace_all(specLabel, "_", " "))
        ggsave(paste0(plotTag, ".png"), p2, path = path2ShinyOut_Outputs, width = 19, height = 12, units = "cm")
      }))
    
    
    # update pbar
    progress_genModelOuts$inc(5/pbarNSteps)
    
    
    # -- Summary tables
    df_overallColl %>%
      mutate(sumPlot = pmap(list(dt = data, spec = specLabel, turb = turbineModel), function(dt, spec, turb){
        
        dt %<>%
          mutate(Turbine = turb) %>%
          dplyr::rename(Option = option) %>%
          group_by(Turbine, Option) %>%
          summarise(Mean = mean(overalCollisions), 
                    SD = sd(overalCollisions), CV = SD/Mean, Median = median(overalCollisions), 
                    #IQR = IQR(overalCollisions), 
                    `2.5%` = quantile(overalCollisions, 0.025), 
                    `25%` = quantile(overalCollisions, 0.25),
                    `75%` = quantile(overalCollisions, 0.75),
                    `97.5%` = quantile(overalCollisions, 0.975)) %>%
          mutate_at(.vars = vars(Mean:`97.5%`), list(~sprintf(fmt = "%.3f", .))) %>%
          ungroup() %>% select(-Turbine)  # leave turbine model out of the table for now - current version with only one turbine model per simulation
        
        #print(dt)
        
        sumTableTag <- paste0(spec, "_summTable_overallCollisions")
        #print(sumTableTag)
        
        output[[sumTableTag]] <- renderDataTable({
          datatable(dt, rownames = FALSE, 
                    options = list(
                      #autoWidth = TRUE,
                      pageLength = 12, 
                      dom = 't'))
        })
        
        fwrite(dt, file.path(path2ShinyOut_Outputs, paste0(sumTableTag, ".csv")))
      }))
    
    # update pbar
    progress_genModelOuts$inc(6/pbarNSteps)
    
  })
  
  
  
  
  # --------------------------------------------------------------------------------------------------------
  #  ----   Massage and relocate summaries of randomly generated parameter values for user's Download    ----
  # --------------------------------------------------------------------------------------------------------
  
  observeEvent(rv$sCRM_output_ls, {
    
    slctSpecLabels <- slctSpeciesTags()$specLabel
    
    # Turbine parameters
    turbSampFiles <- list.files(path = file.path(path2Outputs_results, "tables/"), pattern = "_sampledTurbineParameters")
    
    walk(turbSampFiles, function(x){
      
      cSpecLabel <- slctSpecLabels[str_which(string = x, slctSpecLabels)]
      
      cSpecTurbSampledData <- fread(paste0(path2Outputs_results, "/tables/", x)) %>%
        select(-V1) %>%
        mutate_at(.vars = vars(Mean:IQR), list(~sprintf(fmt = "%.4f", .)))
      
      fwrite(cSpecTurbSampledData, file = file.path(path2ShinyOut_Outputs, paste0(cSpecLabel, "_sampledTurbineParameters.csv")))
    })
    
    
    # Bird parameters
    birdSampFiles <- list.files(path =file.path(path2Outputs_results, "tables/"), pattern = "_sampledBirdParameters")
    
    walk(birdSampFiles, function(x){
      
      cSpecLabel <- slctSpecLabels[str_which(string = x, slctSpecLabels)]
      
      cSpecBirdSampledData <- fread(paste0(path2Outputs_results, "/tables/", x)) %>%
        select(-V1) %>%
        mutate_at(.vars = vars(Mean:IQR), list(~sprintf(fmt = "%.4f", .)))
      
      fwrite(cSpecBirdSampledData, file = file.path(path2ShinyOut_Outputs, paste0(cSpecLabel, "_sampledBirdParameters.csv")))
    })
    
  })
  
  
  
  
  
  # ----------------------------------------------------------------
  #  ----              Download model outputs                    ----
  # ----------------------------------------------------------------
  
  output$dwnld_ModelOuts <- downloadHandler(
    filename = function() {
      "modelOutputs.zip"
    },
    content = function(file) {
      basedir <- getwd()
      setwd(file.path("shinyOutputs", sessTempOutFolder))
      fs <- list.files()
      zip(zipfile = file, files = fs)
      setwd(basedir)
    },
    contentType = "application/zip"
  )
  
  
  
  
  
  
  # ---------------------------------------------------------
  #  ----       Session's house-cleaning                  ----
  # ---------------------------------------------------------
  
  # --- Delete temporary folders (& files) created during the current session
  onStop(function(){
    #cat("Session stopped\n")
    unlink(file.path("shinyOutputs", sessTempOutFolder), recursive = TRUE)
    unlink(path2Outputs_results, recursive = TRUE)
  })
  
  
  
  
  
  # ----------------------------------------------------------------
  #  ----              Miscellaneous  Stuff                    ----
  # ----------------------------------------------------------------
  
  # Version updates - describing latest developments/updates implemented in the app
  observeEvent(input$appvrsn, {
    showModal(
      modalDialog(size = "l",
                  title = h3("Release Notes"),
                  h4("v0.0.9 - July, 2021"),
                  p("Added the bird features module"),
                  tags$ul(
                    tags$li(tags$b("Additions & Updates"), 
                            tags$ul(
                              tags$li("Changed over to a tab-based structure for the species features"),
                              tags$li("Generated the module for creating species feature tabs"),
                              tags$li("Changed colour scheme to teal - blue")
                            )),
                    tags$li(tags$b("To-Do List"),
                            tags$ul(
                              tags$li("Allow users to upload shapefile"),
                              tags$li("Migratory bird maps to be generated and added"),
                              tags$li("Species list to be finalized and added")
                            ))
                  ),
                  easyClose = TRUE
      ))
  }, priority = 10)
  
  
  
  

}

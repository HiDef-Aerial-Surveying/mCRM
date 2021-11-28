#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinycssloaders
#' @import shinyjs
#' @import shinyBS
#' @import rhandsontable
#' @import plyr
#' @import purrr
#' @import tidyr
#' @import ggplot2
#' @import magrittr
#' @import msm
#' @import V8
#' @import shinyWidgets
#' @import data.table
#' @import DT
#' @import zip
#' @import RColorBrewer
#' @import pracma
#' @import heatmaply
#' @import leaflet
#' @import stochLAB
#' @noRd
#' 


# Set some globals ------------------------------------------------------

ggplot2::theme_set(ggplot2::theme_bw())


species <- sort(c("Arctic Skua", "Northern Fulmar", "Great Black-backed Gull", "Common Guillemot", "Northern Gannet",
                  "Black-legged Kittiwake", "Lesser Black-Backed Gull", "Little Auk", "Atlantic Puffin", 
                  "Razorbill", "Arctic Tern", "Black-headed Gull", "Black-throated Diver", "Common Gull", "Common Scoter",
                  "Common Tern", "Cormorant", "Eider", "European Shag", "Herring Gull", "Little Gull", "Manx Shearwater",
                  "Red-throated Diver", "Sandwich Tern"))


defaultSpecies<- "Black-legged Kittiwake"

# generate continuous Spectral pallete
Spectral_pal_cont <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11,"Spectral")))
YlOrRd_pal_cont <- grDevices::colorRampPalette(c("white", RColorBrewer::brewer.pal(9,"YlOrRd")))
PuBuGn_pal_cont <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9,"PuBuGn"))
YlOrBr_pal_cont <- grDevices::colorRampPalette(c("white",RColorBrewer::brewer.pal(9,"YlOrBr")))
manual1_pal_cont <- grDevices::colorRampPalette(rev(c("#8E063B", "#AB4147", "#C56551", "#DA8459", "#E99F61", "#F2B669", "#F6C971", "#F4D97B", "#EDE388", "#E2E6BD", "#e0e2cc", "#f1f2e6")))
manual2_pal_cont <- grDevices::colorRampPalette(rev(c("#7D0112", "#8E2C19", "#9E4723", "#AD5F30", "#BC763E", "#C88C4F", "#D4A261", "#DEB675", "#E6C98A", "#ECDAA0", "#F1E9B8", "#F2F1E4")))

# javascript code for extendShinyjs to highlight NAs in input fields
jsCode <- '
shinyjs.backgroundCol = function(params) {
var defaultParams = {
id : null,
col : "red"
};
params = shinyjs.getParams(params, defaultParams);
var el = $("#" + params.id);
el.css("background-color", params.col);
}'

NAsHighlightColor <- "#FDC0C0"


app_ui <- function(request) {
  
  # Dashboard header ------------------------------------------------------
  
  header <- dashboardHeader(
    
    titleWidth =270,
    #title = "Avian Migration CRM",
    
    tags$li(class = "dropdown", actionLink("appvrsn", label = tags$b("v0.0.9"), style = "font-size: 19px")), 
    tags$li(class = "dropdown", a(icon('github', "fa-2x"), href='https://github.com/HiDef-Aerial-Surveying/mCRM', 
                                  style = "padding-top: 10px; padding-bottom: 10px", target='_blank', id="lbl_codeLink")),
    tags$li(class = "dropdown", a(icon('bug', "fa-2x"), href='https://github.com/HiDef-Aerial-Surveying/mCRM/issues', #exclamation-circle
                                  style = "padding-top: 10px; padding-bottom: 10px", target='_blank', id="lbl_issuesLink")),
    
    tags$li(class = "dropdown", actionLink("saveInputs_btt", label = NULL, icon("save", "fa-2x", lib = "font-awesome"),
                                           style = "padding-top: 10px; padding-bottom: 10px")),
    tags$li(class = "dropdown", actionLink("restoreInputs_btt", label = NULL, icon("window-restore", "fa-2x", lib = "font-awesome"),
                                           style = "padding-top: 10px; padding-bottom: 10px")),
    
    
    tags$li(class = "dropdown headerimg", a(img(src = "www/bioConsultSH_Logo_2.png", height = "40px"), href='https://bioconsult-sh.de/en/',
                                  style = "padding-top: 5px; padding-bottom: 5px;", target='_blank', id="lbl_bioConsultLogoLink"),
            style="float: right"),
    tags$li(class = "dropdown headerimg", a(img(src = "www/HiDef Logo_2.png", height = "40px"), href='https://hidef.bioconsult-sh.de/',
                                  style = "padding-top: 5px; padding-bottom: 5px;", target='_blank', id="lbl_hiDefLogoLink"),
            style="float: right"),
    tags$li(class = "dropdown headerimg", a(img(src = "www/DMP_logo_1.png", height = "40px"), href='https://www.dmpstats.com',
                                  style = "padding-top: 5px; padding-bottom: 5px", target='_blank', id="lbl_dmpLogoLink"), 
            style="float: right"),
    tags$li(class = "dropdown headerimg", a(img(src = "www/MS Logo Linear-01_2.png", height = "30px"), href='https://www.gov.scot/Topics/marine',
                                  style = "padding-top: 10px; padding-bottom: 10px;", target='_blank', id="lbl_marineScotlandLink"),
            style="float: right")  
  )
  
  header$children[[2]]$children <-  tags$a(href='#',
                                             tags$img(src='www/hexSticker.png',height="100%"))
  
  # Dashboard sidebar -------------------------------------------------------
  
  sidebar <- dashboardSidebar(
    width = 270,
    hr(),
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Step 1: Turbine & Wind farm features", tabName = "tab_turbWindPars", icon = icon("tachometer-alt")
      ),
      hr(),
      menuItem(
        "Step 2: Species features", tabName = "tab_birdPars", icon = icon("twitter")
      ),
      hr(),
      menuItem(
        "Step 3: Generate scenarios", tabName = "tab_generateScenarios", icon = icon("cogs")
      ),
      hr(),
      menuItem(
        "Step 4: Simulation & Results", tabName = "tab_simulation", icon = icon("bar-chart")
      ),
      hr(),

      bsAlert(anchorId = "alert")
      
      #initStore("store", "shinyStore-ex1")
      
    ),
    fluidRow(
      column(
        align='center',
        width=10,
        offset=1,
        h2("Avian Migration Collision risk"),
        img(src = "www/hexSticker.png", height = "180px"),
        p("The Avian migration collision risk model (mCRM) was developed under a Marine Scotland contract by HiDef Aerial Surveying Ltd. and\
          DMP statistics. This application is in BETA and is undergoing testing and approval")
      )
    )
    
    
    
  )  
  
  
  
  # Dashboard body ----------------------------------------------------------
  body <- dashboardBody(
    div(class="tab-content", id="tabItemsEnvelope",  # required as reference to the dynamic UI tab for each species via insertUI()
        tabItem(tabName="tab_turbWindPars",
                fluidRow(
                  box(
                      title = "Wind farm footprints",
                      width = 12,
                      status = "primary", 
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      column(4,
                             radioGroupButtons(inputId = "radGrpInput_wfshape_builtin_or_userinput",
                                               individual = TRUE,
                                               justified = TRUE, 
                                               label = NULL,
                                               choices = c("Existing windfarms" = "existWindFarms",
                                                           "Custom windfarms" = "customWindFarms"),
                                               checkIcon = list(yes = tags$i(class = "fa fa-circle",
                                                                             style = "color: steelblue"),
                                                                no = tags$i(class = "fa fa-circle-o",
                                                                            style = "color: steelblue"))),
                             selectizeInput("selectInput_builtin_wfList",
                                         label = "Select wind farms (Maximum 5)",
                                         choices = Scotwind_Merged$NAME[order(Scotwind_Merged$NAME)],
                                         options = list(maxItems = 5L)
                             ),
                             
                             p("Click button to update the list of windfarms"),
                             actionButton("button_update_Windfarm_tabs","Update windfarm list", class="btn-lg btn-success"),
                             br(),
                             
                             p("If drawing from the existing list of windfarms, simply click in the text box above\
                               to begin searching. The dropdown is populated from publicly accessible shapefiles of\
                               offshore wind development sites as per EMODnet, accessed July 2021."),
                             br(),
                             p("The map and input parameters for the wind farm sites are populated automatically. \
                               The map is interactive, and clicking on a polygon will display the name of the site."),
                             br(),
                             p("You may upload your own polygon shapefile as well, please see instructions on the\
                               required format for upload.")
                      ),
                      column(8,
                             leaflet::leafletOutput("map",width="100%") %>% withSpinner(color="#6794d5")
                      )
                  )
                ),
                
                
                
                fluidRow(
                  column(width=12,
                         box(width=12,
                             status='primary',
                             tabsetPanel(id="windfarm_Tabs",
                                         type="tabs"
                             )
                         )))
                ### Windfarm parameter modules get inserted here
                #uiOutput("windfarm_placeholder"),

        ),
        tabItem(tabName="tab_birdPars", class="active",
                fluidRow(
                  box(
                    title = "Species selection",
                    width = 12,
                    status = "primary", 
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(12,
                           selectizeInput("selectInput_builtin_speciesList",
                                          label = "Select species (scientific names instead?)",
                                          choices = defaultSpeciesValues$Common_name,
                                          options = list(maxItems = 20L)
                           ),
                           br(),
                           p("Click to update the species based on the above selection. If you remove a species, you'll have to re-enter the data"),
                           actionButton("button_update_Species_tabs","Update species list", class="btn-lg btn-success"),
                           br(),
                           uiOutput("Species_Count")
                    )
                    
                  ),
                  fluidRow(
                    column(width=12,
                           box(width=12,
                               status='primary',
                              tabsetPanel(id="specTabs",
                                          type="tabs"
                             )
                           )))
                )
                ),
        
        tabItem(tabName="tab_generateScenarios",class="active",
                fluidRow(
                 column(12,
                        p("Click the button to generate a spreadsheet of scenarios to run based on your selections"),
                        HTML("<p><strong>You must generate these scenarios before continuing</strong></p>"),
                        p("This list can be edited manually, or you may copy and paste from a spreadsheet")
                        ),
                 column(3,
                        actionButton("button_generate_scenarios","Generate Scenarios", class="btn-lg btn-success")),
                 column(3,
                        actionButton("button_download_scenarios","Download Scenarios", class="btn-lg btn-warning")),
                 column(3,
                        actionButton("button_upload_scenarios","Upload Scenarios", class="btn-lg btn-primary")),
                 column(3,
                        actionButton("button_download_scenario_worksheet","Download Scenario worksheet", class="btn-lg btn-info"))
                ),
                fluidRow(
                style="padding-top:20px;",
                 box(
                   title = "Bird scenarios",
                   width = 12,
                   status = "primary", 
                   solidHeader = TRUE,
                   column(12,
                          rHandsontableOutput("hotInput_output_bird_scenarios", width = "100%"),
                   )
                 )
                ),
                fluidRow(
                  style="padding-top:20px;",
                  box(
                    title = "Wind farm scenarios",
                    width = 12,
                    status = "primary", 
                    solidHeader = TRUE,
                    column(12,
                           rHandsontableOutput("hotInput_output_wf_scenarios", width = "100%"),
                    )
                  )
                ),
                fluidRow(
                  style="padding-top:20px;",
                  box(
                    title = "Population scenarios",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    column(12,
                           rHandsontableOutput("hotInput_output_population_scenarios",width="100%"))
                  )
                )
                
                
                
                
                ),
        
        
        tabItem(tabName="tab_simulation", class = "active",
                fluidRow(
                  box(title = "Simulation Options", width = 2, status = "primary", solidHeader = TRUE, #background = "aqua", 
                      # checkboxInput(inputId = "chkBoxInput_simulPars_largeArrarCorr", label = tags$b("Apply large Array Correction"), 
                      #               value = TRUE),
                      
                      tags$b(HTML(paste0("Large Array Correction", actionLink("lbl_arrayCorrect", label=NULL, 
                                                                              icon=icon('info-circle'))))),
                      shinyBS::bsTooltip(id = "lbl_arrayCorrect", 
                                         title = paste0("Adjustment to the probability of bird collision to account for the depletion ",
                                                        "of bird density in later rows of windfarms with a large array of turbines"),
                                         options = list(container = "body"), placement = "right", trigger = "hover"),
                      
                      switchButton(inputId = "chkBoxInput_simulPars_largeArrarCorr",
                                   label = "", 
                                   value = TRUE, col = "GB", type = "OO"),
                      
                      hr(),
                      sliderInput(inputId = "sldInput_simulPars_numIter", label = "Number of Iterations",
                                  min = 1000, max = 5000, step = 100, value = 1000),
                      
                      hr(),
                      actionButton(inputId = "actButtonInput_simulPars_GO", label = tags$b("Run Simulation"), 
                                   icon = icon("cogs"), width = "100%")
                  ),
                  
                  
                  
                  box(title = "Model Ouputs", width = 10, status = "primary", solidHeader = TRUE,
                      uiOutput("simResults_UI")
                  )
                )
        )
    ),
    
    
    # popup msg for the elements in the header
    shinyBS::bsTooltip(id = "lbl_issuesLink",
                       title = paste0("Submit issues, queries & suggestions. Thanks!"),
                       options = list(container = "body"), placement = "bottom", trigger = "hover"),
    shinyBS::bsTooltip(id = "lbl_codeLink",
                       title = paste0("Code and user manual"),
                       options = list(container = "body"), placement = "bottom", trigger = "hover"),
    shinyBS::bsTooltip(id = "appvrsn",
                       title = paste0("Release notes"),
                       options = list(container = "body"), placement = "bottom", trigger = "hover"),
    shinyBS::bsTooltip(id = "saveInputs_btt",
                       title = "Store currently specified input values",
                       options = list(container = "body"), placement = "bottom", trigger = "hover"),
    shinyBS::bsTooltip(id = "restoreInputs_btt",
                       title = "Restore inputs to latest stored values",
                       options = list(container = "body"), placement = "bottom", trigger = "hover")
    
    
    
  )
  
  
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    bootstrapPage(
      useShinyjs(),
      extendShinyjs(text = jsCode,functions=c("backgroundCol","getParams")),
      useSweetAlert(),
      
      # Add custom CSS & Javascript;
      tagList(tags$head(
        tags$link(rel="stylesheet", type="text/css",href="www/styles.css"),
        tags$link(rel="stylesheet", type="text/css",href="www/button.css"),
        tags$script(type="text/javascript", src = "www/busy.js"),
        tags$style(".swal-modal {width: 30%;}")
      )),
      
      dashboardPage(header,
                    sidebar,
                    body),
      
      div(class = "busy",
          tags$b(h4("Working on it...")),
          img(src="www/loading.gif")
      )
      
      
    )
    
    
    
    
    
    
  )### top level tagList end
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  add_resource_path(
    'sbs', app_sys('app/sbs')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'mCRM'
    ),
    bundle_resources(
      path = app_sys('app/sbs'),
      app_title = 'mCRM'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
  )
}


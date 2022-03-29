#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinycssloaders
#' @import shinyBS
#' @import rhandsontable
#' @import shinyWidgets
#' @import zip
#' @import leaflet
#' @import stochLAB
#' @importFrom DT DTOutput
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets actionBttn
#' @importFrom shinyWidgets downloadBttn
#' @importFrom bsplus bs_embed_tooltip
#' @importFrom utils packageVersion
#' @noRd
#' 


# Set some globals ------------------------------------------------------

app_ui <- function(request) {
  
  # Dashboard header ------------------------------------------------------
  
  header <- dashboardHeader(
    
    titleWidth =270,
  
    tags$li(class = "dropdown", a(tags$b(paste0("v",packageVersion("mCRM"))), href="https://github.com/HiDef-Aerial-Surveying/mCRM/blob/master/NEWS.md",
                                  style = "padding-top: 10px; padding-bottom: 10px", target='_blank', id="lbl_newsLink")),
    
    tags$li(class = "dropdown", a(icon('file', "fa-2x"), href='https://hidef-aerial-surveying.github.io/mCRM/', 
                                  style = "padding-top: 10px; padding-bottom: 10px", target='_blank', id="lbl_guideLink")),
    tags$li(class = "dropdown", a(icon('video', "fa-2x"), href='#', 
                                  style = "padding-top: 10px; padding-bottom: 10px", target='_blank', id="lbl_videoLink")),
    tags$li(class = "dropdown", a(icon('github', "fa-2x"), href='https://github.com/HiDef-Aerial-Surveying/mCRM', 
                                  style = "padding-top: 10px; padding-bottom: 10px", target='_blank', id="lbl_codeLink")),
    tags$li(class = "dropdown", a(icon('bug', "fa-2x"), href='https://github.com/HiDef-Aerial-Surveying/mCRM/issues', #exclamation-circle
                                  style = "padding-top: 10px; padding-bottom: 10px", target='_blank', id="lbl_issuesLink")),
    
    tags$li(class = "dropdown headerimg", a(img(src = "www/bioConsultSH_Logo_2.png", height = "40px"), href='https://bioconsult-sh.de/en/',
                                  style = "padding-top: 5px; padding-bottom: 5px;", target='_blank', id="lbl_bioConsultLogoLink"),
            style="float: right"),
    tags$li(class = "dropdown headerimg", a(img(src = "www/HiDef_Logo_2.png", height = "40px"), href='https://hidef.bioconsult-sh.de/',
                                  style = "padding-top: 5px; padding-bottom: 5px;", target='_blank', id="lbl_hiDefLogoLink"),
            style="float: right"),
    tags$li(class = "dropdown headerimg", a(img(src = "www/DMP_logo_1.png", height = "40px"), href='https://www.dmpstats.com',
                                  style = "padding-top: 5px; padding-bottom: 5px", target='_blank', id="lbl_dmpLogoLink"), 
            style="float: right"),
    tags$li(class = "dropdown headerimg", a(img(src = "www/MS_Logo_Linear-01_2.png", height = "40px"), href='https://www.gov.scot/Topics/marine',
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
        "Step 4: Simulation & Results", tabName = "tab_simulation", icon = icon("chart-bar")
      ),
      hr(),
      bsAlert(anchorId = "alert")
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
                             
                             selectInput(inputId = "selectInput_wfshape_builtin_or_userinput",
                                         label = "Existing shapefiles, or upload your own?",
                                         choices = c("Existing windfarms" = "existWindFarms",
                                                     "Custom windfarms" = "customWindFarms")),
                
                             uiOutput("Windfarm_Shapes"),
                             
                             uiOutput("selectInput_custom_Windfarm_name_header") %>% withSpinner(color="#0dc5c1"),
                             
                             #actionButton("test","test", class="btn-lg btn-success"),
                             
                             p("Click button to update the list of windfarms"),
                             
                             shinyWidgets::actionBttn(
                               "button_update_Windfarm_tabs",
                               label = "Update windfarm list",
                               icon = icon("wrench"),
                               style="stretch",
                               color="success",
                               no_outline=FALSE
                             ),
                             
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
                                         type="tabs")
                         )))
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
                                          options = list(maxItems = 70L)
                           ),
                           br(),
                           p("Click to update the species based on the above selection. If you remove a species, you'll have to re-enter the data"),
                           shinyWidgets::actionBttn(
                             "button_update_Species_tabs",
                             label = "Update species list",
                             icon = icon("wrench"),
                             style="stretch",
                             color="success",
                             no_outline=FALSE
                           ),
                           
                           br(),
                           uiOutput("Species_Count")
                    )
                    
                  ),
                  fluidRow(
                    column(width=12,
                           box(width=12,
                               status='primary',
                              tabsetPanel(id="specTabs",
                                          type="tabs")
                           )))
                )
                ),
        
        tabItem(tabName="tab_generateScenarios",class="active",
                fluidRow(
                 column(12,
                        h1("Generate windfarm scenarios"),
                        p("Using the buttons below, you can either generate scenarios based on your inputs in Steps 1 and 2,\
                           Download those scenarios as an XLSX worksheet so you can easily facilitate changing parameters,
                           Upload scenarios using the worksheet template,
                           or Download a blank worksheet that you can fill in and upload."),
                        h3("There must be scenarios in these tables before you proceed to Step 4"),
                        p("All the tables generated in this step can be edited on this screen using Excel-style copy and pasting")
                        ),
                 column(1,
                        shinyWidgets::actionBttn(
                          inputId =  "button_generate_scenarios",
                          color = "success",
                          style = "material-circle",
                          icon = icon("cog"),
                          size = "lg"
                        ) %>%
                          bsplus::bs_embed_tooltip(
                            title = "Generate scenario", 
                            placement = "bottom")#,
                        #class = "css_col_inline_btns"
                       ),
                 column(1,
                        shinyWidgets::actionBttn(
                          inputId =  "button_download_scenarios_modal",
                          color = "warning",
                          style = "material-circle",
                          icon = icon("download"),
                          size = "lg"
                        ) %>%
                          bsplus::bs_embed_tooltip(
                            title = "Download constructed scenarios", 
                            placement = "bottom")
                        #actionButton("button_download_scenarios_modal","Download Scenarios", class="btn-lg btn-warning")
                        
                        ),
                 column(1,
                        shinyWidgets::actionBttn(
                          inputId =  "button_upload_scenarios_modal",
                          color = "primary",
                          style = "material-circle",
                          icon = icon("upload"),
                          size = "lg"
                        ) %>%
                          bsplus::bs_embed_tooltip(
                            title = "Upload scenarios", 
                            placement = "bottom")
                        
                        #actionButton("button_upload_scenarios_modal","Upload Scenarios", class="btn-lg btn-primary")
                        
                        ),
                 column(1,
                        shinyWidgets::downloadBttn(
                          "button_download_blank_worksheet",
                          color = "default",
                          style = "material-circle",
                          size = "lg"
                        ) %>%
                          bsplus::bs_embed_tooltip(
                            title = "Download blank worksheet", 
                            placement = "bottom")
                        )
                        #a(href='mCRM_worksheet.xlsx',"Download Scenario worksheet",download=NA,target="_blank",
                          #class="btn btn-default action-button btn-lg btn-info"))
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
                  column(2,
                         box(title = "Simulation Options", width = 12, status = "primary", solidHeader = TRUE, 
                             
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
                         box(title="Download Outputs",width=12,status="primary",solidHeader=TRUE,
                             uiOutput("Report_Download"),
                             uiOutput("Download_Tables")
                             )
                         ),
                  column(10,
                         uiOutput("summTables"),
                         
                         box(title = "Cumulative Outputs", width = 12, status = "primary", solidHeader = TRUE,
                             DT::DTOutput("summTable_cumulative")
                         )
                         )
                )
        )
    ),
    # popup msg for the elements in the header
    shinyBS::bsTooltip(id = "lbl_issuesLink",
                       title = paste0("Submit issues, queries & suggestions. Thanks!"),
                       options = list(container = "body"), placement = "bottom", trigger = "hover"),
    shinyBS::bsTooltip(id = "lbl_codeLink",
                       title = paste0("Code"),
                       options = list(container = "body"), placement = "bottom", trigger = "hover"),
    shinyBS::bsTooltip(id = "lbl_newsLink",
                       title = paste0("Release notes"),
                       options = list(container = "body"), placement = "bottom", trigger = "hover"),
    shinyBS::bsTooltip(id = "lbl_guideLink",
                       title = paste0("User Guide"),
                       options = list(container = "body"), placement = "bottom", trigger = "hover"),
    shinyBS::bsTooltip(id = "lbl_videoLink",
                       title = paste0("User Guide (video)"),
                       options = list(container = "body"), placement = "bottom", trigger = "hover"),
    
  )
  
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bootstrapPage(
      useShinyjs(),
      #extendShinyjs(text = jsCode,functions=c("getParams")),
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
    
  )
}


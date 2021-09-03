#' bird_densities UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bird_densities_ui <- function(id,specID,seasonid){
  fullid = paste(id,specID,seasonid,sep="_")
  newid <- stringr::str_replace_all(fullid," ","_")
  newid <- stringr::str_replace_all(newid,"-","_")
  ns <- NS(newid)
  tagList(
          data.frame(count=0, countSD = 0) %>%
          rhandsontable(rowHeaders=NULL, colHeaders = c(paste(seasonid,"count (# birds)"), paste("SD",seasonid,"count"))) %>%
          hot_cols(colWidths = 150) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE),
  )
}
    
#' bird_densities Server Function
#'
#' @noRd 
mod_bird_densities_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_bird_densities_ui("bird_densities_ui_1")
    
## To be copied in the server
# callModule(mod_bird_densities_server, "bird_densities_ui_1")
 

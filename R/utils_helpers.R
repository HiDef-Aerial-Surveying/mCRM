#' @importFrom  purrr possibly
#' @importFrom  msm qtnorm
#' @export
qtnorm_possibly <- purrr::possibly(msm::qtnorm, otherwise = 0)

#' @importFrom  purrr possibly
#' @importFrom  data.table fread
#' @export
fread_possibly <- purrr::possibly(data.table::fread, otherwise = NULL)

#' Function to create icons and labels with info about parameters
#' @export
label.help <- function(label, id){
  HTML(paste0(label, actionLink(id,label=NULL,icon=icon('info-circle'))))
}

#' generate a temporary folder name for the current session to write out the simulation output files
#' @export
getTempFolderName <- function(){
  paste0("sessionOutputs_", paste(sample(c(letters, LETTERS, 0:9), size = 12, replace = TRUE), collapse = ""))
}

#' @export
`%!in%` <- Negate(`%in%`)


#' little safety function required to surpass the "Error:" value returned when ref points or resampled user bird density data is missing - 
#' used when checking/preparing data for modelling
#' @importFrom  purrr possibly
#' 
#' 
#' 
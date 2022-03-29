#' Function to create icons and labels with info about parameters
#' @export
label.help <- function(label, id){
  HTML(paste0(label, actionLink(id,label=NULL,icon=icon('info-circle'))))
}

#' Negating the '%in%' function for use in tools.
#' @export
`%!in%` <- Negate(`%in%`)

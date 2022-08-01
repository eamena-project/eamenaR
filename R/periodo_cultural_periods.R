#' Convert Perio.do CSV into an interactive data.table
#' @name periodo_cultural_periods
#' @description read a release of the Perio.do cultural periods thesaurus
#' and creates an interactive dataframe
#'
#' @param data.name the name of the data.table. Only useful if export.plot is TRUE
#' @param periodo.url the URL of the Perio.do CSV.
#' @param export.plot if TRUE, export the plot, if FALSE will only display it
#' @param dirOut the folder where the outputs will be saved. By default: '/results'.
#' If it doesn't exist, it will be created. Only useful is export plot is TRUE
#'
#' @return A DT data.frame of the Periodo cultural periods
#'
#' @examples
#'
#'
#' @export
list_cultural_periods <- function(data.name = "periodo",
                                  periodo.url = 'https://n2t.net/ark:/99152/p0dataset.csv',
                                  export.plot = F,
                                  dirOut = paste0(system.file(package = "eamenaR"), "/results/")){
  perio.do <- read.csv(periodo.url, fileEncoding = "UTF-8")
  perio.do.dt <- DT::datatable(perio.do)
  if(export.plot){
    dir.create(dirOut, showWarnings = FALSE)
    gout <- paste0(dirOut, data.name,".html")
    htmlwidgets::saveWidget(perio.do.dt,
                            gout)
  } else {
    print(perio.do.dt)
  }
}

#' Basic statistics on a GeoJSON file
#' @name geojson_stat
#' @description Create a distribution map
#'
#' @stat.name the name of the output stat By default "stat".
#' @param geojson.path the path of the GeoJSON file. By default 'caravanserail.geojson'
#' @param stat the statistic that will be computed. By default 'list_fields' (list the fields)
#' @param export.stat if TRUE, export the stats, if FALSE will only display it
#' @param dataOut the folder where the outputs will be saved. By default: '/results'.
#' If it doesn't exist, it will be created. Only useful is export plot is TRUE
#'
#' @return A map interactive (leaflet) or not
#'
#' @examples
#'
#' @export
geojson_stat <- function(stat.name = "stat",
                         geojson.path = paste0(system.file(package = "eamenaR"), "/extdata/caravanserail.geojson"),
                         stat = c("list_fields"),
                         export.stat = F,
                         dataOut = paste0(system.file(package = "eamenaR"), "/results/")
){
  # geojson.path = paste0(system.file(package = "eamenaR"), "/extdata/caravanserail.geojson") ;
  # stat.name = "stat" ; stat = c("list_fields") ; export.stat = F ;
  # dataOut = paste0(system.file(package = "eamenaR"), "/results/")
  ea.geojson <- sf::st_read(geojson.path)
  if("list_fields" %in% stat){
    field.names <- colnames(ea.geojson)[! colnames(ea.geojson) %in% "geometry"]
    if (export.stat) {
      dir.create(dataOut, showWarnings = FALSE)
      tout <- paste0(dataOut, stat.name, ".tsv")
      df <- data.frame(field.names = field.names)
      write.table(df, tout, sep = "\t")
      print(paste(tout, "is exported"))
    } else {
      cat(field.names, sep = "\n")
    }
  }
}


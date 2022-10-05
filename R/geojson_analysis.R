#' Complex interactive statistics on a GeoJSON file
#' @name geojson_analysis
#' @description Complex interactive statistics on a GeoJSON file with the Shiny package
#'
#' @param stat.name the name of the output file. By default "stat".
#' @param geojson.path the path of the GeoJSON file. By default 'caravanserail.geojson'
#' @param ids the IDs of the resources, by default "EAMENA.ID" (n.b: R fieldname format, without spaces)
#' @param stat the statistic that will be computed. By default 'list_fields' (list the fields)
#' The other options are: "list_ids" list EAMENA IDs ; etc.
#' @param export.stat if TRUE return the stats to be stored in a new variable
#' @param write.stat if TRUE, export the stats in a new file, if FALSE will only display it
#' @param dirOut the folder where the outputs will be saved. By default: '/results'.
#' If it doesn't exist, it will be created. Only useful is write.stat is TRUE
#'
#' @return Show or export basic statistics on the GeoJSON file
#'
#' @examples
#'
#' geojson_analysis(stat.name = "factorial analyis", export.stat = T)
#'
#' @export
geojson_analysis <- function(stat.name = "stat",
                         geojson.path = paste0(system.file(package = "eamenaR"),
                                               "/extdata/caravanserail.geojson"),
                         ids = "EAMENA.ID",
                         # stat = c("list_fields"),
                         export.stat = F,
                         write.stat = F,
                         dirOut = paste0(system.file(package = "eamenaR"), "/results/")
){
  ea.geojson <- sf::st_read(geojson.path)
}

# geojson_analysis(stat.name = "factorial analyis", export.stat = T)

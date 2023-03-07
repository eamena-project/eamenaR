#' Complex interactive statistics on GeoJSON file
#'
#' @name geojson_analysis
#'
#' @description Complex interactive statistics on GeoJSON file with Shiny package
#'
#' @param stat.name name of output file. Default "stat".
#' @param geojson.path path of the GeoJSON file. Default 'caravanserail.geojson'.
#' @param ids IDs of resources, by default "EAMENA.ID" (nb: R field name format, without spaces).
#' @param stat statistic to be computed. By default 'list_fields' (list the fields). Alt options: "list_ids" list EAMENA IDs ; etc.
#' @param export.stat if TRUE return stats to be stored in new variable.
#' @param write.stat if TRUE, export stats in new file, if FALSE will only display it.
#' @param dirOut folder where outputs will be saved. By default: '/results'. If doesn't exist, it will be created. Only useful if write.stat is TRUE.
#'
#' @return Show or export basic statistics on GeoJSON file
#'
#' @examples
#'
#' geojson_analysis(stat.name = "factorial analyis", export.stat = T)
#'
#'
geojson_analysis <- function(stat.name = "stat",
                             geojson.path = paste0(system.file(package = "eamenaR"),
                                                   "/extdata/caravanserail.geojson"),
                             # ids = "EAMENA.ID",
                             ids = ref_ids("hp.id"),
                             # stat = c("list_fields"),
                             export.stat = F,
                             write.stat = F,
                             dirOut = paste0(system.file(package = "eamenaR"), "/results/")
){
  ea.geojson <- sf::st_read(geojson.path)
}

# geojson_analysis(stat.name = "factorial analyis", export.stat = T)

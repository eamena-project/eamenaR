# THIS IS THE MERGE BETWEEN geom_stat and geom_boxplot

#' Basic statistics on GeoJSON file like lists or charts
#'
#' @name geojson_stat
#'
#' @description Basic descriptive statistics on GeoJSON file. This function is used by `geojson_format_path()` and is the front counterpart of `ref_hps()` for a Postgres query.
#'
#' @param stat.name name of output file. Default "stat".
#' @param geojson.path path of GeoJSON file. Default 'caravanserail.geojson'
#' @param concept.name name of field used to store IDs. Default `hp.id`.
#' @param stat type of statistic that will be computed. Default `list_fields` (list the fields). Other options are: `list_ids` list EAMENA IDs. Use `stat` to diplay charts like pie chart or histograms, etc., see option `chart.type`
#' @param chart.type either "`pie`" for pie chart, or "`hist`" for histogram, "`radar`" for radar diagrams. Only useful if the option `stat` is set to `stats` (`stat = "stats"`).
#' @param field.names field name on which statistic will be performed. Only useful if the option `stat` is set to `stats` (`stat = "stats"`).
#' @param name of field on which paths will be grouped. For example "route". Will create as many plots as there are different categories. Default NA.
#' @param color.set the RBrewer color set. Default "Set1".
#' @param fig.width,fig.height size of output chart.
#' @param fig.dev format of image: "png" (default), "jpg", "svg", etc.
#' @param export.stat if TRUE return stats to be stored in new variable
#' @param export.plot if TRUE, export tats in new file, if FALSE will only display it
#' @param dirOut folder where outputs will be saved. Default: '/results'. If it doesn't exist, will be created. Only useful is export.plot is TRUE.
#' @param verbose if TRUE (by default), print messages.
#'
#' @return Show or export basic statistics on GeoJSON file
#'
#' @examples
#'
#' # list HP names
#' geojson_stat(stat.name = "geojson_fields", stat = "list_ids")
#'
#'
#' @export
geojson_imagery <- function(geojson.path = paste0(system.file(package = "eamenaR"),
                                                  "/extdata/caravanserail.geojson"),
                            export.plot = FALSE,
                            dirOut = paste0(system.file(package = "eamenaR"),
                                            "/results/"),
                            verbose = TRUE){
  pass
}

# geojson.path <- "C:/Users/Thomas Huet/Desktop/Sistan/data/sistan_part1_hps.geojson"
# geojson_imagery(geojson.path)

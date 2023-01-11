#' Get values of a given field in a GeoJSON file.
#'
#' @name geojson_get_field
#'
#' @description Get all values of a given field.
#'
#' @param geojson.path the path to the GeoJSON file.
#' @param field a field name, by default "EAMENA ID".
#'
#' @return A vector with all values
#'
#' @examples
#'
#' geojson_get_field()
#' ##  [1] "EAMENA-0192281" "EAMENA-0182033" "EAMENA-0182054" "EAMENA-0182055" "EAMENA-0182056" "EAMENA-0182057" "EAMENA-0182058"
#' ##  [8] "EAMENA-0164899" "EAMENA-0164904" "EAMENA-0164906" "EAMENA-0164905" "EAMENA-0164943" "EAMENA-0164999" "EAMENA-0207261"
#' ## ...
#'
#' @export
geojson_get_field <- function(geojson.path = paste0(system.file(package = "eamenaR"),
                                                    "/extdata/caravanserail.geojson"),
                              field = "EAMENA ID"){
  ea.geojson <- geojsonsf::geojson_sf(geojson.path)
  return(ea.geojson[["EAMENA ID"]])
}

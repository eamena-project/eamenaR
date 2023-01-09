#' Get values of a given field in a GeoJSON file.
#'
#' @name geojson_get_field
#'
#' @description Get values of a given field
#'
#' @param geojson.path the path to the GeoJSON file, eg: "https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/geojson/caravanserail.geojson"
#' @param field a field name, in R format, eg: EAMENA.ID
#'
#' @return A vector with all values
#'
#' @examples
#'
#' geojson_get_field(geojson.path = "https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/geojson/caravanserail.geojson")
#'
#' @export
geojson_get_field <- function(geojson.path = paste0(system.file(package = "eamenaR"),
                                                    "/extdata/caravanserail.geojson"),
                              field = "EAMENA ID"){
  ea.geojson <- geojsonsf::geojson_sf(geojson.path)
  return(ea.geojson[["EAMENA ID"]])
  # r <- geojsonio::geojson_read(geojson.path)
  # all.val <- c()
  # for(i in seq(1, length(r[[2]]))){
  #   # print(i)
  #   val <- r[[2]][[i]]$properties[[field]]
  #   # print(val)
  #   # print(is.null(val))
  #   if(is.null(val)){val <- NA}
  #   all.val <- c(all.val, val)
  # }
  # return(all.val)
}

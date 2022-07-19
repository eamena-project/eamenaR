#' Get a list of parameters from a GeoJSON file.
#' @name geojson_get_field
#' @description Get values of a given field
#'
#' @param geojson.path the path to the GeoJSON file,
#' eg: "https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/geojson/caravanserail.geojson"
#' @param field a field name, in R format, eg: EAMENA.ID
#'
#' @return A vector with all values
#'
#' @examples
#'
#' @export
geojson_get_field <- function(geojson.path, field = "EAMENA.ID"){
  # geojson.path <- "https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/geojson/caravanserail.geojson"
  # field = "Cultural.Sub.period.Type"
  r <- geojson_read(geojson.path)
  all.val <- c()
  for(i in seq(1, length(r[[2]]))){
    # print(i)
    val <- r[[2]][[i]]$properties[[field]]
    # print(val)
    # print(is.null(val))
    if(is.null(val)){val <- NA}
    all.val <- c(all.val, val)
  }
  return(all.val)
}
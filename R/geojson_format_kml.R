#' Convert from/to GeoJSON, KMZ or KML
#'
#' @name geojson_format_kml
#' @description Convert from KML/KMZ to GeoJSON, or from GeoJSON to KML. Geometries drawn in Google Earth or Google Earth pro can be exported as KML or KMZ (ie, compressed KML). At the same time, geometries drawn in EAMENA can be exported as GeoJSON. For a given HP, this data can be then imported into the EAMENA DB.
#'
#' @param geom.path the path to the KML, KMZ or GeoJSON file.
#' @param geom.types the types of geometries ("POINT", "LINE" or "POLYGON") that will be selected. By default all: `c("POINT", "LINE", "POLYGON")`.
#' @param export if TRUE, will export the KML/KMZ file in a GeoJSON format, or the GeoJSON file as a KML, if FALSE simple plot.
#' @param dirOut the path to the folder where the KML/KMZ/GeoJSON file will be created.
#' @param geojson.name the name of the KML/KMZ/GeoJSON that will be created without the extension
#' @param verbose if TRUE (by default) then display different messages.
#'
#' @return a GeoJSON file or a KML file
#'
#' @examples
#'
#' library(dplyr)
#'
#' # from KMZ to GeoJSON
#' geojson_format_kml(geom.path = "C:/Rprojects/eamenaR/inst/extdata/Waypoints.kmz",
#'                    dirOut = "C:/Rprojects/eamenaR/inst/extdata/",
#'                    geom.types = "POLYGON",
#'                    export = T,
#'                    geojson.name = "Waypoints_outGeoJSON")
#'
#' # from GeoJSON to KML
#' geojson_format_kml(geom.path = "C:/Rprojects/eamenaR/inst/extdata/kites.geojson",
#'                    dirOut = "C:/Rprojects/eamenaR/inst/extdata/",
#'                    export = T,
#'                    geojson.name = "kites_outKML")
#'
#' @export
geojson_format_kml <- function(geom.path = paste0(system.file(package = "eamenaR"),
                                                  "/extdata/Waypoints.kmz"),
                               geom.types = c("POINT", "LINE", "POLYGON"),
                               export = T,
                               dirOut = paste0(system.file(package = "eamenaR"),
                                               "/extdata/"),
                               geojson.name = "Waypoints",
                               verbose = T){
  ext <- DescTools::SplitPath(geom.path)$extension

  # path <- "C:/Rprojects/eamenaR/inst/extdata/"
  # In <- "kites.geojson"
  # pathIn <- paste0(path, In)
  # pathOut <- paste0(path, DescTools::SplitPath(In)$filename, ".kml")
  # xx <- sf::st_read()
  # sf::st_write(xx, pathOut, driver = "kml", delete_dsn = TRUE)

  if(ext == "geojson"){
    geom <- sf::st_read(geom.path)
    toGeom <- ".kml"
  }
  if(ext == "kmz"){
    # extract/de-compress gives a KML
    td <- tempdir()
    KML <- unzip(geom.path, exdir = td, junkpaths = TRUE)
    geom <- sf::st_read(KML)
    toGeom <- ".geojson"
  }
  if(ext == "kml"){
    geom <- sf::st_read(geom.path)
    toGeom <- ".geojson"
  }
  # td <- tempdir()
  # # extract/de-compress gives a KML
  # KML <- unzip(geom.path, exdir = td, junkpaths = TRUE)
  # geom <- sf::st_read(KML)
  selectedGeom <- paste0(geom.types,  collapse = "|")
  geom <- geom %>%
    filter(grepl(selectedGeom, sf::st_geometry_type(geometry)))
  ## check if is point
  # is.POINT <- grepl("POINT", st_as_text(geom$geometry))
  if(export){
    if(ext == "kmz" | ext == "kml"){
      sf::st_write(geom,
                   paste0(dirOut, geojson.name, toGeom),
                   delete_dsn = TRUE)
    }
    if(ext == "geojson"){
      sf::st_write(geom,
                   paste0(dirOut, geojson.name, toGeom),
                   driver = "kml",
                   delete_dsn = TRUE)
    }
  } else {
    plot(geom)
  }
}

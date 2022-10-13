#' Extracting KMZ or KML coordinates from a file and addin this coordinates into EAMENA for a same HP
#'
#' @name geom_kml
#' @description Geometries drawn in Google Earth or Google Earth pro can be exported as KML or KMZ (ie, compressed KML). For a given HP, this data can be then imported into the EAMENA DB.
#'
#' @param kml.path the path to the KML or KMZ file.
#' @param geom.types the types of geometries ("POINT", "LINE" or "POLYGON") that will be selected. By default all: `c("POINT", "LINE", "POLYGON")`.
#' @param export if TRUE, will export the KML/KMZ file in a GeoJSON format.
#' @param dirOut the path to the folder where the GeoJSON file will be created.
#' @param geojson.name the name of the GeoJSON that will be created.
#' @param verbose if TRUE (by default) then display different messages.
#'
#' @return a GeoJSON file
#'
#' @examples
#'
#' geom_kml(kml.path = "C:/Rprojects/eamenaR/inst/extdata/Waypoints.kmz",
#'          dirOut = "C:/Rprojects/eamenaR/inst/extdata/",
#'          geom.types = "POLYGON",
#'          export = T,
#'          geojson.name = "Waypoints.geojson")
#'
#' @export
geom_kml <- function(kml.path = paste0(system.file(package = "eamenaR"),
                                       "/extdata/Waypoints.kmz"),
                     geom.types = c("POINT", "LINE", "POLYGON"),
                     export = T,
                     dirOut = paste0(system.file(package = "eamenaR"),
                                     "/extdata/"),
                     geojson.name = "Waypoints.geojson",
                     verbose = T){
  ext <- DescTools::SplitPath(kml.path)$extension
  if(ext == "kmz"){
    # extract/de-compress gives a KML
    td <- tempdir()
    KML <- unzip(kml.path, exdir = td, junkpaths = TRUE)
    geom <- sf::st_read(KML)
  }
  if(ext == "kml"){
    geom <- sf::st_read(kml.path)
  }
  # td <- tempdir()
  # # extract/de-compress gives a KML
  # KML <- unzip(kml.path, exdir = td, junkpaths = TRUE)
  # geom <- sf::st_read(KML)
  selectedGeom <- paste0(geom.types,  collapse = "|")
  geom <- geom %>%
    filter(grepl(selectedGeom, st_geometry_type(geometry)))
  ## check if is point
  # is.POINT <- grepl("POINT", st_as_text(geom$geometry))
  if(export){
    st_write(geom,
             paste(dirOut, geojson.name),
             delete_dsn = TRUE)
  }
}

geom_kml(kml.path = "C:/Rprojects/eamenaR/inst/extdata/Waypoints.kmz",
         geom.types = "POLYGON",
         dirOut = "C:/Rprojects/eamenaR/inst/extdata/",
         export = T,
         geojson.name = "Waypoints.geojson")



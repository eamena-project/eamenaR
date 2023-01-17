#' Convert between GeoJSON and KMZ/KML
#'
#' @name geojson_kml
#'
#' @description Convert from KML/KMZ to GeoJSON, or from GeoJSON to KML. Geometries drawn in Google Earth or Google Earth pro can be exported as KML or KMZ (ie, compressed KML). At the same time, geometries drawn in EAMENA can be exported as GeoJSON. For a given HP, this data can be then imported into the EAMENA DB.
#'
#' @param geom.path the path to the KML, KMZ or GeoJSON file.
#' @param geom.types the types of geometries ("POINT", "LINE" or "POLYGON") that will be selected. By default all: `c("POINT", "LINE", "POLYGON")`.
#' @param export if TRUE, will export the KML/KMZ file in a GeoJSON format, or the GeoJSON file as a KML, if FALSE simple plot.
#' @param dirOut the path to the folder where the KML/KMZ/GeoJSON file will be created.
#' @param geojson.name the name of the KML/KMZ/GeoJSON that will be created without the extension
#' @param select.name for KML export only. The field selected to be the KML name of the HP, by default "EAMENA ID".
#' @param select.fields for KML export only. KML conversion remove a large number of GeoJSON fields. This variable is used to select the fields we want to preserve. By default: c("EAMENA ID","Resource Name", "resourceid").
#' @param verbose if TRUE (by default) then display different messages.
#'
#' @return a GeoJSON file or a KML file, depending on the input file.
#'
#' @examples
#'
#' library(dplyr)
#'
#' # from KMZ to GeoJSON, with the default 'Waypoints.kmz' input file
#' library(dplyr)
#' geojson_kml(geom.types = "POINT")
#'
#' # from GeoJSON to KML
#' geojson_kml(geom.path = "C:/Rprojects/eamenaR/inst/extdata/kites.geojson",
#'                    dirOut = "C:/Rprojects/eamenaR/inst/extdata/",
#'                    export = T,
#'                    geojson.name = "kites_outKML")
#'
#' @export
geojson_kml <- function(geom.path = paste0(system.file(package = "eamenaR"),
                                           "/extdata/Waypoints.kmz"),
                        geom.types = c("POINT", "LINE", "POLYGON"),
                        export = T,
                        dirOut = paste0(system.file(package = "eamenaR"),
                                        "/extdata/"),
                        geojson.name = "Waypoints",
                        select.name = "EAMENA ID",
                        select.fields = c("EAMENA ID", "Resource Name", "resourceid"),
                        verbose = T){
  ext <- DescTools::SplitPath(geom.path)$extension
  if(verbose){print(paste0("*read: ", geom.path))}
  if(ext == "geojson"){
    geom <- geojsonsf::geojson_sf(geom.path)
    # geom <- sf::st_read(geom.path, quiet = TRUE)
    toGeom <- ".kml"
  }
  if(ext == "kmz"){
    # extract/de-compress gives a KML
    td <- tempdir()
    KML <- unzip(geom.path, exdir = td, junkpaths = TRUE)
    geom <- sf::st_read(KML, quiet = TRUE)
    toGeom <- ".geojson"
  }
  if(ext == "kml"){
    geom <- sf::st_read(geom.path, quiet = TRUE)
    toGeom <- ".geojson"
  }
  if(verbose){print(paste0(" ... done"))}
  selectedGeom <- paste0(geom.types,  collapse = "|")
  if(verbose){print(paste0("Filter on selected geometries: ", selectedGeom))}
  geom <- geom %>%
    filter(grepl(selectedGeom, sf::st_geometry_type(geometry)))
  if(export){
    outFile <- paste0(dirOut, geojson.name, toGeom)
    if(ext == "kmz" | ext == "kml"){
      sf::st_write(geom,
                   outFile,
                   delete_dsn = TRUE)
    }
    if(ext == "geojson"){
      geom <- geom[ , select.fields] # subset of fields for KML
      # # tried also this
      # geom.as <- as(geom, "Spatial")
      # maptools::kmlPoints(
      #   obj = geom.as,
      #   kmlfile = paste0(dirOut, geojson.name, toGeom),
      #   kmlname = geojson.name,
      #   kmldescription = "",
      #   name = geom.as$EAMENA.ID,
      #   description = "",
      #   icon = "http://www.gstatic.com/mapspro/images/stock/962-wht-diamond-blank.png"
      # )
      # tried that
      geom$name <- geom[[select.name]]
      # geom$Description <- geom[[selectedDescription]]
      # geom$Description <- geom$resourceid
      sf::st_write(geom,
                   outFile,
                   driver = "kml",
                   delete_dsn = TRUE)
      # # tried this:
      # geom.as <- as(geom, "Spatial")
      # rgdal::writeOGR(geom.as,
      #                 paste0(dirOut, geojson.name, "ZZZ", toGeom),
      #                 driver="KML",
      #                 layer="poly")
    }
    if(verbose){print(paste0("Exported file: ", outFile))}
  } else {
    plot(geom)
  }
}

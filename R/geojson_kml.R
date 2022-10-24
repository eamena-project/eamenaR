#' Convert from/to GeoJSON, KMZ or KML
#'
#' @name geojson_kml
#' @description Convert from KML/KMZ to GeoJSON, or from GeoJSON to KML. Geometries drawn in Google Earth or Google Earth pro can be exported as KML or KMZ (ie, compressed KML). At the same time, geometries drawn in EAMENA can be exported as GeoJSON. For a given HP, this data can be then imported into the EAMENA DB.
#'
#' @param geom.path the path to the KML, KMZ or GeoJSON file.
#' @param geom.types the types of geometries ("POINT", "LINE" or "POLYGON") that will be selected. By default all: `c("POINT", "LINE", "POLYGON")`.
#' @param export if TRUE, will export the KML/KMZ file in a GeoJSON format, or the GeoJSON file as a KML, if FALSE simple plot.
#' @param dirOut the path to the folder where the KML/KMZ/GeoJSON file will be created.
#' @param geojson.name the name of the KML/KMZ/GeoJSON that will be created without the extension
#' @param selectedName for KML export only. The field selected to be the KML name of the HP, by default "EAMENA.ID".
#' @param selectedFields for KML export only. KML conversion remove a large number of GeoJSON fields. This variable is used to select the fields we want to preserve. By default: c("EAMENA.ID","Resource.Name", "resourceid").
#' @param verbose if TRUE (by default) then display different messages.
#'
#' @return a GeoJSON file or a KML file
#'
#' @examples
#'
#' library(dplyr)
#'
#' # from KMZ to GeoJSON
#' geojson_kml(geom.path = "C:/Rprojects/eamenaR/inst/extdata/Waypoints.kmz",
#'                    dirOut = "C:/Rprojects/eamenaR/inst/extdata/",
#'                    geom.types = "POLYGON",
#'                    export = T,
#'                    geojson.name = "Waypoints_outGeoJSON")
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
                        selectedName = "EAMENA.ID",
                        selectedFields = c("EAMENA.ID","Resource.Name", "resourceid"),
                        verbose = T){
  ext <- DescTools::SplitPath(geom.path)$extension
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
  selectedGeom <- paste0(geom.types,  collapse = "|")
  geom <- geom %>%
    filter(grepl(selectedGeom, sf::st_geometry_type(geometry)))
  if(export){
    if(ext == "kmz" | ext == "kml"){
      sf::st_write(geom,
                   paste0(dirOut, geojson.name, toGeom),
                   delete_dsn = TRUE)
    }
    if(ext == "geojson"){
      geom <- geom[ , selectedFields] # subset of fields for KML
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
      geom$name <- geom[[selectedName]]
      # geom$Description <- geom[[selectedDescription]]
      # geom$Description <- geom$resourceid
      sf::st_write(geom,
                   paste0(dirOut, geojson.name, toGeom),
                   driver = "kml",
                   delete_dsn = TRUE)
      # # tried this:
      # geom.as <- as(geom, "Spatial")
      # rgdal::writeOGR(geom.as,
      #                 paste0(dirOut, geojson.name, "ZZZ", toGeom),
      #                 driver="KML",
      #                 layer="poly")
    }
  } else {
    plot(geom)
  }
}
#

# # # from GeoJSON to KML
# library(dplyr)
# geojson_kml(geom.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail.geojson",
#             dirOut = "C:/Users/Thomas Huet/Desktop/GE-EAMENA/Waypoints/",
#             export = T,
#             geom.types = c("POINT"),
#             geojson.name = "caravanserail_outKML")

# # # from GeoJSON to KML
# library(dplyr)
# geojson_kml(geom.path = "C:/Users/Thomas Huet/Desktop/GE-EAMENA/Waypoints/caravanserail_outKML2.kml",
#             dirOut = "C:/Users/Thomas Huet/Desktop/GE-EAMENA/Waypoints/",
#             export = T,
#             geom.types = c("POLYGON"),
#             geojson.name = "caravanserail_outGeoJSON")

#' Convert between GeoJSON and SHP
#'
#' @name geojson_shp
#'
#' @description Convert from SHP (shapefile) to GeoJSON, or from GeoJSON to SHP.
#'
#' @param geom.path the path to the SHP/GeoJSONfile.
#' @param geom.types the types of geometries ("POINT", "LINE" or "POLYGON") that will be selected. By default all: `c("POINT", "LINE", "POLYGON")`.
#' @param export if TRUE, will export the SHP file in a GeoJSON format, or the GeoJSON file as a SHP, if FALSE simple plot.
#' @param dirOut the path to the folder where the SHP/GeoJSON file will be created.
#' @param geojson.name the name of the SHP/GeoJSON that will be created without the extension
#' @param select.name for SHP export only. The field selected to be the SHP name of the HP, by default `"EAMENA ID"`.
#' @param select.fields for SHP export only. SHP conversion remove a large number of GeoJSON fields. This variable is used to select the fields we want to preserve. By default: `c("EAMENA ID","Resource Name", "resourceid")`.
#' @param verbose if TRUE (by default) then display different messages.
#'
#' @return a GeoJSON file or a SHP file, depending on the input file.
#'
#' @examples
#'
#' library(dplyr)
#'
#' # from GeoJSON to SHP
#' geojson_shp(geom.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail.geojson",
#'             geom.types = "POLYGON",
#'             dirOut = "C:/Rprojects/eamenaR/results/",
#'             export = T,
#'             geojson.name = "caravanserailOUT")
#'
#' @export
geojson_shp <- function(geom.path = paste0(system.file(package = "eamenaR"),
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
    toGeom <- ".shp"
  }
  if(ext == "shp"){
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
    if(ext == "shp"){
      sf::st_write(geom,
                   outFile,
                   delete_dsn = TRUE)
    }
    if(ext == "geojson"){
      geom <- geom[ , select.fields] # subset of fields for SHP
      geom$name <- geom[[select.name]]
      sf::st_write(geom,
                   outFile,
                   driver = "ESRI Shapefile",
                   delete_dsn = TRUE)
    }
    if(verbose){print(paste0("Exported file: ", outFile))}
  } else {
    plot(geom)
  }
}

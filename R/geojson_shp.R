#' Convert between GeoJSON and SHP
#'
#' @name geojson_shp
#'
#' @description Convert from SHP (shapefile) to GeoJSON, or from GeoJSON to SHP.
#'
#' @param geom.path path to the SHP/GeoJSONfile.
#' @param geom.types types of geometries ("POINT", "LINE", "POLYGON") that will be selected. Default all: `c("POINT", "LINE", "POLYGON")`.
#' @param export if TRUE, will export SHP file in GeoJSON format, or GeoJSON file as SHP, if FALSE simple plot.
#' @param dirOut path to folder where SHP/GeoJSON file will be created. Default, 'results/'.
#' @param geojson.name name of SHP/GeoJSON that will be created without extension
#' @param select.name for SHP export only. Field selected to be SHP name of the heritage place, default `"EAMENA ID"`.
#' @param select.fields for SHP export only. SHP conversion remove large number of GeoJSON fields. This variable is used to select fields to preserve. Default: `c("EAMENA ID","Resource Name", "resourceid")`.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return GeoJSON file or SHP file, depending on input file.
#'
#' @examples
#'
#' library(dplyr)
#'
#' # GeoJSON to SHP, only POINTS
#' geojson_shp(geom.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail.geojson",
#'             geom.types = "POINT",
#'             dirOut = "C:/Rprojects/eamenaR/results/",
#'             export = T,
#'             geojson.name = "caravanserailOUT")
#'
#'
#' # GeoJSON to SHP, only POLYGONS
#'library(dplyr)
#'
#'geojson_shp(geom.types = "POLYGON",
#'            geojson.name = "cvns-polygons",
#'            geom.path = paste0(system.file(package = "eamenaR"),
#'                               "/extdata/caravanserail.geojson"),
#'            dirOut = "C:/Rprojects/eamena-gee/data/")
#'
#' @export
geojson_shp <- function(geom.path = paste0(system.file(package = "eamenaR"),
                                           "/extdata/Waypoints.kmz"),
                        geom.types = c("POINT", "LINE", "POLYGON"),
                        export = TRUE,
                        dirOut = paste0(system.file(package = "eamenaR"),
                                        "/results/"),
                        geojson.name = "Waypoints",
                        select.name = "EAMENA ID",
                        select.fields = c("EAMENA ID", "Resource Name", "resourceid"),
                        verbose = TRUE){
  # `%>%` <- dplyr::`%>%` # used to not load dplyr
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
    dplyr::filter(grepl(selectedGeom, sf::st_geometry_type(geometry)))
  if(export){
    dir.create(dirOut, showWarnings = FALSE)
    outFile <- paste0(dirOut, geojson.name, toGeom)
    if(ext == "shp"){
      if(verbose){print(paste0("Shapefile -> GeoJSON", outFile))}
      sf::st_write(geom,
                   outFile,
                   delete_dsn = TRUE)
    }
    if(ext == "geojson"){
      if(verbose){print(paste0("GeoJSON -> Shapefile", outFile))}
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

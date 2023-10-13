#' Convert different types of files (GeoJSON, CSV, etc.) into an appropriate structure for a Bulk Upload append procedure
#'
#' @name list_mapping_bu_append
#'
#' @description Function allows file format conversion from GeoJSON to CSV. Output CSV file can be imported into EAMENA DB with bulk upload append procedure.
#'
#' @param fileIn path to the file. Default: path to a GeoJSON file which comes from the `geojson_kml()` function.
#' @param method the type of transformation that will be done. Default `geom`: Convert from GeoJSON to CSV. Other transformations: `ir` for Information Resources.
#' @param export if TRUE (default), will export GeoJSON to CSV file.
#' @param dirOut path to folder where CSV file will be created.
#' @param csv.name name of CSV that will be created without the extension
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return a CSV file
#'
#' @examples
#'
#' ## Geometries
#' list_mapping_bu_append()
#'
#' ## List of related IR (HP <-> IR)
#'
#'
#' @export
list_mapping_bu_append <- function(fileIn = paste0(system.file(package = "eamenaR"),
                                                   "/extdata/caravanserail_outGeoJSON.geojson"),
                                   method = "geom",
                                   export = T,
                                   dirOut = paste0(system.file(package = "eamenaR"),
                                                   "/extdata/"),
                                   csv.name = "caravanserail_outCSV",
                                   verbose = T){
  if(verbose){print(paste0("*read: ", fileIn))}
  if(method = "geom"){
    if(verbose){print(paste0("Works with Geometries"))}
    geom <- sf::st_read(fileIn, quiet = TRUE)
    geom.noZ <- sf::st_zm(geom)
    if(verbose){print(paste0(" ... Z-dim is removed"))}
    n <- nrow(geom.noZ)
    df <- data.frame("ResourceID" = geom.noZ$Name,
                     "Geometric Place Expression" = sf::st_as_text(geom.noZ$geometry),
                     # TODO: modify these values
                     "Location Certainty" = rep("High", n),
                     "Geometry Extent Certainty" = rep("High", n),
                     "Geometry Type" = rep("Perimeter Polygon", n),
                     check.names = FALSE)
    outCSV <- paste0(dirOut, csv.name, ".csv")
    write.table(df, outCSV,
                row.names = FALSE, sep = ",")
    if(verbose){print(paste0("Exported to: ", outCSV))}
  }
  if(method = "geom"){
    if(verbose){print(paste0("Works with List of related Heritage Places"))}

  }
}

# list_mapping_bu_append(fileIn = "https://raw.githubusercontent.com/eamena-project/eamenaR/main/inst/extdata/information_resources_list.csv",
#                        method = "geom",)

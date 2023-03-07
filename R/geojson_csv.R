#' Convert from GeoJSON to CSV
#'
#' @name geojson_csv
#'
#' @description Function allows file format conversion from GeoJSON to CSV. Output CSV file can be imported into EAMENA DB with bulk upload append procedure.
#'
#' @param geom.path path to GeoJSON file. File comes from the `geojson_kml()` function.
#' @param export if TRUE (default), will export GeoJSON to CSV file.
#' @param dirOut path to folder where CSV file will be created.
#' @param csv.name name of CSV that will be created without the extension
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return a CSV file
#'
#' @examples
#'
#' geojson_csv()
#'
#' @export
geojson_csv <- function(geom.path = paste0(system.file(package = "eamenaR"),
                                           "/extdata/caravanserail_outGeoJSON.geojson"),
                        export = T,
                        dirOut = paste0(system.file(package = "eamenaR"),
                                        "/extdata/"),
                        csv.name = "caravanserail_outCSV",
                        verbose = T){
  if(verbose){print(paste0("*read: ", geom.path))}
  geom <- sf::st_read(geom.path, quiet = TRUE)
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

#' Join the GeoJSON file of heritage places (nodes) with the CSV file of routes (edges)
#'
#' @name ref_routes
#'
#' @description Join the GeoJSON file of heritage places with the CSV file of routes. Each heritage place has its route (the `by` option) registered in a CSV file. This function creates a merge of these two files
#'
#' @param geojson.path the path of the GeoJSON file. By default 'caravanserail.geojson'
#' @param csv.path the path to the CSV where the edges between two heritage places are recorded. By default 'caravanserail_paths.csv'.
#' @param concept.name the concept that will be retrieve from the `ids.csv` file. By default "hp.id".
#' @param by the field in the CSV file
#' @param verbose if TRUE (by default), print messages.
#'
#' @return A `sf` file of the heritage places with their routes.
#'
#' @examples
#'
#'
#' @export
ref_routes <- function(geojson.path = paste0(system.file(package = "eamenaR"),
                                             "/extdata/caravanserail.geojson"),
                       csv.path = paste0(system.file(package = "eamenaR"),
                                         "/extdata/caravanserail_paths.csv"),
                       concept.name = "hp.id",
                       by = NA,
                       verbose = TRUE){
  r.id <- eamenaR::ref_ids(concept.name)
  # read the CSV file
  paths <- read.csv(csv.path)
  df1 <- paths[ , c("from", by)]
  names(df1) <- c(r.id, by)
  df2 <- paths[ , c("to", by)]
  names(df2) <- c(r.id, by)
  df <- rbind(df1, df2)
  df <- df[!duplicated(df), ]
  # read the heritage places
  # ea.geojson <- geojsonsf::geojson_sf(geojson.path)
  if(inherits(geojson.path, "sf")){
    if(verbose){
      print(paste0("Reads a 'sf' dataframe"))
    }
    ea.geojson <- geojson.path
  }
  if(is.character(geojson.path)){
    if(verbose){
      print(paste0("Reads a path"))
    }
    # hp.geom.sf <- geojsonsf::geojson_sf(geojson.path)
    ea.geojson <- sf::st_read(geojson.path)
  }
  # remove leading/trailing spaces
  names(ea.geojson) <- trimws(colnames(ea.geojson))
  ea.geojson <- merge(ea.geojson, df, by = r.id)
  if(verbose){
    if(is.character(geojson.path)){
      print(paste0("merge of '", DescTools::SplitPath(geojson.path)$fullfilename,
                   "' and '", DescTools::SplitPath(csv.path)$fullfilename,
                   "' has been done on the field '", by,"'"))}
  }
  return(ea.geojson)
}

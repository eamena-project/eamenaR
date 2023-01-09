#' Test if a resource (HP) geometry is within a Grid Square (gs).
#'
#' @name geom_within_gs
#'
#' @description Test if the geometry of a resource (eg. Heritage Place) is within a Grid Square (gs). If so, return the ID of the Grid Square.
#' @param resource.wkt the WKT geometry of a resource, as a character format. This WKT geometry can comes from a BU sheet (ex: "POINT(0.9 35.8)").
#' @param gs.path a path to a GeoJSON file, by default to the example 'grid_squares.geojson' This GeoJSON is an EAMENA output of the Grids as a GeoJSON URL. The name of the grids can be calculated with the \link[eamenaR]{geom_bbox} function. The GeoJSON is read and convert to a `sf` object.
#' @param verbose if TRUE (by default): verbose.
#'
#' @details when this function is called for a dataframe, it has to be nested into a loop.
#'
#' @return the ID of the Grid Square for each WKT geometries.
#'
#' @examples
#'
#' library(dplyr)
#'
#' geom_within_gs(resource.wkt = "POINT(0.9 35.8)")
#'
#'
#' @export
geom_within_gs <- function(resource.wkt = NA,
                           gs.path = paste0(system.file(package = "eamenaR"),
                                            "/extdata/grid_squares.geojson"),
                           verbose = TRUE){
  err <-  flag <- 0
  gs.sf <- sf::st_read(gs.path, quiet = T)
  resource.geom <- data.frame(wkt = resource.wkt)
  tryCatch({
    resource.sf <- sf::st_as_sf(resource.geom, wkt = "wkt")
  },
  error = function(cond) {
    err <<- 1
  }
  )
  if(err == 0){
    for(gs in seq(1, nrow(gs.sf))){
      grid.square.wkt <- gs.sf$geometry[[gs]]
      is.within <- sf::st_within(resource.sf, grid.square.wkt) %>%
        lengths > 0
      if(is.within){
        flag <- 1
        if(verbose){print("OK: this geometry exists in the grid squares")}
        return(gs.sf$Grid.ID[[gs]])
      }
    }
    if(!flag) {
      missed <- paste0("this geometry is not in '",
                       DescTools::SplitPath(gs.path)$fullfilename,
                       "'")
      warnings(missed)
    }
  }
  if(err == 1){
    return(NA)
    }
}


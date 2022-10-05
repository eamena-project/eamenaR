#' Test if a resource (HP) geometry is within a Grid Square.
#'
#' @name geom_within_gs
#' @description Test if the geometry of a resource (eg. Heritage Place) is within a Grid Square (gs). If so, return the ID of the Grid Square.
#' @param resource.wkt the WKT geometry of a resource, as a character format. This WKT geometry can comes from a BU sheet (ex: "POINT(0.9 35.8)").
#' @param grid.squares.path a path to a GeoJSON file, by default to the example 'grid_squares.geojson' This GeoJSON is an EAMENA output of the Grids as a GeoJSON URL. The name of the grids can be calculated with the \link[eamenaR]{geom_bbox} function. The GeoJSON is read and convert to a `sf` object. This GeoJSON
#'
#' @return the ID of the Grid Square
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
                           grid.squares.path = paste0(system.file(package = "eamenaR"),
                                                      "/extdata/grid_squares.geojson")){
  flag <- 0
  grid.squares.sf <- sf::st_read(grid.squares.path, quiet = T)
  resource.geom <- data.frame(wkt = resource.wkt)
  resource.sf <- sf::st_as_sf(resource.geom, wkt = "wkt")
  for(gs in seq(1, nrow(grid.squares.sf))){
    grid.square.wkt <- grid.squares.sf$geometry[[gs]]
    is.within <- sf::st_within(resource.sf, grid.square.wkt) %>%
      lengths > 0
    if(is.within){
      flag <- 1
      return(grid.squares.sf$Grid.ID[[gs]])
    }
  }
  if(!flag) {
    missed <- paste0("this geometry is not in '",
                     DescTools::SplitPath(grid.squares.path)$fullfilename,
                     "'")
    return(missed)
  }
}

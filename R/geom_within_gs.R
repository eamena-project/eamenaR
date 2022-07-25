#' Test if a resource (HP) geometry is within a Grid Square.
#' @name geom_within_gs
#' @description Test if the geometry of a resource (eg. Heritage Place) is
#' within a Grid Square (gs). If so, return the ID of the Grid Square
#'
#' @param resource.wkt the WKT geometry of a resource, as a character format. This
#' WKT geometry can comes from a BU sheet (ex: "POINT(0.9 35.8)")
#' @param grid.squares.path a path to a GeoJSON file. This GeoJSON is an
#' EAMENA output of the Grids as a GeoJSON URL. The GeoJSON is read and comvert to a sf object
#'
#' @return the ID of the Grid Square
#'
#' @examples
#'
#' @export
geom_within_gs <- function(resource.wkt = NA,
                           grid.squares.path = paste0(system.file(package = "eamenaR"), "/extdata/grid_squares.geojson")){
  flag <- 0
  grid.squares.sf <- sf::st_read(grid.squares.path)
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

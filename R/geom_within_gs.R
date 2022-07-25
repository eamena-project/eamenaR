#' Test if a resource (HP) geometry is within a Grid Square.
#' @name geom_within_gs
#' @description Test if the geometry of a resource (eg. Heritage Place) is
#' within a Grid Square (gs). If so, return the ID of the Grid Square
#'
#' @param resource.wkt the WKT geometry of a resource, as a character format. This
#' WKT geometry can comes from a BU sheet (ex: POINT(0.916350216921341 35.9625191284127))
#' @param grid_squares a path to a GeoJSON file. This GeoJSON is the
#' EAMENA output of the GeoJSON URL. The GeoJSON is read and comvert to a sf object
#'
#' @return the ID of the Grid Square
#'
#' @examples
#'
#' @export
geom_within_gs <- function(resource.wkt = NA, grid.squares.sf = NA){
  # grid_squares <- geojson_read(geojson.path)
  grid.squares.sf <- sf::st_read(geojson.path)
  resource.geom <- data.frame(wkt = resource.wkt)
  # resource.geom <- data.frame(wkt = 'POINT(10 10)')
  resource.sf <- sf::st_as_sf(resource.geom, wkt = "wkt")
  for(gs in seq(1, nrow(grid.squares.sf))){
    # gs <- 1
    # grid.square.id <- grid.squares.sf$Grid.ID[[gs]]
    grid.square.wkt <- grid.squares.sf$geometry[[gs]]
    # is.within <- st_within(resource.sf, grid.square.wkt)
    is.within <- sf::st_within(resource.sf, grid.square.wkt) %>% lengths > 0
    if(is.within){
      return(grid.squares.sf$Grid.ID[[gs]])
    }
    # grid.square.geom <- data.frame(wkt = grid.square.wkt)
    # grid.square.sf <- st_as_sf(grid.square.geom, wkt = "wkt")
  }
}

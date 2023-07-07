#' Divide a Grid Square into a selected number of subgrids
#'
#' @name geojson_grid
#'
#' @description Divide a Grid Square (GS) into a selected number of subgrids (rows, cols). This function is useful for anyone who want to survey systematicaly a GS (~ 24km * 28 km) by dividing this GS in many smaller grids (numbered from GS.1 to GS.n). By default, GS are divided into 140 rows and 60 columns to fit with a Google Earth scale ("distance to your eye") that correspond to 300 m in a way each subgrid is entirely visible on a computer screen
#'
#' @param geojson.path path of GeoJSON file. Default 'E42N30-42.geojson'
#' @param rows number of cells in the X dimension. By default 140.
#' @param cols number of cells in the Y dimension. By default 60.
#' @param export if TRUE, export the GeoJSON file, if FALSE will only return it
#' @param fileOut name of the exported GeoJSON with extension (ex: "E42N30-42_subgrids.geojson"). Default `NA`, will add "_subgrids" to the input GeoJSON
#' @param dirOut folder where outputs will be saved. Default: '/results'. If it doesn't exist, will be created. Only useful is export.plot is TRUE.
#' @param verbose if TRUE (by default), print messages.
#'
#' @return A GeoJSON file with the same extent as the input GeoJSON file, but divided in many smaller polygons numbered from 1 to ...
#'
#' @examples
#'
#' # Create a grid of 10 rows by 20 columns
#' geojson_grid(geojson.path = paste0(system.file(package = "eamenaR"),
#'                                           "/extdata/E42N30-42.geojson"),
#'              rows = 10,
#'              cols = 20)
#' @export
geojson_grid <- function(geojson.path = paste0(system.file(package = "eamenaR"),
                                               "/extdata/E42N30-42.geojson"),
                         rows = 140,
                         cols = 60,
                         export = TRUE,
                         fileOut = NA,
                         dirOut = paste0(system.file(package = "eamenaR"),
                                         "/results/"),
                         verbose = TRUE) {
  if(is.na(fileOut)){
    fileOut <- paste0(DescTools::SplitPath(geojson.path)$filename, "_subgrids.geojson")
  }
  if(verbose){print(paste0("Read", geojson.path))}
  polygon <- sf::st_read(geojson.path)
  grid.id <- polygon$Grid.ID
  nb.subgrids <- cols*rows
  subgrids.ids <- paste0(grid.id, '.', seq(1, nb.subgrids))
  if(verbose){print(paste0("  * nb of subgrids:", nb.subgrids,
                           "(", head(subgrids.ids, 1), " ... ", tail(subgrids.ids, 1), ")"))}
  polygon_sf <- sf::st_as_sf(polygon)
  bbox <- sf::st_bbox(polygon_sf)
  cell_width <- (bbox[3] - bbox[1]) / cols
  cell_height <- (bbox[4] - bbox[2]) / rows
  #  of points
  sfc.polygons <- sf::st_make_grid(bbox,
                                   cellsize = c(cell_width, cell_height))
  if(verbose){print(paste0("Subgrids created"))}
  polygons <- sf::st_as_sf(sfc.polygons)
  polygons <- sf::st_cast(polygons, "POLYGON")
  polygons$id <- rev(subgrids.ids)
  # # Intersect the grid polygons with the original polygon
  # intersected_polygons <- sf::st_intersection(polygons, polygon_sf)
  if(export){
    outPath <- paste0(dirOut, fileOut)
    sf::st_write(polygons,
                 outPath,
                 delete_dsn = TRUE)
    if(verbose){print(paste0("File '", fileOut,"' has been exported to ", dirOut))}
  } else {
    return(polygons)
  }
}

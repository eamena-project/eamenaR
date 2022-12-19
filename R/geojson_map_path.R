#' Create a map with paths between different heritage places
#'
#' @name geojson_map_path
#'
#' @description Create a distribution map of heritage places linked together by paths, for example for caravanserails
#'
#' @param map.name the name of the output map and the name of the saved file (if export.plot is TRUE). By default "map_path".
#' @param geojson.path the path of the GeoJSON file. By default 'caravanserail.geojson'.
#' @param csv.path the path to the CSV where the edges between two heritage places are recorded. By default 'caravanserail_paths.csv'.
#' @param export.type the type of output: a map (`map`) or a profile (`profile`).
#' @param routes limit the study to some routes. By default NA, no limitation.
#' @param stamen.zoom the zoom of the Stamen basemap, between 0 (world, unprecise) to 21 (building, very precise). By default NA, the zoom level will be calculated automatically.
#' @param interactive if TRUE will plot a VisNetwork. By default FALSE.
#' @param export.plot if TRUE, export the plot, if FALSE will only display it.
#' @param dirOut the folder where the outputs will be saved. By default: '/results'. If it doesn't exist, it will be created. Only useful is export plot is TRUE.
#' @param verbose if TRUE (by default), print messages
#'
#' @return A PNG map of heritage places linked together by paths
#'
#' @examples
#'
#' # plot a general map of heritage places
#' geojson_map_path(map.name = "caravanserail_paths", export.plot = T, fig.width = 11)
#'
#' # create an interactive map of each route
#' geojson_map_path(geojson.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserailZ.geojson",
#'                  csv.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail_paths.csv",
#'                  routes = c(0, 1, 2, 3, 4),
#'                  interactive = T,
#'                  export.plot = T,
#'                  dirOut = "C:/Rprojects/eamenaR/results/")
#'
#' @export
geojson_map_path <- function(map.name = "paths",
                             geojson.path = paste0(system.file(package = "eamenaR"),
                                                   "/extdata/caravanserail.geojson"),
                             csv.path = paste0(system.file(package = "eamenaR"),
                                               "/extdata/caravanserail_paths.csv"),
                             export.type = c("map"),
                             routes = NA,
                             interactive = FALSE,
                             stamen.zoom = NA,
                             export.plot = F,
                             dirOut = paste0(system.file(package = "eamenaR"),
                                             "/results/"),
                             fig.width = 8,
                             fig.height = 8,
                             verbose = TRUE){
  if(verbose){print("* paths between HPs")}
  paths <- eamenaR::geojson_format_path(geojson.path, csv.path)
  paths.geom.sf <- sf::st_as_sf(paths, wkt = "path.wkt")
  sf::st_crs(paths.geom.sf) <- 4326
  hp.geom.sf <- geojsonsf::geojson_sf(geojson.path)
  sf::st_crs(hp.geom.sf) <- 4326
  if("profile" %in% export.type & interactive == FALSE){
    if(verbose){print(" - creates a static 'profile' of the paths")}
    # TO COMPLETE ...
  }
  if("map" %in% export.type & interactive == TRUE){
    if(verbose){print(" - creates an interactive 'map' of the paths")}
    for(route in routes){
      if(verbose){print(paste0("   selected route is '", route, "'"))}
      paths.route1 <- paths[paths$route == route, ]
      paths.route1.df.from <- paths.route1[ , c("from", "from.id")]
      paths.route1.df.to <- paths.route1[ , c("to", "to.id")]
      names(paths.route1.df.from) <- names(paths.route1.df.to) <- c("title", "label")
      paths.route1.df <- rbind(paths.route1.df.from, paths.route1.df.to)
      nodes <- paths.route1.df[!duplicated(paths.route1.df), ]
      nodes$id <- nodes$title
      edges <- paths.route1[]
      colnames(edges)[which(names(edges) == "dist.m")] <- "value"
      edges$title <- paste(round(edges$value/1000, 0), "km") # to km
      colnames(nodes)[which(names(nodes) == "dist.m")] <- "value"
      visRoute <- visNetwork::visNetwork(nodes, edges,
                                         main = paste0("route ", route),
                                         width = "90%", height = "90vh") %>%
        visNetwork::visEdges(arrows = "to") %>%
        visNetwork::visOptions(highlightNearest = T)
      if(!export.plot){print(visRoute)}
      if(export.plot){
        if(verbose){print(paste0("  - export an interactive network"))}
        fileOut <- paste0(map.name, "_map_route_", route, ".html")
        visNetwork::visSave(visRoute,
                            paste0(dirOut, fileOut),
                            # selfcontained = TRUE,
                            background = "white")
        if(verbose){print(paste0("* the network '", fileOut, "' has been exported to: ", dirOut))}
      }
    }
  }
  if("map" %in% export.type & interactive == FALSE){
    if(verbose){print(" - creates a static 'map' of the paths")}
    left <- as.numeric(sf::st_bbox(hp.geom.sf)$xmin)
    bottom <- as.numeric(sf::st_bbox(hp.geom.sf)$ymin)
    right <- as.numeric(sf::st_bbox(hp.geom.sf)$xmax)
    top <- as.numeric(sf::st_bbox(hp.geom.sf)$ymax)
    buffer <- mean(c(abs(left - right), abs(top - bottom)))/10
    bbox <- c(left = left - buffer,
              bottom = bottom - buffer,
              right = right + buffer,
              top = top + buffer
    )
    if(is.na(stamen.zoom)){
      stamen.zoom <- rosm:::tile.raster.autozoom(
        rosm::extract_bbox(
          matrix(bbox, ncol = 2, byrow = TRUE)),
        epsg = 4326)
    }
    if(verbose){print(paste0("    - retrieve Stamen basemap background (zoom = ", stamen.zoom, ")"))}
    stamenbck <- ggmap::get_stamenmap(bbox,
                                      zoom = stamen.zoom,
                                      maptype = "terrain-background")
    hp.geojson.point <- hp.geom.sf[sf::st_geometry_type(hp.geom.sf$geometry) == "POINT", ]
    hp.geojson.line <- hp.geom.sf[sf::st_geometry_type(hp.geom.sf$geometry) == "LINESTRING", ]
    hp.geojson.polygon <- hp.geom.sf[sf::st_geometry_type(hp.geom.sf$geometry) == "POLYGON", ]

    mapOut <- ggmap::ggmap(stamenbck) +
      ggplot2::geom_sf(data = paths.geom.sf,
                       ggplot2::aes(colour = route),
                       inherit.aes = FALSE) +
      ggplot2::geom_sf(data = hp.geojson.point,
                       colour = "black",
                       inherit.aes = FALSE) +
      ggplot2::geom_sf(data = hp.geojson.line,
                       colour = "black",
                       inherit.aes = FALSE) +
      ggplot2::geom_sf(data = hp.geojson.polygon,
                       colour = "black",
                       inherit.aes = FALSE) +
      ggrepel::geom_text_repel(data = hp.geojson.point,
                               ggplot2::aes(x = sf::st_coordinates(hp.geojson.point)[, "X"],
                                            y = sf::st_coordinates(hp.geojson.point)[, "Y"],
                                            label = rownames(hp.geojson.point)),
                               size = 2,
                               segment.color = "black",
                               segment.size = .1,
                               segment.alpha = .5,
                               min.segment.length = .3,
                               force = .5,
                               max.time = 1.5,
                               max.overlaps = Inf,
                               inherit.aes = FALSE) +
      ggrepel::geom_text_repel(data = hp.geojson.polygon,
                               ggplot2::aes(x = sf::st_coordinates(sf::st_centroid(hp.geojson.polygon))[, "X"],
                                            y = sf::st_coordinates(sf::st_centroid(hp.geojson.polygon))[, "Y"],
                                            label = rownames(hp.geojson.polygon)),
                               size = 2,
                               segment.color = "black",
                               segment.size = .1,
                               segment.alpha = .5,
                               min.segment.length = .3,
                               force = .5,
                               max.time = 1.5,
                               max.overlaps = Inf,
                               inherit.aes = FALSE) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 15,
                                                        hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(size = 12,
                                                           hjust = 0.5))
    if (export.plot) {
      if(verbose){print(paste0("    - export 'map'"))}
      dir.create(dirOut, showWarnings = FALSE)
      gout <- paste0(dirOut, map.name, ".png")
      ggplot2::ggsave(filename = gout,
                      plot = mapOut,
                      height = fig.height,
                      width = fig.width)
      if(verbose){print(paste0("    - 'map' is exported"))}
    } else {
      mapOut
    }
  }
  if("profile" %in% export.type){

  }
}

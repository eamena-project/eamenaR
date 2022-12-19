#' Create a map and a profile with paths between different heritage places
#'
#' @name geojson_map_path
#'
#' @description Create a distribution map and an elevation profile of heritage places linked together by paths, for example for caravanserails
#'
#' @param map.name the name of the output map and the name of the saved file (if export.plot is TRUE). By default "map_path".
#' @param geojson.path the path of the GeoJSON file. By default 'caravanserail.geojson'.
#' @param csv.path the path to the CSV where the edges between two heritage places are recorded. By default 'caravanserail_paths.csv'.
#' @param export.type the type of output: a map (`map`) or a profile (`profile`). For this latter the Z should be calculated with the `geojson_addZ()` function.
#' @param routes limit the study to some routes. By default NA, no limitation.
#' @param stamen.zoom the zoom of the Stamen basemap, between 0 (world, unprecise) to 21 (building, very precise). By default NA, the zoom level will be calculated automatically.
#' @param interactive if TRUE will plot a VisNetwork. By default FALSE.
#' @param export.plot if TRUE, export the plot, if FALSE will only display it.
#' @param dirOut the folder where the outputs will be saved. By default: '/results'. If it doesn't exist, it will be created. Only useful is export plot is TRUE.
#' @param verbose if TRUE (by default), print messages
#'
#' @return A PNG map and an HMTL map of heritage places linked together by paths
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
#' # create the profile of each route
#' df <- geojson_addZ(geojson.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail.geojson",
#'                    dirOut = "C:/Rprojects/eamenaR/inst/extdata/")
#' geojson_map_path(geojson.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserailZ.geojson",
#'                  csv.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail_paths.csv",
#'                  routes = c(0, 1, 2, 3, 4),
#'                  export.type = "profile",
#'                  export.plot = T,
#'                  fig.height = 11,
#'                  fig.width = 18,
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
      gout <- paste0(map.name, "_map", ".png")
      ggplot2::ggsave(filename = paste0(dirOut, gout),
                      plot = mapOut,
                      height = fig.height,
                      width = fig.width)
      if(verbose){print(paste0("    - the routes' map '", gout,"' is exported into: '", dirOut,"'"))}
    } else {
      mapOut
    }
  }
  if("profile" %in% export.type){
    # to store values
    df.dist.Zs <- data.frame(dist = numeric(),
                             hp = character(),
                             Zs = numeric(),
                             route = character())
    # routes <- unique(paths.geom.sf$route)
    for(route in routes){
      # paths by routes
      if(verbose){print(paste0("\n", "* read route: '", route, "'"))}
      # route <- 1
      # subset
      paths.route <- paths.geom.sf[paths.geom.sf$route == route, ]
      # paths.route <- paths.geom.sf
      paths.route.df <- data.frame(from = paths.route$from,
                                   to = paths.route$to,
                                   dist.m = paths.route$dist.m)

      # "EAMENA-0207260" %in% paths.route.df$to
      # "EAMENA-0207260" %in% df1

      # unique values
      paths.route.df.hp.names <- unique(as.vector(as.matrix(paths.route.df[ , c("from", "to")])))

      # paths.route.df.hp.names <- unique(paths.route.df[ , c("from", "to")])
      #
      # paths.route.df.hp.names <- unique(unique(paths.route.df$from),
      #                                   unique(paths.route.df$to))
      # hps
      hps.route <- hp.geom.sf[hp.geom.sf[["EAMENA ID"]] %in% paths.route.df.hp.names, ]

      # "EAMENA-0207260" %in% paths.route.df.hp.names

      hps.route$name <- hps.route[["EAMENA ID"]]

      # "EAMENA-0207260" %in%  hps.route$name

      # check
      if(verbose){print("  - check if all HPs are in the paths, and vice versa")}
      if(length(hps.route$name) != length(paths.route.df.hp.names)){
        if(verbose){print("    - missing data somewhere (HPs or paths)")}
        # possible duplicates
        # unique(paths.route.df.hp.names[! paths.route.df.hp.names %in% hps.route$name])
        # unique(hps.route$name[! hps.route$name %in% paths.route.df.hp.names])
        setdiff(hps.route$name, paths.route.df.hp.names)
        setdiff(paths.route.df.hp.names, hps.route$name)
        n_occur <- data.frame(table(hps.route$name))
        n_occur[n_occur$Freq > 1,]
        n_occur <- data.frame(table(paths.route.df.hp.names))
        n_occur[n_occur$Freq > 1,]
      } else {
        if(verbose){print("    - check done")}
      }

      hps.route.df <- as.data.frame(hps.route)
      # reorder columns
      idx.name <- ncol(hps.route.df)
      hps.route.df <- hps.route.df[ , c(idx.name, 2:idx.name - 1)]

      # hps.route.df[hps.route.df$name == start.node, "Z"]

      #colnames(hps.route.df)

      # g <- igraph::graph_from_data_frame(paths.route.df,
      #                                    directed = TRUE,
      #                                    vertices = hps.route.df)
      # unique(paths.route.df$from, paths.route.df$to)
      g <- igraph::graph_from_data_frame(paths.route.df,
                                         directed = TRUE)
      if(verbose){print("  - set the weight of the edges")}
      E(g)$weight <- E(g)$dist.m
      # get the start node
      if(1 == 1){
        if(verbose){print("  - find one starting node in the path")}
        node.deg.1 <- degree(g, mode = 'in') == 0
        df.nodes <- as.data.frame(node.deg.1)
        df.nodes$EAMENAID <- rownames(df.nodes)
        start.node <- df.nodes[df.nodes$node.deg.1 == TRUE, "EAMENAID"]
        start.node <- start.node[1] # only one
        if(verbose){print(paste0("    - the starting node of route '", route, "' is: '", start.node, "'"))}
      }
      # start.node <- "EAMENA-0207504"
      start.node.z <- hps.route.df[hps.route.df$name == start.node, "Z"]
      not.start.node <- V(g)$name[!V(g)$name %in% start.node]
      if(verbose){print(paste0("  - calculate distances from the starting node '", start.node,
                               "' to any other HPs on the route '", route, "'"))}
      df.dist <- distances(
        graph = g,
        v = start.node,
        # v = rep(start.node, length(not.start.node)),
        to = not.start.node
      )
      rownames(df.dist) <- "dist"
      # add HP names
      df.dist <- rbind(df.dist, colnames(df.dist))
      # add the route
      df.dist <- rbind(df.dist, rep(route, ncol(df.dist)))
      # add the Z
      Zs <- c()
      for(c in colnames(df.dist)){
        # c <- "EAMENA-0207260"
        Z <- hps.route.df[hps.route.df$name == c, "Z"]
        if(verbose){print(paste0("    - HP '", c, " altitude: ", Z, " m"))}
        Zs <- c(Zs, Z)
      }
      # df.Zs <- t(data.frame(hp = colnames(df.dist),
      #                       Z = Zs))

      # c <- "EAMENA-0207505"))
      df.dist.Z <- rbind(df.dist, Zs)
      df.dist.Z <- as.data.frame(t(df.dist.Z))
      colnames(df.dist.Z)[which(names(df.dist.Z) == "V2")] <- "hp"
      colnames(df.dist.Z)[which(names(df.dist.Z) == "V3")] <- "route"
      # add the HP origin
      df.dist.Z <- rbind(df.dist.Z, data.frame(dist = 0,
                                               hp = start.node,
                                               Zs = start.node.z,
                                               route = route,
                                               row.names = start.node))
      rownames(df.dist.Z) <- paste0(rownames(df.dist.Z), ".", route)
      # aggregate the route to a bigger df
      df.dist.Zs <- rbind(df.dist.Zs, df.dist.Z)
    }
    # type conversion
    df.dist.Zs$Zs <- as.numeric(as.character(df.dist.Zs$Zs))
    df.dist.Zs$dist <- as.numeric(as.character(df.dist.Zs$dist))
    # df.dist.Z[ , c("Zs", "dist")] <-
    df.dist.Zs$dist <- df.dist.Zs$dist/1000
    # recovers ids (short names for plotting)
    df.ids <- eamenaR::geojson_stat(stat = c("list_ids"),
                                    geojson.path = geojson.path,
                                    export.stat = T)
    df.ids <- data.frame(id = rownames(df.ids),
                         hp = df.ids$ea.ids)
    df.complete <- merge(df.dist.Zs, df.ids, by = "hp", all.x = T)
    # create profile
    gout <- ggplot2::ggplot(df.complete, aes(x = dist, y = Zs, label = id)) +
      ggplot2::facet_grid(route ~ .) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggrepel::geom_text_repel(max.overlaps = Inf) +
      ggplot2::xlab("distance (km)") +
      ggplot2::ylab("elevation (m)") +
      ggplot2::theme_bw()
    if (export.plot) {
      if(verbose){print(paste0("    - export 'profile'"))}
      dir.create(dirOut, showWarnings = FALSE)
      profOut <- paste0(map.name, "_profile", ".png")
      ggplot2::ggsave(filename = paste0(dirOut, profOut),
                      plot = gout,
                      height = fig.height,
                      width = fig.width)
      if(verbose){print(paste0("    - the routes' profile '", profOut,"' is exported into: '", dirOut,"'"))}
    } else {
      gout
    }
  }
}

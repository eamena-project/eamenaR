#' Create map and profile with paths between different heritage places (HP)
#'
#' @name geojson_map_path
#'
#' @description Create distribution map and elevation profile of HP linked together by paths, for example, caravanserails.
#'
#' @param d A hash dictionary.
#' @param map.name name of output map and name of saved file (if export.plot is TRUE). Default "map_path".
#' @param geojson.path path of GeoJSON file. Default 'caravanserail.geojson'.
#' @param csv.path path to CSV where edges between two HPs are recorded. Default 'caravanserail_paths.csv'.
#' @param stats type of output: map (`map`) or profile (`profile`). For latter Z should be calculated with the `geojson_addZ()` function.
#' @param name of category column. Default "route" for caravanserais.
#' @param show.ids Show HP labels. Default: TRUE.
#' @param selected.category limit study to some categories. For example to some particular routes for caravanserais. Default NA, no limitation.
#' @param symbology.path path to XLSX recording symbology for different values, default 'symbology.xlsx'.
#' @param stamen.zoom zoom of Stamen basemap, between 0 (world, unprecise) to 21 (building, very precise). By default NA, zoom level will be calculated automatically.
#' @param interactive if TRUE will plot VisNetwork. Default FALSE.
#' @param color.set RBrewer color set. Default "Set1".
#' @param verbose if TRUE (by default), print messages.
#'
#' @return An hash object with maps of HPs linked together by paths, profiles, etc.
#'
#' @examples
#'
#' library(dplyr)
#'
#' # plot general map of HPs
#' d <- hash::hash()
#' d <- geojson_map_path(map.name = "caravanserail_paths")
#' d$caravanserail_paths_map
#'
#'
#' # create an interactive map of each route
#' geojson_map_path(geojson.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserailZ.geojson",
#'                  csv.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail_paths.csv",
#'                  selected.category = c(0, 1, 2, 3, 4),
#'                  interactive = T)
#'
#' # create interactive map of each route and export
#' geojson_map_path(geojson.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserailZ.geojson",
#'                  csv.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail_paths.csv",
#'                  selected.category = c(0, 1, 2, 3, 4),
#'                  interactive = T,
#'                  export.plot = T,
#'                  dirOut = "C:/Rprojects/eamenaR/results/")
#'
#' # create profile of each route
#' df <- geojson_addZ(geojson.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail.geojson",
#'                    dirOut = "C:/Rprojects/eamenaR/inst/extdata/")
#' geojson_map_path(geojson.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserailZ.geojson",
#'                  csv.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail_paths.csv",
#'                  selected.category = c(0, 1, 2, 3, 4),
#'                  stats = "profile")
#'
#' # create profile of each route and export
#' geojson_map_path(geojson.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserailZ.geojson",
#'                  csv.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail_paths.csv",
#'                  selected.category = c(0, 1, 2, 3, 4),
#'                  stats = "profile",
#'                  export.plot = T,
#'                  fig.height = 11,
#'                  fig.width = 18,
#'                  dirOut = "C:/Rprojects/eamenaR/results/")
#'
#' @export
geojson_map_path <- function(d = NA,
                             map.name = "paths",
                             geojson.path = paste0(system.file(package = "eamenaR"),
                                                   "/extdata/caravanserail.geojson"),
                             csv.path = paste0(system.file(package = "eamenaR"),
                                               "/extdata/caravanserail_paths.csv"),
                             stats = c("map"),
                             by = "route",
                             show.ids = TRUE,
                             selected.category = NA,
                             symbology.path = paste0(system.file(package = "eamenaR"),
                                                     "/extdata/symbology.xlsx"),
                             interactive = FALSE,
                             stamen.zoom = NA,
                             color.set = "Set1",
                             verbose = TRUE){
  symbology <- openxlsx::read.xlsx(symbology.path)
  r.id <- eamenaR::ref_ids("hp.id")
  if(verbose){print("* paths between HPs")}
  paths <- geojson_format_path(geojson.path,
                               csv.path,
                               by = by,
                               verbose = verbose)
  nb.all.path <- nrow(paths)
  # remove where NA
  paths <- paths[!is.na(paths$path.wkt), ]
  nb.clear.path <- nrow(paths)
  if(verbose){
    print(paste0(" there are ", nb.all.path - nb.clear.path,
                 " missing matches between HP and paths (",
                 nb.all.path - nb.clear.path, " will be missing)"))
  }
  paths.geom.sf <- sf::st_as_sf(paths, wkt = "path.wkt")
  sf::st_crs(paths.geom.sf) <- 4326
  d[["paths"]] <- paths.geom.sf
  if(inherits(geojson.path, "sf")){
    if(verbose){
      print(paste0("Reads a 'sf' dataframe"))
    }
    hp.geom.sf <- geojson.path
  }
  if(is.character(geojson.path)){
    if(verbose){
      print(paste0("Reads a GeoJSON file path"))
    }
    # hp.geom.sf <- geojsonsf::geojson_sf(geojson.path)
    hp.geom.sf <- sf::st_read(geojson.path)
    sf::st_crs(hp.geom.sf) <- 4326
  }
  # ids?
  if(verbose){
    print(paste0("Run geojson_stat for 'list_ids'"))
  }
  df.ids <- geojson_stat(stat = c("list_ids"),
                         geojson.path = geojson.path,
                         export.stat = T)
  df.ids <- data.frame(id = rownames(df.ids),
                       hp.1 = df.ids[["hp.id"]])
  # categories colors
  by.ids <- unique(paths.geom.sf[[by]])
  by.colors <- RColorBrewer::brewer.pal(length(by.ids), color.set)
  df.colors <- data.frame(route = by.ids,
                          color = by.colors
  )
  paths.geom.sf <- merge(paths.geom.sf, df.colors, by = by)
  by.unique <- unique(paths.geom.sf[[by]])
  # TODO: COMPLETE ...
  # }
  # if("profile" %in% stats & interactive == FALSE){
  #   if(verbose){print(" - creates a static 'profile' of the paths")}
  #
  if("map" %in% stats & interactive){
    if(verbose){print(" - creates an interactive 'map' of the paths")}
    for(a.by in by.unique){
      if(verbose){print(paste0("   selected", by, " is '", a.by, "'"))}
      paths.route1 <- paths[paths[[by]] == a.by, ]
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
      gout <- visNetwork::visNetwork(nodes, edges,
                                     main = paste0(by, " ", a.by),
                                     width = "90%", height = "90vh") %>%
        visNetwork::visEdges(arrows = "to") %>%
        visNetwork::visOptions(highlightNearest = T)
    }
    outName <- paste0(map.name, "_map_interact")
    d[[outName]] <- gout
  }
  if("map" %in% stats & !interactive){
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
    # if(is.na(stamen.zoom)){
    #   if(verbose){print(" - calculate zoom")}
    #   stamen.zoom <- rosm:::tile.raster.autozoom(
    #     rosm::extract_bbox(
    #       matrix(bbox, ncol = 2, byrow = TRUE)),
    #     epsg = 4326)
    # }
    if(verbose){print(paste0("    - retrieve Stadia basemap background (zoom = ", stamen.zoom, ")"))}
    stadia_map_token = "aa5c9739-90c7-410b-9e9b-6c904df6e4dd"
    ggmap::register_stadiamaps(stadia_map_token)
    stamenbck <- ggmap::get_stadiamap(bbox = c(left = left - buffer, # as.numeric(sf::st_bbox(GSnew)$xmin),
                                               bottom = bottom - buffer , # as.numeric(sf::st_bbox(GSnew)$ymin),
                                               right = right + buffer, # as.numeric(sf::st_bbox(GSnew)$xmax),
                                               top = top + buffer), #  as.numeric(sf::st_bbox(GSnew)$ymax)),
                                      maptype = "stamen_terrain_background",
                                      crop = FALSE,
                                      zoom = 7)
    # stamenbck <- ggmap::get_stamenmap(bbox,
    #                                   zoom = stamen.zoom,
    #                                   maptype = "terrain-background")
    if(verbose){print(paste0("      ... retrieved"))}
    # terr = sf::st_read("C:/Rprojects/eamenaR/inst/extdata/caravanserail.geojson")
    hp.geojson.point <- hp.geom.sf[sf::st_geometry_type(hp.geom.sf$geometry) == "POINT", ]
    hp.geojson.point <- sf::st_zm(hp.geojson.point)
    hp.geojson.line <- hp.geom.sf[sf::st_geometry_type(hp.geom.sf$geometry) == "LINESTRING", ]
    hp.geojson.polygon <- hp.geom.sf[sf::st_geometry_type(hp.geom.sf$geometry) == "POLYGON", ]
    xxxv <- setNames(by.colors, by.ids)
    gout <- ggmap::ggmap(stamenbck) +
      ggplot2::geom_sf(data = paths.geom.sf,
                       # ggplot2::aes(colour = color),
                       ggplot2::aes(colour = factor(route)),
                       size = 1,
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
      # ggrepel::geom_text_repel(data = hp.geojson.point,
      #                          ggplot2::aes(x = sf::st_coordinates(hp.geojson.point)[, "X"],
      #                                       y = sf::st_coordinates(hp.geojson.point)[, "Y"],
      #                                       label = rownames(hp.geojson.point)),
      #                          size = 2,
      #                          segment.color = "black",
      #                          segment.size = .1,
      #                          segment.alpha = .5,
      #                          min.segment.length = .3,
      #                          force = .5,
      #                          max.time = 1.5,
      #                          max.overlaps = Inf,
      #                          inherit.aes = FALSE) +
      ggplot2::labs(title = map.name) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 15,
                                                        hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(size = 12,
                                                           hjust = 0.5)) +
      ggplot2::labs(color = by) +
      # ggplot2::scale_discrete_manual(values = setNames(by.colors, by.ids)) +
      ggplot2::scale_colour_manual(values = by.colors)
    if(show.ids){
      if(verbose){
        print("Show HP ids")
      }
      gout <- gout +
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
                                 inherit.aes = FALSE)
    }
    outName <- paste0(map.name, "_map")
    d[[outName]] <- gout
    # ggplot2::scale_fill_discrete(labels = by.ids)
  }
  if("profile" %in% stats){
    # to store values
    df.dist.Zs <- data.frame(hp.1 = character(),
                             dist = numeric(),
                             hp.2 = character(),
                             Zs = numeric(),
                             route = character())
    if(is.na(selected.category)){
      selected.category <- sort(unique(paths.geom.sf[[by]]))
    }
    for(route in selected.category){
      # route <- 1
      # paths by
      if(verbose){print(paste0("\n",
                               "* read '", by, "' = ", route))}
      # subset
      paths.route <- paths.geom.sf[paths.geom.sf[[by]] == route, ]
      paths.route.df <- data.frame(from = paths.route$from,
                                   to = paths.route$to,
                                   dist.m = paths.route$dist.m)
      # unique values
      paths.route.df.hp.names <- unique(as.vector(as.matrix(paths.route.df[ , c("from", "to")])))
      hps.route <- hp.geom.sf[hp.geom.sf[[r.id]] %in% paths.route.df.hp.names, ]
      hps.route$name <- hps.route[[r.id]]
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
        n_occur[n_occur$Freq > 1, ]
        n_occur <- data.frame(table(paths.route.df.hp.names))
        n_occur[n_occur$Freq > 1, ]
      } else {
        if(verbose){print("    - check done")}
      }
      hps.route.df <- as.data.frame(hps.route)
      # reorder columns
      idx.name <- ncol(hps.route.df)
      hps.route.df <- hps.route.df[ , c(idx.name, 2:idx.name - 1)]
      # use igraph to find the first HP and to cumulated distances
      g <- igraph::graph_from_data_frame(paths.route.df,
                                         directed = TRUE)
      if(verbose){print("  - set the weight of the edges")}
      igraph::E(g)$weight <- igraph::E(g)$dist.m
      # get the start node
      if(1 == 1){
        if(verbose){print("  - find one starting node in the path")}
        node.deg.1 <- igraph::degree(g, mode = 'in') == 0
        df.nodes <- as.data.frame(node.deg.1)
        # print("||||")
        # print(colnames(df.nodes))
        df.nodes$id <- rownames(df.nodes)
        start.node <- df.nodes[df.nodes$node.deg.1 == TRUE, "id"]
        start.node <- start.node[1] # only one
        if(verbose){print(paste0("    - the starting node of route '", route, "' is: '", start.node, "'"))}
      }
      # start.node <- "EAMENA-0207504"
      start.node.z <- hps.route.df[hps.route.df$name == start.node, "Z"]
      not.start.node <- igraph::V(g)$name[!igraph::V(g)$name %in% start.node]
      if(verbose){print(paste0("  - calculate distances from the starting node '", start.node,
                               "' to any other HPs on the route '", route, "'"))}
      df.dist <- igraph::distances(
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
      df.dist.Z <- rbind(df.dist, Zs)
      df.dist.Z <- as.data.frame(t(df.dist.Z))
      df.dist.Z$hp.1 <- rownames(df.dist.Z)
      colnames(df.dist.Z)[which(names(df.dist.Z) == "V2")] <- "hp.2"
      colnames(df.dist.Z)[which(names(df.dist.Z) == "V3")] <- "route"
      df.dist.Z <- df.dist.Z[ , c("hp.1", "dist", "hp.2", "route", "Zs")]
      # add the HP origin
      df.dist.Z <- rbind(df.dist.Z, data.frame(hp.1 = start.node,
                                               dist = 0,
                                               hp.2 = start.node,
                                               route = route,
                                               Zs = start.node.z,
                                               row.names = start.node))
      # to avoid duplicated in rownames ?
      # rownames(df.dist.Z) <- paste0(rownames(df.dist.Z), ".", route)
      # aggregate the route to a bigger df
      df.dist.Zs <- rbind(df.dist.Zs, df.dist.Z)
    }
    # rownames(df.dist.Zs) <- seq(1, )

    # # recovers ids (short names for plotting)
    # df.ids <- eamenaR::geojson_stat(stat = c("list_ids"),
    #                                 geojson.path = geojson.path,
    #                                 export.stat = T)
    # df.ids <- data.frame(id = rownames(df.ids),
    #                      hp.1 = df.ids[["hp.id"]])
    df.complete <- merge(df.dist.Zs, df.ids, by = "hp.1", all.x = T)
    df.complete <- df.complete[df.complete$dist != Inf, ]
    # type conversion
    df.complete$Zs <- as.numeric(as.character(df.complete$Zs))
    df.complete$dist <- as.numeric(as.character(df.complete$dist))
    # df.dist.Z[ , c("Zs", "dist")] <-
    df.complete$dist <- df.complete$dist/1000
    # replace the category column with the generic "by" for the plot
    names(df.complete)[names(df.complete) == by] <- 'by'
    # create profile
    gout <- ggplot2::ggplot(df.complete, ggplot2::aes(x = dist, y = Zs, label = id)) +
      ggplot2::facet_grid(by ~ .) +
      ggplot2::geom_line(ggplot2::aes(colour = factor(by)),
                         size = 1) +
      ggplot2::geom_point() +
      ggrepel::geom_text_repel(max.overlaps = Inf) +
      ggplot2::xlab("distance (km)") +
      ggplot2::ylab("elevation (m)") +
      ggplot2::labs(color = factor(by)) +
      ggplot2::scale_colour_manual(values = by.colors) +
      ggplot2::theme_bw()
    outName <- paste0(map.name, "_profile")
    # print(outName)
    # print(gout)
    d[[outName]] <- gout
    if(verbose){print(paste0("profiles.. done!"))}
  }
  d[["ids"]] <- df.ids
  return(d)
}

# geojson_map_path(geojson.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserailZ.geojson",
#                  csv.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail_paths.csv",
#                  # selected.category = c(0, 1, 2, 3, 4),
#                  stats = "profile",
#                  export.plot = T)

# geojson_map_path(map.name = "caravanserail_paths", export.plot = T, fig.width = 15)


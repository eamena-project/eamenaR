#' Add a Z value to geometries
#'
#' @name geojson_addZ
#'
#' @description
#'
#' @param map.name the name of the output map and the name of the saved file (if export.plot is TRUE). By default "map".
#' @param geojson.path the path of the GeoJSON file. By default 'caravanserail.geojson'.
#' @param ids the IDs of the resources, by default "EAMENA.ID" (R fieldname format, without spaces).
#' @param field.names a vector one or many field names for thematic cartography. If NA (by default), will create a general map
#' @param highlights.ids EAMENA IDs (ex: 'EAMENA-0205783') that will be highlighted in the map. If NA (by default), no highlights.
#' @param symbology the path to the XLSX recording the symbology for the different values, by default 'symbology.xlsx'.
#' @param stamen.zoom the zoom of the Stamen basemap, between 0 (world, unprecise) to 21 (building, very precise). By default NA, the zoom level will be calculated automatically.
#' @param plotly.plot if FALSE create a static PNG, if TRUE create a plotly plot as a HTML widget.
#' @param export.plot if TRUE, export the plot, if FALSE will only display it.
#' @param dirOut the folder where the outputs will be saved. By default: '/results'. If it doesn't exist, it will be created. Only useful is export plot is TRUE.
#'
#' @return
#'
#' @examples
#'
#'
#' @export
geojson_addZ <- function(geojson.path = paste0(system.file(package = "eamenaR"),
                                              "/extdata/caravanserail.geojson"),
                        # ids = "EAMENA ID",
                        # field.names = NA,
                        # highlights.ids = NA,
                        # symbology = paste0(system.file(package = "eamenaR"),
                        #                    "/extdata/symbology.xlsx"),
                        # stamen.zoom = NA,
                        # plotly.plot = F,
                        # export.plot = F,
                        geojson.path.out = NA,
                        dirOut = paste0(system.file(package = "eamenaR"),
                                        "/results/"),
                        verbose = T){
  if(verbose){print(paste0("* get Z from `https://www.gmrt.org/` service"))}
  ea.geojson <- geojsonsf::geojson_sf(geojson.path)
  n.row <- nrow(my.geom)
  if(verbose){print(paste0("    read ", n.row, " geometries"))}
  my.geom <- ea.geojson
  # xxx <- wellknown::sf_convert(ea.geojson[[2]][[2]])
  df.profile <- data.frame(x = double(),
                           y = double(),
                           z = double())
  for (i in 1:n.row){
    # i <- 2
    # hp.is <- my.geom$hp[i]
    # bc.is <- my.geom$bc[i]
    if(verbose){
      if(i == 1 | i%%5 == 0){
        print(paste0("     - read ", i, "/", n.row))
      }
    }
    a.geom <- my.geom[i, ]
    if(sf::st_geometry_type(a.geom$geometry) %in% c("MULTIPOINT", "POLYGON", "MULTIPOLYGON")){
      # centroids
      coordinates <- jsonlite::toJSON(sf::st_coordinates(sf::st_centroid(a.geom)))
    }
    if(sf::st_geometry_type(a.geom$geometry) == "POINT"){
      coordinates <- jsonlite::toJSON(sf::st_coordinates(a.geom))
    }
    http.req <- paste0("https://www.gmrt.org/services/ProfileServer?boundspath=",
                       coordinates,
                       "&format=json")
    r <- httr::GET(http.req)
    rr <- httr::content(r)
    rrr <- xml2::as_list(rr)
    rrrr <- rrr$html$body$p[[1]]
    rrrrr <- unlist(stringr::str_split(string = rrrr, pattern = "\\],\\["))
    rrrrrr <- gsub("\\]", "", rrrrr)
    rrrrrrr <- gsub("\\[", "", rrrrrr)
    for(j in 1:length(rrrrrrr)){
      # i <- 1
      val <- unlist(stringr::str_split(rrrrrrr[j], pattern = ","))
      # val <- c(hp.is, bc.is, val)
      # val <- c(val[1:3],  val[2], val[3])
      df.profile[nrow(df.profile)+1, ] <- val[1:3]
    }

    # df.profile <- df.profile %>%
    #   mutate_all(~as.numeric(.))
    #
    # # limits
    # dmax <- round_any(max(df.profile$d), 10, f = ceiling)
    # dmin <- 0
    # zmax <- round_any(max(df.profile$z), 10, f = ceiling)
    # zmin <- floor(min(df.profile$z))
    #
    # # hp location (here the head of the desert kite)
    # head.coord <- df.profile %>%
    #   group_by(hp, bc) %>%
    #   filter(d == min(d)) %>%
    #   arrange(hp, bc, d) %>%
    #   unite(label, hp, bc, sep = "-", remove = FALSE)
    #
    # bc.jpg <- ggplot(df.profile, aes(x = d, y = z, color = hp, group = 1)) +
    #   facet_grid(hp ~ bc, scales = "free") +
    #   geom_line(size = 1) +
    #   geom_point(data = head.coord, aes(x = d, y = z)) +
    #   geom_text(data = head.coord, aes(x = d, y = z, label = label), vjust = -.5) +
    #   xlab("distance from the head (m)") +
    #   ylab("altitude (m)") +
    #   scale_x_continuous(limits = c(dmin, dmax), position = "top") +
    #   scale_y_continuous(limits = c(zmin, zmax), position = "right") +
    #   theme_bw() +
    #   theme(legend.position = "none")
    # out.bc.jpg <- "bc_profiles.jpg"
    # ggsave(out.bc.jpg, bc.jpg) # save local
    # drive_upload(out.bc.jpg, path = results.path, overwrite = T)
  }
  return(df.profile)
  if(verbose){print(paste0("* done"))}
}



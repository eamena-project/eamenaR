#' Create map, whether static or interactive leaflet, from GeoJSON file
#'
#' @name geojson_map
#'
#' @description Create distribution map
#'
#' @param map.name name of output map and name of saved file (if export.plot is TRUE). Default "map".
#' @param geojson.path path of GeoJSON file. Default 'caravanserail.geojson'.
#' @param ids IDs of resources, default "EAMENA.ID" (R fieldname format, without spaces).
#' @param field.names a vector of one or many field names for thematic cartography. If NA (default), will create a general map
#' @param highlights.ids EAMENA IDs (ex: 'EAMENA-0205783') that will be highlighted in map. If NA (default), no highlights.
#' @param symbology path to XLSX recording symbology for different values, default 'symbology.xlsx'.
#' @param stamen.zoom zoom of Stamen basemap, between 0 (world, unprecise) to 21 (building, very precise). Default NA, zoom level will be calculated automatically.
#' @param interactive if FALSE create static PNG (by  default), if TRUE create a plotly plot as HTML widget.
#' @param export.plot if TRUE, export plot, if FALSE will only display it.
#' @param dirOut folder where outputs will be saved. Default: '/results'. If it doesn't exist, will be created. Only useful if export plot is TRUE.
#' @param fig.width,fig.height size of output map.
#'
#' @return An interactive map (leaflet) or not
#'
#' @examples
#'
#'
#' # plot a general map of heritage places
#' geojson_map(map.name = "caravanserail")
#'
#' # save a thematic map
#' geojson_map(map.name = "caravanserail",
#'            field.names = c("Damage Extent Type"),
#'            export.plot = T)
#'
#' # save different thematic maps
#' geojson_map(map.name = "caravanserail",
#'            field.names = c("Disturbance Cause Type ", "Damage Extent Type"),
#'            export.plot = T)
#'
#' # save an interactive map
#' geojson_map(map.name = "caravanserail_plotly",
#'             interactive = T,
#'             export.plot = T)
#'
#' # plot a general map of geoarchaeological data
#' geojson_map(map.name = "geoarch",
#'             ids = "GEOARCH.ID",
#'             stamen.zoom = 6,
#'             geojson.path = "C:/Rprojects/eamena-arches-dev/data/geojson/geoarchaeo.geojson",
#'             export.plot = F)
#'
#' @export
geojson_map <- function(map.name = "map",
                        geojson.path = paste0(system.file(package = "eamenaR"),
                                              "/extdata/caravanserail.geojson"),
                        ids = "EAMENA ID",
                        field.names = NA,
                        highlights.ids = NA,
                        symbology = paste0(system.file(package = "eamenaR"),
                                           "/extdata/symbology.xlsx"),
                        stamen.zoom = NA,
                        fields.for.labels = c("Site Feature Interpretation Type",
                                              "Cultural Period Type",
                                              "Administrative Division ",
                                              "Country Type ",
                                              "Overall Condition State Type"),
                        interactive = F,
                        export.plot = F,
                        dirOut = paste0(system.file(package = "eamenaR"),
                                        "/results/"),
                        fig.width = 8,
                        fig.height = 8){
  # field.names <- "Overall Condition State Type"
  # TODO: generalise from point to other geometries: centroid Polygon, Lines
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  symbology <- openxlsx::read.xlsx(symbology)
  ea.geojson <- geojsonsf::geojson_sf(geojson.path)
  if(is.na(stamen.zoom)){
    bbox <- sf::st_bbox(ea.geojson)
    stamen.zoom <- rosm:::tile.raster.autozoom(
      rosm::extract_bbox(
        matrix(bbox, ncol = 2, byrow = TRUE)),
      epsg = 4326)
  }
  ea.geojson <- sf::st_zm(ea.geojson) # rm Z
  ea.geojson.geom.types <- sf::st_geometry_type(ea.geojson$geometry)
  # Pt, Ln, Pl
  ea.geojson.point <- ea.geojson[sf::st_geometry_type(ea.geojson$geometry) == "POINT", ]
  ea.geojson.point$idf <- rownames(ea.geojson.point)
  ea.geojson.line <- ea.geojson[sf::st_geometry_type(ea.geojson$geometry) == "LINESTRING", ]
  ea.geojson.line$idf <- rownames(ea.geojson.line)
  ea.geojson.polygon <- ea.geojson[sf::st_geometry_type(ea.geojson$geometry) %in% c("POLYGON", "MULTIPOLYGON"), ]
  ea.geojson.polygon$idf <- rownames(ea.geojson.polygon)
  if(!is.na(highlights.ids)){
    ea.geojson.highlights.point <- row.names(ea.geojson.point[ea.geojson.point@data[ , ids] %in% highlights.ids, ])
    ea.geojson.highlights.line <- row.names(ea.geojson.line[ea.geojson.line@data[ , ids] %in% highlights.ids, ])
    ea.geojson.highlights.polygon <- row.names(ea.geojson.polygon[ea.geojson.polygon@data[ , ids] %in% highlights.ids, ])
  }

  # non plolty
  if(!interactive){
    left <- as.numeric(sf::st_bbox(ea.geojson.point)$xmin)
    bottom <- as.numeric(sf::st_bbox(ea.geojson.point)$ymin)
    right <- as.numeric(sf::st_bbox(ea.geojson.point)$xmax)
    top <- as.numeric(sf::st_bbox(ea.geojson.point)$ymax)
    buffer <- mean(c(abs(left - right), abs(top - bottom)))/10
    bbox <- c(left = left - buffer,
              bottom = bottom - buffer,
              right = right + buffer,
              top = top + buffer
    )
    stamenbck <- ggmap::get_stamenmap(bbox,
                                      zoom = stamen.zoom,
                                      maptype = "terrain-background")
    cpt.field.name <- 0
    if(!is.na(field.names)){
      # one map by field, if there are different values for the same field: one by value
      cpt.field.value <- 0
      cpt.field.name <- cpt.field.name + 1
      print(paste0("* there is/are '", length(field.names),"' different field name to read"))
      for(field.name in field.names){
        # field.name <- "Damage Extent Type"
        # field.name <- "Disturbance Cause Type "
        # field.name <- "Overall Condition State Type"
        # cpt.field.value <- 0
        # cpt.field.name <- cpt.field.name + 1
        # print(paste0(" ", cpt.field.name, "/",length(field.names),")    read '", field.name,"' field name"))
        aggregated.unique.values <- unique(ea.geojson.point[[field.name]])
        splitted.values <- stringr::str_split(aggregated.unique.values, ", ")
        different.values.in.the.field <- Reduce("|", unlist(lapply(splitted.values, length)) > 1)
        if(different.values.in.the.field){
          splitted.unique.values <- unique(unlist(splitted.values))
          splitted.unique.values <- as.character(na.omit(splitted.unique.values))
        }
        # print(paste0("*       - there is/are ", length(splitted.unique.values)," different field values to read"))
        symbology.field <- symbology[symbology$list == field.name, c("values", "colors")]
        if(nrow(symbology.field) == 0){
          # default colors
          coloramp <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
          colors <- colorRampPalette(coloramp)(length(splitted.unique.values))
          symbology.field <- data.frame(values = splitted.unique.values,
                                        colors = colors)
        }
        names(symbology.field)[names(symbology.field) == 'values'] <- field.name

        if(different.values.in.the.field){
          # as many maps as there are different values
          for(field.value in splitted.unique.values){
            # field.value <- "Water and/or Wind Action"
            cpt.field.value <- cpt.field.value + 1
            print(paste0("        ", cpt.field.value, "/",
                         length(splitted.unique.values),")    read '",
                         field.value,"' field value"))
            ea.geojson.point.sub <- ea.geojson.point[grep(field.value, ea.geojson.point[[field.name]]), ]
            ea.geojson.point.sub <- merge(ea.geojson.point.sub,
                                          symbology.field, by = field.name,
                                          all.x = T)
            ea.geojson.point.sub$colors[is.na(ea.geojson.point.sub$colors)] <- "#808080"
            if(nrow(ea.geojson.point.sub) > 0){
              gmap <- ggmap::ggmap(stamenbck) +
                ggplot2::geom_sf(data = ea.geojson.point,
                                 ggplot2::aes(color = "grey"),
                                 inherit.aes = FALSE) +
                ggplot2::geom_sf(data = ea.geojson.point.sub,
                                 ggplot2::aes(color = "red"),
                                 inherit.aes = FALSE) +
                ggrepel::geom_text_repel(data = ea.geojson.point.sub,
                                         ggplot2::aes(x = sf::st_coordinates(ea.geojson.point.sub)[, "X"],
                                                      y = sf::st_coordinates(ea.geojson.point.sub)[, "Y"],
                                                      label = idf),
                                         size = 2,
                                         segment.color = "black",
                                         segment.size = .1,
                                         segment.alpha = .5,
                                         min.segment.length = .3,
                                         force = .5,
                                         max.time = 1.5,
                                         max.overlaps = Inf,
                                         inherit.aes = FALSE) +
                ggplot2::scale_color_identity(guide = "legend",
                                              name = field.name,
                                              label = c("other", field.value)) +
                ggplot2::labs(title = map.name,
                              subtitle = paste0(field.name, " = ", field.value)) +
                ggplot2::theme(plot.title = ggplot2::element_text(size = 15,
                                                                  hjust = 0.5),
                               plot.subtitle = ggplot2::element_text(size = 12,
                                                                     hjust = 0.5))
              if (export.plot) {
                dir.create(dirOut, showWarnings = FALSE)
                field.value.norm <- gsub("/", "_", field.value)
                field.value.norm <- gsub(" ", "_", field.value.norm)
                field.value.norm <- gsub("%", "perc", field.value.norm)
                gout <- paste0(dirOut, map.name, "_", field.name, "_",
                               field.value.norm, ".png")
                ggplot2::ggsave(gout, gmap,
                                width = fig.width,
                                height = fig.height)
                print(paste(gout, "is exported"))
              } else {
                print(gmap)
              }
            }
          }
        }

        if(!different.values.in.the.field){
          # only 1 value by field
          ea.geojson.point <- merge(ea.geojson.point, symbology.field, by = field.name, all.x = T)
          ea.geojson.point$colors[is.na(ea.geojson.point$colors)] <- "#808080"
          if(nrow(ea.geojson.point) > 0){
            gmap <- ggmap::ggmap(stamenbck) +
              ggplot2::geom_sf(data = ea.geojson.point,
                               ggplot2::aes(color = colors),
                               # colour = "black",
                               inherit.aes = FALSE) +
              ggrepel::geom_text_repel(data = ea.geojson.point,
                                       ggplot2::aes(x = sf::st_coordinates(ea.geojson.point)[, "X"],
                                                    y = sf::st_coordinates(ea.geojson.point)[, "Y"],
                                                    label = rownames(ea.geojson.point)),
                                       size = 2,
                                       segment.color = "black",
                                       segment.size = .1,
                                       segment.alpha = .5,
                                       min.segment.length = .3,
                                       force = .5,
                                       max.time = 1.5,
                                       max.overlaps = Inf,
                                       inherit.aes = FALSE) +
              ggplot2::scale_color_identity(guide = "legend",
                                            name = field.name,
                                            label = symbology.field[[field.name]]) +
              ggplot2::labs(title = map.name) +
              ggplot2::theme(plot.title = ggplot2::element_text(size = 15,
                                                                hjust = 0.5))
          }
          if (export.plot) {
            dir.create(dirOut, showWarnings = FALSE)
            field.value.norm <- gsub("/", "_", field.name)
            field.value.norm <- gsub(" ", "_", field.name)
            # field.value.norm <- gsub("%", "perc", field.value.norm)
            gout <- paste0(dirOut, map.name, "_", field.name, ".png")
            ggplot2::ggsave(gout, gmap,
                            width = fig.width,
                            height = fig.height)
            print(paste(gout, "is exported"))
          } else {
            print(gmap)
          }
        }
      }
    } else {
      # general map
      ea.geojson.point.sub <- ea.geojson.point
      gmap <- ggmap::ggmap(stamenbck) +
        ggplot2::geom_sf(data = ea.geojson.point.sub,
                         colour = "black",
                         inherit.aes = FALSE) +
        ggrepel::geom_text_repel(data = ea.geojson.point.sub,
                                 ggplot2::aes(x = sf::st_coordinates(ea.geojson.point.sub)[, "X"],
                                              y = sf::st_coordinates(ea.geojson.point.sub)[, "Y"],
                                              label = rownames(ea.geojson.point.sub)),
                                 size = 2,
                                 segment.color = "black",
                                 segment.size = .2,
                                 segment.alpha = .5,
                                 min.segment.length = .1,
                                 force = .4,
                                 max.time = 1.5,
                                 max.iter = 30000,
                                 max.overlaps = Inf,
                                 inherit.aes = FALSE) +
        ggplot2::labs(title = map.name) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 15,
                                                          hjust = 0.5))
      if (export.plot) {
        dir.create(dirOut, showWarnings = FALSE)
        gout <- paste0(dirOut, map.name, ".png")
        ggplot2::ggsave(gout, gmap,
                        width = fig.width,
                        height = fig.height)
        print(paste(gout, "is exported"))
      } else {
        print(gmap)
      }
    }
  }

  # yes plotly
  if(interactive){
    # labels
    labels.pt <-"paste0('<b>', ea.geojson.point[[ids]][a.pt],'</b>'"
    labels.ln <-"paste0('<b>', ea.geojson.line[[ids]][a.pt],'</b>'"
    labels.pl <-"paste0('<b>', ea.geojson.polygon[[ids]][a.pt],'</b>'"
    for(ffl in fields.for.labels){
      labels.pt <- paste0(labels.pt,
                          paste0(", '<br>', ea.geojson.point[['", ffl, "']][a.pt]"))
      labels.ln <- paste0(labels.ln,
                          paste0(", '<br>', ea.geojson.line[['", ffl, "']][a.pt]"))
      labels.pl <- paste0(labels.pl,
                          paste0(", '<br>', ea.geojson.polygon[['", ffl, "']][a.pt]"))
    }
    labels.pt <- paste0(labels.pt, ")")
    labels.ln <- paste0(labels.ln, ")")
    labels.pl <- paste0(labels.pl, ")")
    # add geometries
    if(nrow(ea.geojson.point) > 0){
      ea.geojson.point$lbl <- NA
      for(a.pt in seq(1, nrow(ea.geojson.point))){
        ea.geojson.point[a.pt, "lbl"] <- eval(parse(text = labels.pt))
        # ea.geojson.point[a.pt, "lbl"] <- paste0("<b>", ea.geojson.point[[ids]][a.pt],"</b><br>",
        #                                         ea.geojson.point[["Site Feature Interpretation Type"]][a.pt],
        #                                         " - ", ea.geojson.point[["Cultural Period Type"]][a.pt], " - ",
        #                                         ea.geojson.point[["Administrative Division "]][a.pt], " ",
        #                                         ea.geojson.point[["Country Type "]][a.pt], "<br>")
      }
    }
    if(nrow(ea.geojson.line) > 0){
      ea.geojson.line$lbl <- NA
      for(a.pt in seq(1, nrow(ea.geojson.line))){
        ea.geojson.line[a.pt, "lbl"] <- eval(parse(text = labels.ln))
        # ea.geojson.line[a.pt, "lbl"] <- paste0("<b>", ea.geojson.line[[ids]][a.pt],"</b><br>",
        #                                         ea.geojson.line[["Site Feature Interpretation Type"]][a.pt],
        #                                         " - ", ea.geojson.line[["Cultural Period Type"]][a.pt], " - ",
        #                                         ea.geojson.line[["Administrative Division "]][a.pt], " ",
        #                                         ea.geojson.line[["Country Type "]][a.pt], "<br>")
      }
    }
    if(nrow(ea.geojson.polygon) > 0){
      ea.geojson.polygon$lbl <- NA
      for(a.pt in seq(1, nrow(ea.geojson.polygon))){
        ea.geojson.polygon[a.pt, "lbl"] <- eval(parse(text = labels.pl))
        # ea.geojson.polygon[a.pt, "lbl"] <- paste0("<b>", ea.geojson.polygon[[ids]][a.pt],"</b><br>",
        #                                        ea.geojson.polygon[["Site Feature Interpretation Type"]][a.pt],
        #                                        " - ", ea.geojson.polygon[["Cultural Period Type"]][a.pt], " - ",
        #                                        ea.geojson.polygon[["Administrative Division "]][a.pt], " ",
        #                                        ea.geojson.polygon[["Country Type "]][a.pt], "<br>")
      }
    }
    ea.map <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$"Esri.WorldImagery",
                                group = "Ortho") %>%
      leaflet::addProviderTiles(leaflet::providers$"OpenStreetMap",
                                group = "OSM")
    if(nrow(ea.geojson.point) > 0){
      ea.map <- ea.map %>%
        leaflet::addCircleMarkers(data = ea.geojson.point,
                                  weight = 1,
                                  radius = 3,
                                  popup = ~lbl,
                                  label = ~idf,
                                  fillOpacity = .5,
                                  opacity = .8)
    }
    if(nrow(ea.geojson.line) > 0){
      ea.map <- ea.map %>%
        leaflet::addPolylines(data = ea.geojson.line,
                              weight = 1,
                              popup = ~lbl,
                              label = ~idf,
                              fillOpacity = .5,
                              opacity = .8)
    }
    if(nrow(ea.geojson.polygon) > 0){
      ea.map <- ea.map %>%
        leaflet::addPolygons(data = ea.geojson.polygon,
                             weight = 1,
                             popup = ~lbl,
                             label = ~idf,
                             fillOpacity = 0,
                             opacity = .8)
    }
    ea.map <- ea.map %>%
      leaflet::addLayersControl(
        baseGroups = c("Ortho", "OSM"),
        position = "topright") %>%
      leaflet::addScaleBar(position = "bottomright")

    if(!is.na(highlights.ids)){
      if(length(ea.geojson.highlights.point) > 0){
        hl.geom <- ea.geojson.point[rownames(ea.geojson.point@data) == ea.geojson.highlights.point, ]
        ea.map <- ea.map %>%
          leaflet::addCircleMarkers(
            data = hl.geom,
            weight = 1,
            radius = 4,
            popup = ~lbl,
            label = hl.geom[ , ids],
            color = "red",
            fillOpacity = 1,
            opacity = 1)
      }
      if(length(ea.geojson.highlights.line) > 0){
        hl.geom <- ea.geojson.line[rownames(ea.geojson.line@data) == ea.geojson.highlights.line, ]
        ea.map <- ea.map %>%
          leaflet::addPolylines(# lng = ~Longitude,
            data = hl.geom,
            weight = 2,
            color = "red",
            popup = ~lbl,
            label = hl.geom[ , ids],
            fillOpacity = .5,
            opacity = .8)
      }
      if(length(ea.geojson.highlights.polygon) > 0){
        hl.geom <- ea.geojson.polygon[rownames(ea.geojson.polygon@data) == ea.geojson.highlights.polygon, ]
        ea.map <- ea.map %>%
          leaflet::addPolygons(# lng = ~Longitude,
            data = hl.geom,
            weight = 5,
            color = "red",
            popup = ~lbl,
            label = hl.geom[ , ids],
            fillOpacity = .5,
            opacity = .8)
      }
    }
    if (export.plot) {
      dir.create(dirOut, showWarnings = FALSE)
      gout <- paste0(dirOut, map.name, ".html")
      htmlwidgets::saveWidget(ea.map, gout)
      print(paste(gout, "is exported"))
    } else {
      print(ea.map)
    }
  }
}

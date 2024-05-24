#' Create map, whether static or interactive leaflet, from GeoJSON file
#'
#' @name geojson_map
#'
#' @description Create distribution map
#'
#' @param map.name name of output map and name of saved file (if export.plot is TRUE). Default "map".
#' @param geojson.path path of GeoJSON file. Default 'caravanserail.geojson'.
#' @param geojson.grids optional. path of GeoJSON file to show also grids.
#' @param zoom An integer value for the zoom for the Stadiamap basemap. Default NA, will be calculated automatically.
#' @param ids IDs of resources, default "EAMENA.ID" (R fieldname format, without spaces).
#' @param field.names a vector of one or many field names for thematic cartography. If NA (default), will create a general map
#' @param highlights.ids EAMENA IDs (ex: 'EAMENA-0205783') that will be highlighted in map. If NA (default), no highlights.
#' @param symbology path to XLSX recording symbology for different values, default 'symbology.xlsx'.
#' @param maptype type of background basemap, from the  basemaps:: package
#' @param ncol the number of columns
#' @param max.maps to limit the total number of exported maps. Default: NA.
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
#' # read from Zenodo and return a list of maps
#' map.name <- "Sistan_dataset_Damages_Extent"
#' all.g <- geojson_map(map.name = map.name,
#'                      field.names = c("Damage Extent Type"),
#'                      geojson.path = "https://doi.org/10.5281/zenodo.10375902",
#'                      max.maps = 6,
#'                      hp.color = "black",
#'                      hp.color.bck = "grey",
#'                      hp.size = 1.5)
#'
#' # save an interactive map
#' geojson_map(map.name = "caravanserail_plotly",
#'             interactive = T,
#'             export.plot = T)
#'
#' # plot a general map of geoarchaeological data
#' geojson_map(map.name = "geoarch",
#'             ids = "GEOARCH.ID",
#'             geojson.path = "C:/Rprojects/eamena-arches-dev/data/geojson/geoarchaeo.geojson",
#'             export.plot = F)
#'
#' @export
geojson_map <- function(map.name = "map",
                        geojson.path = paste0(system.file(package = "eamenaR"),
                                              "/extdata/caravanserail.geojson"),
                        geojson.grids = NA,
                        zoom = NA,
                        ids = "EAMENA ID",
                        field.names = NA,
                        symbology = paste0(system.file(package = "eamenaR"),
                                           "/extdata/symbology.xlsx"),
                        hp.color = "red",
                        hp.color.bck = "grey",
                        hp.size = 2,
                        maptype = c("esri", "world_imagery"),
                        # maptype = "terrain-background",
                        # stamen.zoom = NA,
                        fields.for.labels = c("Site Feature Interpretation Type",
                                              "Cultural Period Type",
                                              "Administrative Division ",
                                              "Country Type ",
                                              "Overall Condition State Type"),
                        ncol = 4,
                        max.maps = NA,
                        interactive = F,
                        export.plot = F,
                        dirOut = paste0(system.file(package = "eamenaR"),
                                        "/results/"),
                        fig.width = 8,
                        fig.height = 8){
  # field.names <- "Overall Condition State Type"
  # TODO: generalise from point to other geometries: centroid Polygon, Lines
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  symbology.df <- openxlsx::read.xlsx(symbology)
  geojson.read <- eamenaR::geojson_read(geojson.path)
  ea.geojson <- geojson.read[[1]]
  capt <- geojson.read[[2]]
  # #################################################
  # # TODO: factorise this 'read' part to geojson_stats, etc.
  # if(is.character(geojson.path) & DescTools::SplitPath(geojson.path)$extension == "geojson"){
  #   if(verbose){
  #     print(paste0("ROI: Reads a GeoJSON path"))
  #   }
  #   ea.geojson <- sf::read_sf(geojson.path, quiet = TRUE)
  #   capt <- SplitPath(geojson.path)$fullfilename
  #   # where.roi <- sf::st_read(roi, quiet = TRUE)
  # }
  # # geojson.path = "https://zenodo.org/api/records/10375902/files/sistan_part1_hps.zip/content"
  # if(grep('zenodo', geojson.path)){
  # # if(grep('zenodo.*zip', geojson.path)){
  #   if(verbose){
  #     print(paste0("ROI: Download a ZIP file hosted on Zenodo"))
  #   }
  #   geojson.path.api <- gsub("doi.org/.*/zenodo.", replacement = "zenodo.org/api/records/", geojson.path)
  #   if(verbose){
  #     print(paste0("The Zenodo DOI has been converted into an URL API"))
  #   }
  #   temp <- tempfile()
  #   response <- httr::GET(geojson.path.api)
  #   response_content <- httr::content(response, "text")
  #   response_json <- jsonlite::fromJSON(response_content)
  #   geo.file <- as.character(response_json$files$links)
  #   download.file(geo.file,
  #                 destfile = temp, quiet = TRUE, mode = "wb")
  #   td <- tempdir()
  #   lfiles <- unzip(temp, exdir = td)
  #   if(verbose){
  #     cat("Downloaded file(s):", lfiles, "\n")
  #   }
  #   # filter to find the GeoJSON
  #   geojson_paths <- lfiles[grepl("\\.geojson$", lfiles, ignore.case = TRUE)]
  #   if(length(geojson_paths) == 0){
  #     stop("No GeoJSON files have been found in the Zenodo URL")
  #   }
  #   # compute the Zenodo citation (APA)
  #   bib.authors <- as.character(response_json$metadata$creators[1])
  #   bib.title <- response_json$title
  #   bib.url <- response_json$doi_url
  #   bib.year <- format(as.Date(response_json$metadata$publication_date, format = "%Y-%m-%d"), "%Y")
  #   bib.type <- response_json$metadata$resource_type$type
  #   capt <- paste0(bib.authors, ". (", bib.year, "). ", bib.title, " [", bib.type, "]. Zenodo. ", bib.url)
  #   # read only the first GeoJSON as it is expected to have one GeoJSON by ZIP
  #   ea.geojson <- sf::read_sf(geojson_paths[1], quiet = TRUE)
  # }
  #####################
  bbox_values <- sf::st_bbox(ea.geojson)
  bbox <- sf::st_as_sfc(bbox_values)
  ea.geojson <- sf::st_zm(ea.geojson) # rm Z
  ea.geojson.geom.types <- sf::st_geometry_type(ea.geojson$geometry)
  # Pt, Ln, Pl
  ea.geojson.point <- ea.geojson[sf::st_geometry_type(ea.geojson$geometry) == "POINT", ]
  ea.geojson.point$idf <- rownames(ea.geojson.point)
  ea.geojson.line <- ea.geojson[sf::st_geometry_type(ea.geojson$geometry) == "LINESTRING", ]
  ea.geojson.line$idf <- rownames(ea.geojson.line)
  ea.geojson.polygon <- ea.geojson[sf::st_geometry_type(ea.geojson$geometry) %in% c("POLYGON", "MULTIPOLYGON"), ]
  ea.geojson.polygon$idf <- rownames(ea.geojson.polygon)
  # non plolty
  if(!interactive){
    # cast Line and polygons to points
    ea.geojson.line.point <- sf::st_centroid(ea.geojson.line)
    ea.geojson.polygon.point <- sf::st_centroid(ea.geojson.polygon)
    ea.geojson.point.all <- rbind(ea.geojson.point, ea.geojson.line.point)
    ea.geojson.point.all <- rbind(ea.geojson.point.all, ea.geojson.polygon.point)

    ggmap::register_stadiamaps("aa5c9739-90c7-410b-9e9b-6c904df6e4dd")
    if(is.na(geojson.grids)[1]){
      # no grids to plot
      buff <- .1
      left <- as.numeric(sf::st_bbox(ea.geojson.point.all)$xmin)
      left.L <- left - buff # floor(left)
      bottom <- as.numeric(sf::st_bbox(ea.geojson.point.all)$ymin)
      bottom.L <- bottom - buff #floor(bottom)
      right <- as.numeric(sf::st_bbox(ea.geojson.point.all)$xmax)
      right.L <- right + buff # ceiling(right)
      top <- as.numeric(sf::st_bbox(ea.geojson.point.all)$ymax)
      top.L <- top + buff # ceiling(top)
    } else {
      buff <- .05
      ea.geojson.grids <- sf::read_sf(geojson.grids)
      left <- as.numeric(sf::st_bbox(ea.geojson.grids)$xmin)
      left.L <- left - buff # floor(left)
      bottom <- as.numeric(sf::st_bbox(ea.geojson.grids)$ymin)
      bottom.L <- bottom - buff #floor(bottom)
      right <- as.numeric(sf::st_bbox(ea.geojson.grids)$xmax)
      right.L <- right + buff # ceiling(right)
      top <- as.numeric(sf::st_bbox(ea.geojson.grids)$ymax)
      top.L <- top + buff # ceiling(top)
    }
    if(is.na(zoom)){
      if(verbose){
        print(paste0("Basemap: will calculate the zoom automatically"))
      }
      lon_range <- c(bbox_values["xmin"], bbox_values["xmax"])
      lat_range <- c(bbox_values["ymin"], bbox_values["ymax"])
      zoom <- ggmap::calc_zoom(lon_range, lat_range)
    }
    stamenbck <- ggmap::get_stadiamap(bbox = c(left = left.L,
                                               bottom = bottom.L,
                                               right = right.L,
                                               top = top.L),
                                      maptype = "stamen_terrain_background",
                                      crop = TRUE,
                                      zoom = zoom)
    cpt.field.name <- 0
    if(!is.na(field.names)){
      # one map by field, if there are different values for the same field: one by value
      cpt.field.value <- 0
      cpt.field.name <- cpt.field.name + 1
      if(verbose){
        print(paste0("* there is/are '", length(field.names),"' different field name to read"))
      }
      for(field.name in field.names){
        # field.name <- "Damage Extent Type"
        # field.name <- "Disturbance Cause Type "
        # field.name <- "Overall Condition State Type"
        # field.name = c("Disturbance Cause Category Type")

        # cpt.field.value <- 0
        # cpt.field.name <- cpt.field.name + 1
        # print(paste0(" ", cpt.field.name, "/",length(field.names),")    read '", field.name,"' field name"))

        if(!is.na(max.maps)){
          unic.values <- as.data.frame(table(ea.geojson.point.all[[field.name]]))
          df.val.simple <- data.frame(val = character(),
                                      nb = numeric())
          for (i in 1:nrow(unic.values)){
            # i <- 2
            nb <- unic.values[i, "Freq"]
            values <- unic.values[i, "Var1"]
            values <- unlist(stringr::str_split(values, ","))
            values <- trimws(values) # trim spaces
            values <- values[values != ""] # rm empty strings
            for (j in 1:length(values)){
              df.val.simple[nrow(df.val.simple) + 1,] = c(values[j], nb)
            }
          }
          df.val.simple$nb <- as.numeric(df.val.simple$nb)
          # df.periods.simple$period <- trimws(df.periods.simple$period) # trim spaces
          df.val.simple <- aggregate(nb ~ val, data = df.val.simple, sum)
          df.val.simple <- df.val.simple[order(df.val.simple$nb, decreasing = TRUE), ]
          # max.maps <- 9
          df.val.simple <- df.val.simple[c(1:max.maps), ]
          aggregated.unique.values <- unique(ea.geojson.point.all[[field.name]])
          splitted.values <- stringr::str_split(aggregated.unique.values, ", ")
          different.values.in.the.field <- Reduce("|", unlist(lapply(splitted.values, length)) > 1)
          splitted.unique.values <- df.val.simple$val
        } else {
          aggregated.unique.values <- unique(ea.geojson.point.all[[field.name]])
          splitted.values <- stringr::str_split(aggregated.unique.values, ", ")
          different.values.in.the.field <- Reduce("|", unlist(lapply(splitted.values, length)) > 1)
          if(different.values.in.the.field){
            splitted.unique.values <- unique(unlist(splitted.values))
            splitted.unique.values <- as.character(na.omit(splitted.unique.values))
          }
        }
        if(verbose){
          print(paste0("*       - there is/are ", length(splitted.unique.values)," different field values to read"))
        }
        # symbology.df <- openxlsx::read.xlsx(symbology)
        symbology.field <- symbology.df[symbology.df$list == field.name, c("values", "hexa")]
        if(nrow(symbology.field) == 0){
          if(verbose){
            print(paste0("*       - add default colors"))
          }
          coloramp <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
          colors <- colorRampPalette(coloramp)(length(splitted.unique.values))
          symbology.field <- data.frame(values = splitted.unique.values,
                                        colors = colors)
        }
        names(symbology.field)[names(symbology.field) == 'values'] <- field.name
        # - - - - - - - - - - - - - - - - - - -
        if(different.values.in.the.field){
          # as many maps as there are different values
          all.g <- list()
          cpt.field.value <- 0
          splitted.unique.values <- splitted.unique.values[!splitted.unique.values %in% c("")]
          for(field.value in splitted.unique.values){
            # splitted.unique.values <- splitted.unique.values[1:5]
            # field.value <- "Water and/or Wind Action"
            # field.value <- "Agricultural/Pastoral"
            # field.value <- "Infrastructure/Transport"
            cpt.field.value <- cpt.field.value + 1
            print(paste0("        ", cpt.field.value, "/",
                         length(splitted.unique.values),")    read '",
                         field.value,"' field value"))
            ea.geojson.point.sub <- ea.geojson.point.all[grep(field.value, ea.geojson.point.all[[field.name]]), ]
            ea.geojson.point.sub <- merge(ea.geojson.point.sub,
                                          symbology.field, by = field.name,
                                          all.x = T)
            # useful?
            # ea.geojson.point.sub$colors[is.na(ea.geojson.point.sub$colors)] <- hp.color.bck

            tit <- paste0(field.name, ": ", field.value, " (n=", as.character(nrow(ea.geojson.point.sub)), ")")
            print(paste("Creates", tit))
            if(nrow(ea.geojson.point.sub) > 0){
              gmap <- ggmap::ggmap(stamenbck) +
                # ggplot2::ggtitle(label = tit,
                #                  subtitle = )
              if(!is.na(geojson.grids)[1]){
                gmap <- gmap +
                  ggplot2::geom_sf(data = ea.geojson.grids,
                                   color = "darkgrey", fill = NA,
                                   # ggplot2::aes(),
                                   inherit.aes = FALSE)
              }
              gmap <- gmap +
                ggplot2::geom_sf(data = ea.geojson.point.all,
                                 ggplot2::aes(color = hp.color.bck),
                                 size = hp.size,
                                 inherit.aes = FALSE) +
                ggplot2::geom_sf(data = ea.geojson.point.sub,
                                 ggplot2::aes(color = hp.color),
                                 size = hp.size,
                                 inherit.aes = FALSE) +
                ggplot2::scale_color_identity(# guide = "legend",
                  name = field.name,
                  label = c("other", field.value)) +
                ggplot2::labs(title = tit,
                              caption = paste0("Data source:\n", capt)) +
                ggplot2::scale_x_continuous(breaks = seq(floor(left), ceiling(right), by = .5)) +
                ggplot2::scale_y_continuous(breaks = seq(floor(bottom), ceiling(top), by = .5)) +
                ggplot2::theme(plot.title = ggplot2::element_text(size = 12,
                                                                  hjust = 0.5),
                               axis.title = ggplot2::element_blank(),
                               plot.caption = ggplot2::element_text(size = 6, hjust = 0))
              # add to list
              print("map created")
              # if (!export.plot) {
              #   return(all.g)
              # }
              all.g[[length(all.g) + 1]] <- gmap
            }
          }
          if (export.plot) {
            dir.create(dirOut, showWarnings = FALSE)
            field.name.norm <- gsub("/", "_", field.name)
            field.name.norm <- gsub(" ", "_", field.name.norm)
            field.name.norm <- gsub("%", "perc", field.name.norm)
            gout <- paste0(dirOut, map.name, "_", field.name.norm, ".png")
            margin <- ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"))
            ggplot2::ggsave(file = gout,
                            gridExtra::arrangeGrob(#top = field.name,
                              top = grid::textGrob(field.name, gp = grid::gpar(fontsize = 14)),
                              grobs = lapply(all.g, "+", margin), ncol = ncol),
                            width = fig.width,
                            height = fig.height)

            # change to grid arrange
            gout <- paste0(dirOut, map.name, ".png")
            ggplot2::ggsave(gout, gmap,
                            width = fig.width,
                            height = fig.height)
            print(paste(gout, "is exported"))
          }

          return(all.g)
        }

        if(!different.values.in.the.field){
          # only 1 value by field
          ea.geojson.point <- merge(ea.geojson.point, symbology.field, by = field.name, all.x = T)
          ea.geojson.point$colors[is.na(ea.geojson.point$colors)] <- "#808080"
          if(nrow(ea.geojson.point) > 0){

            # gmap <- ggmap::ggmap(stamenbck) +
            gmap <- ggplot2::ggplot() +
              basemaps::basemap_gglayer(bbox) +
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

      # gmap <- ggmap::ggmap(stamenbck) +
      gmap <- ggplot2::ggplot() +
        basemaps::basemap_gglayer(bbox) +
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

geojson_map <- function(map.name = "map",
                        geojson.path = "C:/Rprojects/eamenaR/results/xxx/caravanserais.geojson",
                        ids = "EAMENA ID",
                        field.names = NA,
                        highlights.ids = NA,
                        symbology = paste0(system.file(package = "eamenaR"),
                                           "/extdata/symbology.xlsx"),
                        maptype = "terrain-background",
                        stamen.zoom = 8,
                        fields.for.labels = c("Site Feature Interpretation Type",
                                              "Cultural Period Type",
                                              "Administrative Division ",
                                              "Country Type ",
                                              "Overall Condition State Type"),
                        interactive = F,
                        export.plot = F,
                        dirOut = "C:/Rprojects/eamenaR/results/xxx/",
                        fig.width = 8,
                        fig.height = 8){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  ea.geojson <- sf::st_read(geojson.path, quiet = TRUE)
  ea.geojson <- sf::st_zm(ea.geojson) # rm Z
  ea.geojson.geom.types <- sf::st_geometry_type(ea.geojson$geometry)
  # Pt, Ln, Pl
  ea.geojson.point <- ea.geojson[sf::st_geometry_type(ea.geojson$geometry) == "POINT", ]
  ea.geojson.point$idf <- rownames(ea.geojson.point)
  ea.geojson.line <- ea.geojson[sf::st_geometry_type(ea.geojson$geometry) == "LINESTRING", ]
  ea.geojson.line$idf <- rownames(ea.geojson.line)
  ea.geojson.polygon <- ea.geojson[sf::st_geometry_type(ea.geojson$geometry) %in% c("POLYGON", "MULTIPOLYGON"), ]
  ea.geojson.polygon$idf <- rownames(ea.geojson.polygon)
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
    }
  }
  if(nrow(ea.geojson.line) > 0){
    ea.geojson.line$lbl <- NA
    for(a.pt in seq(1, nrow(ea.geojson.line))){
      ea.geojson.line[a.pt, "lbl"] <- eval(parse(text = labels.ln))
    }
  }
  if(nrow(ea.geojson.polygon) > 0){
    ea.geojson.polygon$lbl <- NA
    for(a.pt in seq(1, nrow(ea.geojson.polygon))){
      ea.geojson.polygon[a.pt, "lbl"] <- eval(parse(text = labels.pl))
    }
  }
  ea.map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$"Esri.WorldImagery",
                              group = "Ortho") %>%
    leaflet::addProviderTiles(leaflet::providers$"OpenStreetMap",
                              group = "OSM") %>%
    leaflet.extras2::addWMS(group = "Clim",
                            baseUrl = "http://54.155.109.226:8080/geoserver/ows",
                            layers = "Beck_KG_V1_present_0p0083",
                            options = leaflet::WMSTileOptions(
                              transparent = TRUE,
                              format = "image/png",
                              info_format = "text/html")
    ) %>%
    leaflet::fitBounds(left, bottom, right, top)
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
      baseGroups = c("Clim", "Ortho", "OSM"),
      position = "topright") %>%
    leaflet::addScaleBar(position = "bottomright")

  ea.map

  ## export as PNG
  # m <- leaflet::leaflet() %>% leaflet::addTiles()
  mapview::mapshot(ea.map,
                   file = paste0(dirOut, map.name, ".png")
  )

}






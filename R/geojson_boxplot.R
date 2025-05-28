#' Create boxplot or various boxplots of path lengths between different heritage places
#'
#' @name geojson_boxplot
#'
#' @description Create boxplot or various boxplots of path lengths between different heritage places
#'
#' @param stat.name name of output file and plot. Default "caravanserais_areas".
#' @param concept.name key used to identify heritage places. Default "hp.id".
#' @param geojson.path path of GeoJSON file. Default 'caravanserail.geojson'.
#' @param csv.path path to CSV where edges between two heritage places are recorded.
#' @param stat statistic that will be computed. Different values can be "area" for areas, or "dist" for distances between heritage places. Default 'area'.
#' @param chart.type type of chart that will be plot. Default, "boxplot".
#' @param name of field on which paths will be grouped. Example "route". Will create as many plots as there are different categories. By default NA.
#' @param interactive if TRUE, create a Plotly chart. Default, FALSE.
#' @param export.plot if TRUE, will save the plot. Default, FALSE.
#' @param fig.width,fig.height size of output chart.
#' @param dirOut folder where outputs will be saved. Default: '/results'. If it doesn't exist, will be created. Only useful is export plot is TRUE.
#' @param color.set the RBrewer color set. Default "Set1".
#' @param verbose if TRUE (Default), print messages.
#'
#' @return A boxplot
#'
#' @examples
#'
#' # boxplot on areas (by default)
#' geojson_boxplot(stat = "area")
#'
#' # boxplot on distances
#' geojson_boxplot(stat = "dist")
#'
#' # Same as before, but stratified by routes, and saved
#' geojson_boxplot(stat.name = "caravanserais_areas", stat = "area", by = "route",
#'                 export.plot = T)
#' geojson_boxplot(stat.name = "caravanserais_dist", stat = "dist", by = "route",
#'                 export.plot = T)
#'
#' # Same as before, but stratified by routes, interactive, and saved
#' geojson_boxplot(stat.name = "caravanserais_areas", stat = "area", by = "route",
#'                 interactive = T,
#'                 export.plot = T)
#' geojson_boxplot(stat.name = "caravanserais_dist", stat = "dist", by = "route",
#'                 interactive = T,
#'                 export.plot = T)
#'
#' @export
geojson_boxplot <- function(stat.name = "caravanserais_areas",
                            concept.name = "hp.id",
                            geojson.path = paste0(system.file(package = "eamenaR"),
                                                  "/extdata/caravanserail.geojson"),
                            csv.path = paste0(system.file(package = "eamenaR"),
                                              "/extdata/caravanserail_paths.csv"),
                            tit = "Distribution of heritage places' areas",
                            stat = c("area"),
                            chart.type = c("boxplot"),
                            by = NA,
                            interactive = F,
                            export.plot = F,
                            dirOut = paste0(system.file(package = "eamenaR"), "/results/"),
                            fig.width = 9,
                            fig.height = 5,
                            color.set = "Set1",
                            verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  r.id <- eamenaR::ref_ids(concept.name)
  if(!is.na(by)){
    hp.geojson <- ref_routes(geojson.path = geojson.path,
                             csv.path = csv.path,
                             by = by,
                             verbose = verbose)
    by.unique <- unique(hp.geojson[[by]])
    df.colors <- data.frame(by = by.unique,
                            color = RColorBrewer::brewer.pal(length(by.unique), color.set))
    # remane col 1
    names(df.colors)[1] <- by
  } else {
    # change its value to 'by'
    if(inherits(geojson.path, "sf")){
      if(verbose){
        print(paste0("Reads a 'sf' dataframe"))
      }
      hp.geojson <- geojson.path
    }
    if(is.character(geojson.path)){
      if(verbose){
        print(paste0("Reads a path"))
      }
      hp.geojson <- sf::read_sf(geojson.path)
    }
    hp.geojson$by <- 1
    by <- "by"
    df.colors <- data.frame(by = 1,
                            color = "red")
    # remane col 1
    names(df.colors)[1] <- by
  }
  if(chart.type == "boxplot"){
    if(stat == "area"){
      my_subtitle <- paste0(tit)
      if(verbose){print(my_subtitle)}
      if(by != "by"){my_subtitle <-  paste0(my_subtitle, " by '", by,"'")}
      df.measurements <- data.frame(id = hp.geojson[[r.id]],
                                    type = hp.geojson[["Dimension Type"]],
                                    value = hp.geojson[["Measurement Number"]],
                                    scale = hp.geojson[["Measurement Unit"]],
                                    by = hp.geojson[[by]]
      )
      names(df.measurements)[names(df.measurements) == 'by'] <- by
      df.measurements <- na.omit(df.measurements)
      df.measurements$value <- as.numeric(df.measurements$value)
      df.measurements <- df.measurements[!duplicated(df.measurements), ]
      if(by != "by"){
        df.measurements <- merge(df.measurements, df.colors, by = by, all.x = T)
        if(verbose){print(paste0("Area - the boxplots will be filtered on the column: '", by,"'"))}
      } else {
        df.measurements$color <- "red"
      }
      # TODO: improve these filter
      df.measurements <- df.measurements[df.measurements$type == "Area" & df.measurements$scale == "square metres (m2)", ]
      measurements.type <- "areas"
      my_ylab <- paste0(measurements.type, " in ", "square metres (m2)")
    }
    # distances
    if(stat == "dist"){
      df.measurements <- geojson_format_path(geojson.path = geojson.path,
                                             csv.path = csv.path,
                                             concept.name = concept.name,
                                             by = by,
                                             verbose = verbose)
      my_subtitle <- paste0("Distribution of distances between two heritage places (n = ",
                            nrow(df.measurements),
                            ")")
      if(by != "by"){my_subtitle <-  paste0(my_subtitle, " by '", by,"'")}
      if(verbose){print(my_subtitle)}
      names(df.measurements)[names(df.measurements) == 'dist.m'] <- 'value'
      if(by != "by"){
        df.measurements <- merge(df.measurements, df.colors, by = by, all.x = T)
        if(verbose){print(paste0("Dist - the boxplots will be filtered on the column: '", by,"'"))}
      } else {
        df.measurements$color <- "red"
      }
      measurements.type <- "distances"
      my_ylab <- paste0(measurements.type, " in ", "meters")
    }
  }
  if(!interactive){
    if(verbose){print(paste0("create a ggplot"))}
    my_theme <- list(
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(size = 10),
                     plot.subtitle = ggplot2::element_text(size = 8),
                     axis.text.x = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(size = 8, angle = 90, vjust = 0, hjust=0.5),
                     axis.ticks.length = ggplot2::unit(2, "pt"),
                     axis.ticks = ggplot2::element_line(colour = "black", size = 0.2),
                     panel.border = ggplot2::element_rect(colour= "black", size = 0.2),
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x =  ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_line(colour = "lightgrey", size = 0.1),
                     panel.spacing = ggplot2::unit(2, "mm"),
                     strip.text = ggplot2::element_text(size = 8),
                     strip.background = ggplot2::element_rect(colour="black", size = 0.2))
    )
    gout <- ggplot2::ggplot(df.measurements, ggplot2::aes(x = 0, y = value)) +
      ggplot2::geom_boxplot(data = df.measurements,
                            # outlier.shape = NA,
                            ggplot2::aes(x = 0, y = value),
                            alpha = 0,
                            fatten = 1.5,
                            width = 0.75,
                            lwd = 0.3,
                            inherit.aes = FALSE) +
      ggplot2::geom_jitter(data = df.measurements,
                           ggplot2::aes(color = color),
                           position = ggplot2::position_jitter(w = 0.3),
                           size = 2,
                           stroke = 0,
                           alpha = 0.7) +
      ggplot2::stat_summary(fun = mean, geom = "point", shape = 3, size = 3, color = "red") +
      ggplot2::scale_colour_identity() +
      ggplot2::ylab(my_ylab) +
      ggplot2::theme_bw() +
      my_theme
    if(by != "by"){
      gout <- gout +
        ggplot2::facet_grid(. ~ df.measurements[[by]], scales = "free")
    }
    if(inherits(geojson.path, "sf")){
      gout <- gout +
        ggplot2::labs(title = paste0("Distribution of ", measurements.type),
                      subtitle = my_subtitle)
    }
    if(is.character(geojson.path)){
      gout <- gout +
        ggplot2::labs(title = paste0("Distribution of ", measurements.type),
                    subtitle = my_subtitle,
                    caption = paste0("Data source:", DescTools::SplitPath(geojson.path)$fullfilename))
    }
    print("ggplot created")
  }
  if(!interactive){
    return(gout)
  }
  if(interactive){
    if(verbose){print(paste0("interactive plot"))}
    df.measurements$lbl.by <- paste(by, df.measurements[[by]])
    df.measurements$lbl.by <- as.factor(df.measurements$lbl.by)
    # TODO: merge into a single code sequence, only the hovertext is different
    if(stat == "dist"){
      gp <- df.measurements %>%
        plotly::group_by(lbl.by) %>%
        # do(p = plot_ly(., x = ~Sepal.Length, y = ~Sepal.Width, color = ~Species, type = "scatter")) %>%
        plotly::do(p = plotly::plot_ly(.,
                                       y = ~value,
                                       type = "box",
                                       boxpoints = "all",
                                       color = ~lbl.by,
                                       colors = df.colors$color,
                                       jitter = 0.5,
                                       pointpos = -1.8,
                                       hoverinfo = 'text',
                                       hovertext = ~paste0(from, " <-> ", to))
        ) %>%
        plotly::subplot(nrows = 1, shareX = TRUE, shareY = TRUE)
    }
    if(stat == "area"){
      gp <- df.measurements %>%
        plotly::group_by(lbl.by) %>%
        # do(p = plot_ly(., x = ~Sepal.Length, y = ~Sepal.Width, color = ~Species, type = "scatter")) %>%
        plotly::do(p = plotly::plot_ly(.,
                                       y = ~value,
                                       type = "box",
                                       boxpoints = "all",
                                       color = ~lbl.by,
                                       colors = df.colors$color,
                                       jitter = 0.5,
                                       pointpos = -1.8,
                                       hoverinfo = 'text',
                                       hovertext = ~id)
        ) %>%
        plotly::subplot(nrows = 1, shareX = TRUE, shareY = TRUE)
    }
    gp <- gp %>%
      plotly::layout(title = paste0("Distribution of ", measurements.type,
                                    "<br><sup>", my_subtitle, "</sup>"),
                     yaxis = list(title = my_ylab))

  }
  if(interactive & export.plot){
    htmlwidgets::saveWidget(gp, paste0(dirOut, stat.name, ".html"))
    if(verbose){
      print(paste0("the '", stat.name,"' file has been saved into '", dirOut, "'"))
    }
  }
  if(interactive & !export.plot){
    gp
  }
}
#
# geojson_boxplot(stat.name = "caravanserais_area", stat = "area", export.plot = F, dirOut = "C:/Rprojects/eamenaR/results/", fig.width = 5)

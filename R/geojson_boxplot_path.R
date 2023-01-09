#' Create a boxplot or various boxplots of path lengths between different heritage places
#'
#' @name geojson_boxplot_path
#'
#' @description
#'
#' @param plot.name the name of the output boxplot and the name of the saved file (if export.plot is TRUE). By default "box_path".
#' @param geojson.path the path of the GeoJSON file. By default 'caravanserail.geojson'.
#' @param csv.path the path to the CSV where the edges between two heritage places are recorded.
#' @param by the name of the field on which the paths will be grouped. Will create as many boxplots as there is different categories. By default NA (no categories).
#' @param plotly.plot if TRUE, export the plot, if FALSE will only display it.
#' @param export.plot if TRUE, show the plot in Plotly window.
#' @param dirOut the folder where the outputs will be saved. By default: '/results'.If it doesn't exist, it will be created. Only useful is export plot is TRUE.
#' @param verbose if TRUE (by default), print messages.
#'
#' @return
#'
#' @examples
#'
#' # normal plot (show only, one boxplot)
#' geojson_boxplot_path()
#'
#' # normal plot (export, a boxplot for each 'route')
#' geojson_boxplot_path(export.plot = T, by = "route")
#'
#' # Plotly plot
#' geojson_boxplot_path(plotly.plot = T, by = "route")
#'
#' @export
geojson_boxplot_path <- function(plot.name = "box_path",
                                 geojson.path = paste0(system.file(package = "eamenaR"),
                                                       "/extdata/caravanserail.geojson"),
                                 csv.path = paste0(system.file(package = "eamenaR"),
                                                   "/extdata/caravanserail_paths.csv"),
                                 by = NA,
                                 plotly.plot = F,
                                 export.plot = F,
                                 dirOut = paste0(system.file(package = "eamenaR"), "/results/"),
                                 fig.width = 9,
                                 fig.height = 5,
                                 verbose = TRUE){
  if(verbose){print("format the nodes (HPs, GeoJSON) and edges (paths, CSV)")}
  paths <- eamenaR::geojson_format_path(geojson.path, csv.path)
  # facets or not
  if(by %in% colnames(paths)){
    names(paths)[names(paths) == by] <- "by"
  } else {paths$by <- 1}
  # stat distances
  bout <- ggplot2::ggplot(paths, ggplot2::aes(x = 0, y = dist.m)) +
    # ggplot2::facet_grid(. ~ route, scales = "free") +
    ggplot2::geom_boxplot(data = paths,
                          ggplot2::aes(x = 0, y = dist.m),
                          alpha = 0,
                          fatten = 1.5,
                          width = 0.75,
                          lwd = 0.3,
                          inherit.aes = FALSE) +
    ggplot2::geom_jitter(ggplot2::aes(color = by),
                         position = ggplot2::position_jitter(w = 0.3),
                         size = 2,
                         stroke = 0,
                         alpha = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = 10),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 8, angle = 90, vjust = 0, hjust=0.5),
                   axis.ticks.length = ggplot2::unit(2, "pt"),
                   axis.ticks = ggplot2::element_line(colour = "black", size = 0.2),
                   panel.border = ggplot2::element_rect(colour = "black", size = 0.2),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x =  ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(colour = "lightgrey", size = 0.1),
                   panel.spacing = ggplot2::unit(2, "mm"),
                   strip.text = ggplot2::element_text(size = 8),
                   strip.background = ggplot2::element_rect(colour = "black", size = 0.2)) +
    ggplot2::ylab("distance (m)") +
    ggplot2::ggtitle("Distribution of distances between two caravanserails by routes")
  if(!is.na(by)){
    # print(colnames(paths))
    if(verbose){print(paste0("the boxplots will be filtered on the column: '", by,"'"))}
    bout <- bout +
      ggplot2::facet_grid(. ~ by, scales = "free")
  }

    # ggplot2::theme_bw() +
    # ggplot2::theme(legend.position = "none",
    #                plot.title = ggplot2::element_text(size = 10)) +
    # ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
    # ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    # ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8, angle = 90, vjust = 0, hjust=0.5)) +
    # ggplot2::ylab("distance (m)") +
    # ggplot2::theme(axis.ticks.length = unit(2, "pt")) +
    # ggplot2::theme(axis.ticks = ggplot2::element_line(colour = "black", size = 0.2)) +
    # ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", size = 0.2)) +
    # ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
    # ggplot2::theme(panel.grid.minor.x =  ggplot2::element_blank()) +
    # ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "lightgrey", size = 0.1)) +
    # ggplot2::theme(panel.spacing = unit(2, "mm")) +
    # ggplot2::theme(strip.text = ggplot2::element_text(size = 8),
    #                strip.background = ggplot2::element_rect(colour = "black", size = 0.2)) +
    # ggtitle("Distribution of distances between two caravanserails by routes")
  if (export.plot) {
    dir.create(dirOut, showWarnings = FALSE)
    gout <- paste0(dirOut, plot.name, ".png")
    ggplot2::ggsave(filename = gout,
                    plot = bout,
                    height = fig.height,
                    width = fig.width)
    if(verbose){print(paste0(gout, " is exported"))}
  } else {
    print(bout)
    if(verbose){print(paste0("boxplot is plotted"))}
  }
  if(plotly.plot){
    plotly::ggplotly(bout)
  }
}


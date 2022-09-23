#' Create a boxplot with paths distances between different heritage places
#' @name geojson_boxplot_path
#' @description
#'
#' @param plot.name the name of the output boxplot and the name of the saved file (if export.plot is TRUE). By default "box_path".
#' @param geojson.path the path of the GeoJSON file. By default 'caravanserail.geojson'.
#' @param csv.path the path to the CSV where the edges between two heritage places are recorded.
#' @param export.plot if TRUE, export the plot, if FALSE will only display it.
#' @param dirOut the folder where the outputs will be saved. By default: '/results'.
#' If it doesn't exist, it will be created. Only useful is export plot is TRUE.
#'
#' @return
#'
#' @examples
#'
#'
#' @export
geojson_boxplot_path <- function(plot.name = "box_path",
                                 geojson.path = paste0(system.file(package = "eamenaR"), "/extdata/caravanserail.geojson"),
                                 csv.path = paste0(system.file(package = "eamenaR"), "/extdata/caravanserail_paths.csv"),
                                 stamen.zoom = 8,
                                 export.plot = F,
                                 dirOut = paste0(system.file(package = "eamenaR"), "/results/"),
                                 fig.width = 8,
                                 fig.height = 8){
  paths <- eamenaR::geojson_format_path(geojson.path, csv.path)
  # stat distances
  bout <- ggplot2::ggplot(paths, ggplot2::aes(x = 0, y = dist.m)) +
    ggplot2::facet_grid(. ~ route, scales="free") +
    ggplot2::geom_boxplot(data = paths,
                          ggplot2::aes(x = 0, y = dist.m),
                          alpha = 0,
                          fatten = 1.5,
                          width = 0.75,
                          lwd = 0.3,
                          inherit.aes = FALSE) +
    ggplot2::geom_jitter(ggplot2::aes(color = route),
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
    print(paste(gout, "is exported"))
  } else {
    bout
  }
}

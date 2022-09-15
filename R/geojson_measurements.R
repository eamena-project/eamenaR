#' Measurements on a GeoJSON file
#' @name geojson_measurements
#' @description Compute measurements (areas, L x l, etc.) on a GeoJSON file
#'
#' @param stat.name the name of the output file. By default "stat".
#' @param geojson.path the path of the GeoJSON file. By default 'caravanserail.geojson'
#' @param ids the IDs of the resources, by default "EAMENA.ID" (n.b: R fieldname format, without spaces)
#' @param stat the statistic that will be computed. By default 'area'.
#' @param plot.stat if TRUE (by default) will plot the stat as a graphic
#' @param export.stat if TRUE return the stats to be stored in a new variable
#' @param write.stat if TRUE, export the stats in a new file.
#' @param dirOut the folder where the outputs will be saved. By default: '/results'.
#' If it doesn't exist, it will be created. Only useful is write.stat is TRUE
#' @param verbose if TRUE (by default) verbose
#'
#' @return Show or export measurements statistics on the GeoJSOn file
#'
#' @examples
#'
#' geojson_measurements(stat.name = "areas", export.stat = T)
#'
#' @export
geojson_measurements <- function(stat.name = "stat",
                                 geojson.path = paste0(system.file(package = "eamenaR"), "/extdata/caravanserail.geojson"),
                                 ids = "EAMENA.ID",
                                 stat = c("area"),
                                 plot.stat = T,
                                 export.stat = F,
                                 write.stat = F,
                                 dirOut = paste0(system.file(package = "eamenaR"), "/results/"),
                                 fig.width = 8,
                                 fig.height = 8,
                                 verbose = TRUE){
  # geojson.path = paste0(system.file(package = "eamenaR"), "/extdata/caravanserail.geojson") ;
  # stat.name = "stat" ; stat = c("list_ids") ; export.stat = F ;
  # dirOut = paste0(system.file(package = "eamenaR"), "/results/")
  ea.geojson <- sf::st_read(geojson.path)
  field.names <- colnames(ea.geojson)[! colnames(ea.geojson) %in% "geometry"]
  if("area" %in% stat){
    df.measurements <- data.frame(type = ea.geojson[["Dimension.Type"]],
                                  value = ea.geojson[["Measurement.Number"]],
                                  scale = ea.geojson[["Measurement.Unit"]]
    )
    df.measurements <- na.omit(df.measurements)
    df.measurements$value <- as.numeric(df.measurements$value)
    lmeasurements.types <- unique(df.measurements$type)
    for (measurements.type in lmeasurements.types){
      # measurements.type <- "Area"
      if(verbose){print(paste0(" compute '", measurements.type,"' measurements"))}
      df.measure.type <- df.measurements[df.measurements$type == measurements.type, ]
      df.measure.type$idf <- rownames(df.measure.type)
      lmeasurements.scale <- unique(df.measurements$scale)
      if(measurements.type == "Area"){
        gout <- ggplot2::ggplot(df.measure.type, ggplot2::aes(x = 0, y = value)) +
          ggplot2::geom_boxplot(data = df.measure.type,
                                ggplot2::aes(x = 0, y = value),
                                alpha = 0,
                                fatten = 1.5, width = 0.5, lwd = 0.3,
                                inherit.aes = FALSE) +
          ggplot2::geom_jitter(ggplot2::aes(color = "red"),
                               position = ggplot2::position_jitter(seed = 1),
                               size = 3, stroke = 0, alpha = 0.7) +
          ggrepel::geom_text_repel(position = ggplot2::position_jitter(seed = 1),
                                   max.overlaps = Inf,
                                   ggplot2::aes(x = 0, y = value,
                                                label = idf)) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "none",
                         plot.title = ggplot2::element_text(size = 10),
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
                         strip.background = ggplot2::element_rect(colour="black", size = 0.2)) +
          ggplot2::ylab(paste0(measurements.type, " in ", lmeasurements.scale)) +
          # scale_color_identity() +
          ggplot2::ggtitle(paste0("Distribution of ", measurements.type))
        if(write.stat){
          ggplot2::ggsave(filename = paste0(dirOut, stat.name, ".png"),
                          plot = gout,
                          height = fig.height,
                          width = fig.width)
          print(paste0("the '", stat.name,"' file has been saved into '", dirOut, "'"))
        }
        if(plot.stat){
          print(gout)
        }
        if(export.stat){
          return(df.measurements)
        }
      }
    }
  }
}

#' Measurements on GeoJSON file
#'
#' @name geojson_measurements
#'
#' @description Compute measurements (areas, L x l, etc.) on GeoJSON file and output boxplot
#'
#' @param stat.name name of output file. Default "stat".
#' @param geojson.path path of GeoJSON file. Default 'caravanserail.geojson'.
#' @param stat statistic that will be computed. Default 'area'.
#' @param name of field on which paths will be grouped. Example "route". Will create as many plots as there are different categories. Default NA.
#' @param csv.path if parameter is TRUE, will use this CSV file to recover route of the heritage places (HP).
#' @param plot.stat if TRUE (default) will plot stat as a graphic.
#' @param export.stat if TRUE return stats to be stored in new variable.
#' @param export.plot if TRUE, export stats in a new file.
#' @param dirOut folder where outputs will be saved. Default: '/results'. If it doesn't exist, will be created. Only useful is export.plot is TRUE.
#' @param verbose if TRUE (default) verbose.
#'
#' @return Show or export measurements statistics on GeoJSON file
#'
#' @examples
#'
#' # boxplot for all HPs
#' geojson_measurements(stat.name = "areas")
#'
#' # by route and export
#' geojson_measurements(stat.name = "areas", by = "route", export.stat = T)
#'
#' @export
geojson_measurements <- function(stat.name = "stat",
                                 geojson.path = paste0(system.file(package = "eamenaR"),
                                                       "/extdata/caravanserail.geojson"),
                                 stat = c("area"),
                                 by = NA,
                                 csv.path = paste0(system.file(package = "eamenaR"),
                                                   "/extdata/caravanserail_paths.csv"),
                                 plot.stat = TRUE,
                                 export.stat = FALSE,
                                 export.plot = FALSE,
                                 dirOut = paste0(system.file(package = "eamenaR"),
                                                 "/results/"),
                                 fig.width = 8,
                                 fig.height = 8,
                                 verbose = TRUE){
  # geojson.path = paste0(system.file(package = "eamenaR"), "/extdata/caravanserail.geojson") ;
  # stat.name = "stat" ; stat = c("list_ids") ; export.stat = F ;
  # dirOut = paste0(system.file(package = "eamenaR"), "/results/")
  if(!is.na(by)){
    paths <- eamenaR::geojson_format_path(geojson.path, csv.path)
    route.from.id <- paths[ , c("from.id", by)]
    route.to.id <- paths[ , c("to.id", by)]
    names(route.from.id) <- names(route.to.id) <- c("idf", by)
    route.id <- rbind(route.from.id, route.to.id)
    route.id <- route.id[!duplicated(route.id), ]
    route.id <- route.id[with(route.id, order(idf)), ]
  }
  # hp.geojson <- sf::st_read(geojson.path)
  hp.geojson <- geojsonsf::geojson_sf(geojson.path)
  field.names <- colnames(hp.geojson)[! colnames(hp.geojson) %in% "geometry"]
  if("area" %in% stat){
    df.measurements <- data.frame(type = hp.geojson[["Dimension Type"]],
                                  value = hp.geojson[["Measurement Number"]],
                                  scale = hp.geojson[["Measurement Unit"]]
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
      if(!is.na(by)){
        df.measure.type <- merge(df.measure.type, route.id, by = "idf", all.x = T)
      }
      if(measurements.type == "Area"){
        gout <- ggplot2::ggplot(df.measure.type, ggplot2::aes(x = 0, y = value)) +
          ggplot2::geom_boxplot(data = df.measure.type,
                                ggplot2::aes(x = 0, y = value),
                                alpha = 0,
                                fatten = 1.5,
                                width = 0.75,
                                lwd = 0.3,
                                inherit.aes = FALSE) +
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
          ggplot2::ggtitle(paste0("Distribution of ", measurements.type))
        if(!is.na(by)){
          gout <- gout +
            ggplot2::geom_jitter(ggplot2::aes(color = route),
                                 position = ggplot2::position_jitter(w = 0.3),
                                 size = 2,
                                 stroke = 0,
                                 alpha = 0.7) +
            ggplot2::facet_grid(. ~ route, scales = "free")
        } else {
          gout <- gout +
            ggplot2::geom_jitter(ggplot2::aes(color = "red"),
                                 position = ggplot2::position_jitter(w = 0.3),
                                 size = 2,
                                 stroke = 0,
                                 alpha = 0.7)
        }
        if(export.plot){
          ggplot2::ggsave(filename = paste0(dirOut, stat.name, ".png"),
                          plot = gout,
                          height = fig.height,
                          width = fig.width)
          print(paste0("the '", stat.name,"' file has been saved into '", dirOut, "'"))
        }
        if(plot.stat){
          return(gout)
        }
        if(export.stat){
          return(df.measurements)
        }
      }
    }
  }
}

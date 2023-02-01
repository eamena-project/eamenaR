#' Basic statistics on a GeoJSON file like lists or charts
#'
#' @name geojson_stat
#'
#' @description Basic descriptive statistics on a GeoJSON file. This function is used by `geojson_format_path()`.
#'
#' @param stat.name the name of the output file. By default "stat".
#' @param geojson.path the path of the GeoJSON file. By default 'caravanserail.geojson'
#' @param ids the IDs of the resources, by default the eamenaR correspondence of "id", see `ref_ids()`.
#' @param concept.name the name of the field used to store the IDs. By default `hp.id`.
#' @param stat the type of statistic that will be computed. By default 'list_fields' (list the fields). The other options are: "list_ids" list EAMENA IDs ; "stat" to diplay charts like pie chart or histograms, etc.
#' @param field.names the field name on which the statistic will be performed. Only useful if the option `stat` is set to `stats` (`stat = "stats"`).
#' @param chart.type either "pie" for pie chart, or "hist" for histogram. Only useful if the option `stat` is set to `stats` (`stat = "stats"`).
#' @param fig.width,fig.height size of the output chart.
#' @param fig.dev the format of the image: "png" (by default), "jpg", "svg", etc.
#' @param export.stat if TRUE return the stats to be stored in a new variable
#' @param write.stat if TRUE, export the stats in a new file, if FALSE will only display it
#' @param dirOut the folder where the outputs will be saved. By default: '/results'. If it doesn't exist, it will be created. Only useful is write.stat is TRUE.
#' @param verbose if TRUE (by default), print messages.
#'
#' @return Show or export basic statistics on the GeoJSON file
#'
#' @examples
#'
#' geojson_stat(stat.name = "geojson_fields", export.stat = T)
#'
#' # list HP names
#' geojson_stat(stat.name = "geojson_fields", stat = "list_ids")
#'
#' # Pie chart on 'Overall Condition Assessment"
#' geojson_stat(stat.name = "overall_cond",
#'              stat = "stats",
#'              field.names = c("Overall Condition State Type"),
#'              write.stat = T)
#'
#' # Do the same, but export in SVG
#' geojson_stat(stat.name = "overall_cond",
#'              stat = "stats",
#'              field.names = c("Overall Condition State Type"),
#'              fig.dev = "svg",
#'              write.stat = T)
#'
#'# Histogram on 'Disturbance Cause Type'
#'geojson_stat(stat.name = "distrub",
#'             stat = "stats",
#'             chart.type = "hist",
#'             field.names = c("Disturbance Cause Type"),
#'             fig.width = 10,
#'             fig.height = 9,
#'             write.stat = T)
#'
#' @export
geojson_stat <- function(stat.name = "stat",
                         geojson.path = paste0(system.file(package = "eamenaR"),
                                               "/extdata/caravanserail.geojson"),
                         ids = eamenaR::ref_ids("hp.id"),
                         concept.name = "hp.id",
                         stat = c("list_fields"),
                         chart.type = c("pie"),
                         field.names = NA,
                         fig.width = 6,
                         fig.height = 6,
                         fig.dev = "png",
                         export.stat = FALSE,
                         write.stat = FALSE,
                         dirOut = paste0(system.file(package = "eamenaR"),
                                         "/results/"),
                         verbose = TRUE){
  # ea.geojson <- sf::st_read(geojson.path)

  # stat <- c("stat")
  # chart.type <- c("pie")
  # field.names <- c("Overall Condition State Type")

# Histogram



  ea.geojson <- geojsonsf::geojson_sf(geojson.path)
  # remove leading/trailing spaces
  names(ea.geojson) <- trimws(colnames(ea.geojson))
  # if(is.na(ids)){
  #   ids <- eamenaR::ref_ids("id")
  # }
  if("list_fields" %in% stat){
    field.names <- colnames(ea.geojson)[! colnames(ea.geojson) %in% "geometry"]
    if (export.stat) {
      dir.create(dirOut, showWarnings = FALSE)
      tout <- paste0(dirOut, stat.name, "_list_fields.tsv")
      df <- data.frame(src.geojson = DescTools::SplitPath(geojson.path)$filename,
                       field.names = field.names)
      write.table(df, tout, sep = "\t", row.names = F)
      if(verbose){print(paste(tout, "is exported"))}
    } else {
      if(verbose){print(paste("Field list:", "\n"))}
      cat(field.names, sep = "\n")
    }
  }
  if("list_ids" %in% stat){
    df <- data.frame(id = row.names(ea.geojson),
                     ea.ids = ea.geojson[[ids]])
    # rename column
    colnames(df)[2] = concept.name
    if (write.stat) {
      dir.create(dirOut, showWarnings = FALSE)
      tout <- paste0(dirOut, stat.name, "_list_ids.tsv")
      write.table(df, tout, sep = "\t", row.names = F)
      if(verbose){print(paste(tout, "is exported"))}
    }
    if (export.stat) {
      rownames(df) <- df$id
      df$id <- NULL
      return(df)
    }
    if (!export.stat & !write.stat){
      if(verbose){print(paste("Ids list:", "\n"))}
      cat(paste0(df$id, ": ", df[ , concept.name]), sep =", ")
    }
  }
  if("stats" %in% stat){
    # df <- data.frame(id = row.names(ea.geojson),
    #                  ea.ids = ea.geojson[[ids]])

    # blank theme for ggplot
    blank_theme <- ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size=14, face="bold")
      )

    for(chart in chart.type){
      # chart <- "pie"
      if(verbose){print(paste0("Chart '", chart,"'"))}
      if(chart == "pie"){
        for(field.name in field.names){
          df <- as.data.frame(table(ea.geojson[[field.name]]))
          df$Freq.perc <- round((df$Freq/sum(df$Freq))*100, 0)
          names(df)[names(df) == 'Var1'] <- field.name
          gg <- ggplot2::ggplot(df, ggplot2::aes(x = "",
                                                 y = Freq.perc,
                                                 fill = .data[[field.name]])) +
            ggplot2::geom_bar(width = 1, stat = "identity") +
            ggplot2::geom_text(ggplot2::aes(label = paste(Freq.perc, "%")),
                               position = ggplot2::position_stack(vjust = 0.5)) +
            ggplot2::coord_polar("y", start = 0) +
            blank_theme
          if(verbose){print(paste("pie", "created"))}
        }
      }
      if(chart == "hist"){
        for(field.name in field.names){
          # field.name <- "Disturbance Cause Type"
          df <- as.data.frame(table(ea.geojson[[field.name]]))
          # there might have different values for a single field
          aggregated.unique.values <- unique(ea.geojson[[field.name]])
          splitted.values <- stringr::str_split(aggregated.unique.values, ", ")
          type <- unlist(splitted.values)
          # different.values.in.the.field <- Reduce("|", unlist(lapply(splitted.values, length)) > 1)
          df <- as.data.frame(table(type))
          # reorder decreasing
          df <- df[order(df$Freq, decreasing = TRUE), ]
          df[['type']] <- factor(df[['type']], levels = df[['type']])
          names(df)[names(df) == 'type'] <- field.name
          gg <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[field.name]], y = Freq)) +
            ggplot2::geom_bar(stat="identity") +
            blank_theme +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
          if(verbose){print(paste("histogram", "created"))}
        }
      }
    }
    # rename column | what for ??
    colnames(df)[2] = concept.name
    if (write.stat) {
      dir.create(dirOut, showWarnings = FALSE)
      gout <- paste0(dirOut, stat.name, "_", chart, ".", fig.dev)
      ggplot2::ggsave(gout, gg,
                      width = fig.width,
                      height = fig.height)
      if(verbose){print(paste(gout, "has been exported"))}
    }
    if (export.stat) {
      # rownames(df) <- df$id
      # df$id <- NULL
      # return(df)
    }
    if (!export.stat & !write.stat){
      if(verbose){print(paste("Chart:"))}
      gg
    }
  }
}

geojson_stat(stat.name = "distrub",
            stat = "stats",
            chart.type = "hist",
            field.names = c("Disturbance Cause Type"),
            fig.width = 10,
            fig.height = 9,
            write.stat = T)


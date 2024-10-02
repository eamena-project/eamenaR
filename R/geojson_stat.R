# DO NOT UPDATE, see `_geojson_stat*`
#####################################
#' Basic statistics on GeoJSON file like lists or charts
#'
#' @name geojson_stat
#'
#' @description Basic descriptive statistics on GeoJSON file. This function is used by `geojson_format_path()`.
#'
#' @param stat.name name of output file. Default "stat".
#' @param geojson.path path of GeoJSON file. Default 'caravanserail.geojson'
#' @param ids IDs of resources, default eamenaR correspondence of "id", see `ref_ids()`.
#' @param concept.name name of field used to store IDs. Default `hp.id`.
#' @param stat type of statistic that will be computed. Default `list_fields` (list the fields). Other options are: `list_ids` list EAMENA IDs. Use `stat` to diplay charts like pie chart or histograms, etc., see option `chart.type`
#' @param chart.type either "`pie`" for pie chart, or "`hist`" for histogram, "`radar`" for radar diagrams. Only useful if the option `stat` is set to `stats` (`stat = "stats"`).
#' @param field.names field name on which statistic will be performed. Only useful if the option `stat` is set to `stats` (`stat = "stats"`).
#' @param ref.periods the periods reference table.
#' @param name of field on which paths will be grouped. For example "route". Will create as many plots as there are different categories. Default NA.
#' @param verbose if TRUE (by default), print messages.
#'
#' @return Show or export basic statistics on GeoJSON file
#'
#' @examples
#'
#' geojson_stat(stat.name = "geojson_fields", export.stat = T)
#'
#' # list HP names
#' geojson_stat(stat.name = "geojson_fields", stat = "list_ids")
#'
#' # Pie chart on 'Overall Condition Assessment"
#' geojson_stat(stat.name = "overall_condition",
#'              stat = "stats",
#'              field.names = c("Overall Condition State Type"),
#'              export.plot = T)
#'
#' # Do the same, but export in SVG
#' geojson_stat(stat.name = "overall_cond",
#'              stat = "stats",
#'              field.names = c("Overall Condition State Type"),
#'              fig.dev = "svg",
#'              export.plot = T)
#'
#'# Histogram on 'Disturbance Cause Type'
#' geojson_stat(stat.name = "distrub",
#'              stat = "stats",
#'              chart.type = "hist",
#'              field.names = c("Disturbance Cause Type"),
#'              fig.width = 10,
#'              fig.height = 9,
#'              export.plot = T)
#'
#' Radar chart on 'Resource Orientation'
#' geojson_stat(stat.name = "orientations",
#'              stat = "stats",
#'              chart.type = "radar",
#'              field.names = c("Resource Orientation"),
#'              fig.width = 9,
#'              fig.height = 8,
#'              export.plot = T)
#'
#' @export
geojson_stat <- function(stat.name = "stat",
                         geojson.path = paste0(system.file(package = "eamenaR"),
                                               "/extdata/caravanserail.geojson"),
                         csv.path = paste0(system.file(package = "eamenaR"),
                                           "/extdata/caravanserail_paths.csv"),
                         ids = eamenaR::ref_ids("hp.id"),
                         concept.name = "hp.id",
                         stat = c("list_fields"),
                         chart.type = c("pie"),
                         field.names = NA,
                         by = NA,
                         ref.periods = "https://raw.githubusercontent.com/achp-project/cultural-heritage/main/periodo-projects/cultural_periods.tsv",
                         verbose = TRUE){
  # ea.geojson <- sf::st_read(geojson.path)

  # stat <- c("stat")
  # chart.type <- c("pie")
  # field.names <- c("Overall Condition State Type")

  # Histogram
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  # ea.geojson <- sf::read_sf(geojson.path)
  if(inherits(geojson.path, "sf")){
    if(verbose){
      print(paste0("Reads a 'sf' dataframe"))
    }
    ea.geojson <- geojson.path
  }
  if(is.character(geojson.path)){
    if(verbose){
      print(paste0("Reads a path"))
    }
    ea.geojson <- sf::read_sf(geojson.path)
  }
  # ea.geojson <- geojsonsf::geojson_sf(geojson.path)
  # remove leading/trailing spaces
  # names(ea.geojson) <- trimws(colnames(ea.geojson))
  # if(is.na(ids)){
  #   ids <- eamenaR::ref_ids("id")
  # }
  if("list_fields" %in% stat){
    if(verbose){print("list_fields")}
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
    if(verbose){print("list_ids")}
    df <- data.frame(id = row.names(ea.geojson),
                     ea.ids = ea.geojson[[ids]])
    # rename column
    colnames(df)[2] = concept.name
    rownames(df) <- df$id
    df$id <- NULL
    return(df)
    # if (export.plot) {
    #   if(verbose){print("     ..export plot")}
    #   dir.create(dirOut, showWarnings = FALSE)
    #   tout <- paste0(dirOut, stat.name, "_list_ids.tsv")
    #   write.table(df, tout, sep = "\t", row.names = F)
    #   if(verbose){print(paste(tout, "is exported"))}
    # }
    # if (export.stat) {
    #   if(verbose){print("     ..export stats")}
    #   rownames(df) <- df$id
    #   df$id <- NULL
    #   return(df)
    # }
    # if (!export.stat & !export.plot){
    #   if(verbose){print(paste("Ids list:", "\n"))}
    #   cat(paste0(df$id, ": ", df[ , concept.name]), sep =", ")
    # }
  }
  if("stats" %in% stat){
    # df <- data.frame(id = row.names(ea.geojson),
    #                  ea.ids = ea.geojson[[ids]])

    # blank theme for ggplot
    if(verbose){print("stats")}
    blank_theme <- ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 14, face = "bold")
      )
    # loop over the values
    for(chart in chart.type){
      if(chart == "basics"){
        if(verbose){print("basics")}
        # TODO: the field names are not those natives in a GeoJSON export, but the ones of SHP. I highlight them with a XXX. YYY means to add eamenaR:: inf front of the function name when finished
        # bbox
        bbox <- sf::st_bbox(ea.geojson)
        geo.extent <-  list(N = bbox[['ymax']], # NSEW extent
                            S = bbox[['ymin']],
                            E = bbox[['xmin']],
                            W = bbox[['xmax']])
        # geometries
        geom.types <- sf::st_geometry_type(ea.geojson)
        df.geom.types.nb <- table(geom.types)
        # grids
        grids_nb_hp <- as.data.frame(table(ea.geojson$`Grid ID`))
        colnames(grids_nb_hp) <- c("GridID", "n")
        grids_nb_hp <- grids_nb_hp[order(grids_nb_hp$GridID),]
        # admin
        df.country <- table(ea.geojson[["Country Type"]])
        df.admin1 <- table(ea.geojson[["Administrative Division "]]) # extra space (sic)
        # periods XXX
        # TODO: use 'ref_ids'
        df.periods <- as.data.frame(table(ea.geojson[["Cultural Period Type"]]))
        # some HP have several cultural periods, split on ','
        df.periods.simple <- data.frame(period = character(),
                                        nb = numeric())
        for (i in 1:nrow(df.periods)){
          # i <- 2
          periods <- df.periods[i, "Var1"]
          nb <- df.periods[i, "Freq"]
          periods <- unlist(stringr::str_split(periods, ","))
          for (j in 1:length(periods)){
            df.periods.simple[nrow(df.periods.simple) + 1,] = c(periods[j], nb)
          }
        }
        df.periods.simple$nb <- as.numeric(df.periods.simple$nb)
        df.periods.simple$period <- trimws(df.periods.simple$period) # trim spaces
        df.periods <- aggregate(nb ~ period, data = df.periods.simple, sum)
        # order the periods
        df.ordered.periods <- ref_periods() # YYY
        df.ordered.periods <- c(df.ordered.periods, "Unknown")
        # test
        not.in.ref <- setdiff(df.periods$period, df.ordered.periods)
        if (length(not.in.ref) > 0){
          print(paste0("  /!\\ These periods aren't listed in the reference list: '", not.in.ref, "'"))
        }
        df.periods <- df.periods[match(df.ordered.periods, df.periods$period),]
        df.periods <- df.periods[complete.cases(df.periods), ]
        periods <- setNames(as.character(df.periods$nb), df.periods$period)
        # ...
        linfos <- c(nb = nrow(ea.geojson),
                    geo.extent = geo.extent,
                    geo.type = df.geom.types.nb,
                    grids = grids_nb_hp,
                    grid_list = cat(as.character(grids_nb_hp$GridID), sep = ", "),
                    adm.country = df.country,
                    adm.adm1 = df.admin1,
                    periods = periods)
        print(str(linfos))
      }

      if(chart == "boxplot"){
        # chart <- "boxplot"
        if(verbose){print(paste0("Chart '", chart,"'"))}
      }

      if(chart == "radar"){
        # chart <- "radar"
        if(verbose){print(paste0("Chart '", chart,"'"))}
        orientations <- c("North", "Northeast", "East",
                          "Southeast", "South",
                          "Southwest", "West",
                          "Northwest")
        if(is.na(by)){
          hp.orient <- ea.geojson[ , c(ids, field.names)]
        } else {
          # TODO
          paths <- eamenaR::geojson_format_path(geojson.path, csv.path, by = by)
          hp.orient <- ea.geojson[ , c(ids, field.names, by)]
        }
        hp.orient[["Direction"]] <- NA
        for(i in seq(1, nrow(hp.orient))){
          hp.orient[i, "Direction"] <- unlist(stringr::str_split(hp.orient[i, "Resource Orientation"],
                                                                 "-"))[1]
        }
        directions <- hp.orient$Direction
        directions.t <- as.data.frame(table(hp.orient$Direction))
        names(directions.t) <- c("direction", "n")
        df <- data.frame(direction = orientations)
        # join on direction
        df <- plyr::join(df, directions.t)
        df[is.na(df)] <- 0
        max.sum <- max(df$n)
        df$direction <- factor(df$direction, levels = df$direction, ordered = T)
        # reorder on levels
        vvv <- reorder(stringr::str_wrap(df$direction, 5), df$n)
        levels(vvv) <- df$direction
        gg <- ggplot2::ggplot(df) +
          ggplot2::geom_hline(
            ggplot2::aes(yintercept = y),
            data.frame(y = seq(0, max.sum, by = 10)),
            color = "lightgrey"
          ) +
          ggplot2::geom_col(
            ggplot2::aes(
              x = vvv,
              # x = reorder(str_wrap(direction, 5), n),
              y = n,
              fill = n
            ),
            position = "dodge2",
            show.legend = TRUE,
            alpha = .9
          ) +
          # Lollipop shaft for mean gain per region
          ggplot2::geom_segment(
            ggplot2::aes(
              x = vvv,
              # x = reorder(str_wrap(direction, 5), n),
              y = 0,
              xend = vvv,
              # xend = reorder(str_wrap(direction, 5), n),
              yend = max.sum
            ),
            linetype = "dashed",
            color = "gray12"
          ) +
          # to align North up
          ggplot2::coord_polar(start = -.4) +
          ggplot2::theme_bw() +
          ggplot2::xlab(stat.name)
        if(verbose){print(paste("radar", "created"))}
      }

      # chart <- "pie"
      if(chart == "pie"){
        if(verbose){print("pie")}
        for(field.name in field.names){
          # field.name <- "Overall Condition State Type"
          # c("Good", "Fair", "Poor", "Very Bad", "Destroyed")
          df <- as.data.frame(table(ea.geojson[[field.name]]))
          df$Var1 <- replace(df$Var1, df$Var1 == "", "NaN")
          df$Freq.perc <- round((df$Freq/sum(df$Freq))*100, 0)
          names(df)[names(df) == 'Var1'] <- field.name
          if(field.name == "Overall Condition State Type"){
            # reorder
            # df <- df[match(c("Good", "Fair", "Poor", "Very Bad", "Destroyed"), df[[field.name]]),]
            # rownames(df) <- seq(1, nrow(df))
            df[[field.name]] <- factor(df[[field.name]],
                                       levels = c("Good", "Fair", "Poor", "Very Bad", "Destroyed"))
          }
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
        warp.at <- 40
        if(verbose){print("hist")}
        for(field.name in field.names){
          if(verbose){print(paste0("Read field: '", field.name, "'"))}
          # field.name <- "Disturbance Cause Type"
          # field.name <- "Disturbance Cause Category Type"
          # print(ea.geojson)
          df <- as.data.frame(sf::st_drop_geometry(ea.geojson))
          df <- as.data.frame(table(df[[field.name]]))
          # if(verbose){print(ea.geojson[[field.name]])}
          df$Var1 <- stringr::str_wrap(df$Var1, width = warp.at)
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
            ggplot2::geom_bar(stat = "identity", fill = "lightblue") +
            blank_theme +
            # ggplot2::ylab(paste0(field.name, " %")) +
            ggplot2::ylab(paste0(field.name, " %")) +
            ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = warp.at)) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
                           axis.title.y = ggplot2::element_text(angle = 90),
                           plot.margin = ggplot2::margin(0, 0, 1, 1, "cm"))
          #   ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 1, 1, "cm"))
          # axis.title.y = ggplot2::element_blank()
          if(is.character(geojson.path)){
            gg <- gg + ggplot2::labs(title = paste0(field.name),
                                     # subtitle = my_subtitle,
                                     caption = paste0("Data source:",
                                                      DescTools::SplitPath(geojson.path)$fullfilename))
          }
          if(inherits(geojson.path, "sf")){
            gg <- gg + ggplot2::labs(title = paste0(field.name))
          }
          if(verbose){print(paste("histogram", "created"))}
        }
      }
    }
    # rename column | what for ??
    # colnames(df)[2] = concept.name
    if(verbose){
      print(paste("Export chart"))
    }
    return(gg)
  }
}

# geojson_stat(stat.name = "distrub_cause_category",
#              stat = "stats",
#              chart.type = "hist",
#              field.names = c("Disturbance Cause Type"),
#              fig.width = 10,
#              fig.height = 9,
#              export.plot = T,
#              dirOut = "C:/Rprojects/eamenaR/results/")



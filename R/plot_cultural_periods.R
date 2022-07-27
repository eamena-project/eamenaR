#' Plot the duration of EAMENA HP cultural periods attribution in a chart. The cultural periods
#' are recorded by years
#' @name plot_cultural_periods
#' @description Read the 'cultural_periods.tsv' hosted on GitHub to find the tpq and taq dates
#'
#' @param d a hash() object (a Python-like dictionary)
#' @param field the field name where the periods, subperiods, etc. will be read in the a hash() object
#' to plot it. It should be 'periods' or 'subperiods'
#' @param type.plot if "static" create a PNG, if "dynamic" create a HTML widget
#' @param bin.width size of the bins, by default, 50 years
#' @param export.plot if TRUE, export the plot, if FALSE will only display it
#' @param dataDir the folder where the outputs will be saved. By default: '/results'.
#' If it doesn't exist, it will be created.
#' @param seg.size,seg.alpha,fig.width,fig.height parameters for the gglpot output
#'
#' @return A plotly chart if 'dynamic', or a gglpot if 'static', to display or save
#'
#' @examples
#'
#' d_sql <- hash::hash()
#' d_sql <- uuid_from_eamenaid("eamena", "EAMENA-0187363", d_sql, "uuid")
#' d_sql <- list_cultural_periods(db = "eamena", d = d_sql, field = "culturalper", uuid = d_sql[["uuid"]])
#' plot_cultural_periods(d = d_sql, field = "culturalper", export.plot = TRUE)
#'
#' @export
plot_cultural_periods <- function(d = NA,
                                  field = NA,
                                  type.plot = "static",
                                  bin.width = 50,
                                  export.plot = F,
                                  dataDir = paste0(system.file(package = "eamenaR"), "/results/"),
                                  seg.size = 1,
                                  seg.alpha = 0.5,
                                  fig.width = 8,
                                  fig.height = 8){
  # field = "periods" ; d <- d ; export.plot = F ; type.plot = "static" ;  bin.width = 50 ; dataDir = paste0(getwd(), "/results/"
  # field = "subperiods" ; d <- d ; export.plot = F ; type.plot = "static" ;  bin.width = 50 ; dataDir = paste0(getwd(), "/results/")
  df.all <- d[[field]]
  df <- df.all[[field]]
  df["ea.duration.tpq"][df["ea.duration.tpq"] == "Present"] <- format(Sys.Date(), "%Y")
  df$ea.duration.taq <- as.numeric(df$ea.duration.taq)
  df$ea.duration.tpq <- as.numeric(df$ea.duration.tpq)
  df <- df[!is.na(df$ea.duration.taq) & !is.na(df$ea.duration.tpq), ]
  # folder
  dir.create(dataDir, showWarnings = FALSE)

  # nb of HP
  hps <- unique(d[[field]]$period$eamenaid)
  nb.hps <- length(hps)
  # # read the tpq/taq
  if (type.plot == "static") {
    cultper.byeamenaid <- ggplot2::ggplot(df) +
      ggplot2::geom_segment(ggplot2::aes(x = ea.duration.taq, xend = ea.duration.tpq,
                                         y = eamenaid , yend = eamenaid),
                            size = seg.size, alpha = seg.alpha) +
      ggplot2::xlab("ANE") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none",
                     axis.text.y = ggplot2::element_text(size = 6))

    if (export.plot) {
      gout <- paste0(dataDir, "cultural_", field, "_byeamenaid.png")
      ggplot2::ggsave(gout,
                      cultper.byeamenaid,
                      width = fig.width,
                      height = fig.height)
      print(paste(gout, "is exported"))
    } else {
      print(cultper.byeamenaid)
    }

    # histogram
    x <- c()
    for(dur in seq(1, nrow(df))){
      # dur <- 1
      a.duration <- seq(df[dur, "ea.duration.taq"], df[dur, "ea.duration.tpq"], by = bin.width)
      x <- as.numeric(c(x, a.duration))
    }
    cultper.histog <- ggplot2::ggplot() +
      ggplot2::aes(x) +
      ggplot2::geom_histogram(binwidth = bin.width, colour = "black", fill = "white") +
      ggplot2::xlab("ANE") +
      ggplot2::theme_bw()
    if(export.plot){
      gout <- paste0(dataDir, "cultural_", field, "_histog.png")
      ggplot2::ggsave(gout,
                      cultper.histog,
                      width = fig.width,
                      height = fig.height)
      print(paste(gout, "is exported"))
    } else {
      print(cultper.histog)
    }
  }
  # Plotly
  if(type.plot == "dynamic"){
    gplotly <- plotly::plot_ly()
    colors <- RColorBrewer::brewer.pal(nb.hps, "Set2")
    for(hp in seq(1, nb.hps)){
      # hp <- 1
      a.hp <- hps[hp] # get a EAMENA id
      df <- df.all$period[df.all$period$eamenaid == a.hp, ]
      # only useful columns
      df.periods <- df[, c("name.periods", "name.periods.certain")]
      time.table <- merge(df.periods, cultural_periods, by.x = "name.periods", by.y = "ea.name", all.x = TRUE)
      # get unique cultural periods
      time.table <- time.table[!duplicated(time.table), ]
      time.table$ea.duration.taq <- as.numeric(as.character(time.table$ea.duration.taq))
      time.table$ea.duration.tpq <- as.numeric(as.character(time.table$ea.duration.tpq))
      # plot
      for(i in seq(1, nrow(time.table))){
        # thedifferent boxes
        # i <- 1
        # 4 points to create a rectangle
        per <- c(rep(time.table[i, "ea.duration.taq"], 2),
                 rep(time.table[i, "ea.duration.tpq"], 2))
        per <- as.numeric(per)
        lbl <- paste0("<b>", time.table[i, "name.periods"], "</b><br>",
                      time.table[i, "ea.duration.taq"], " to ", time.table[i, "ea.duration.tpq"], " ANE")
        gplotly <- plotly::gplotly %>%
          plotly::add_polygons(x = per,
                               # x=c(per1,per2,per3,per4),
                               # x=c(periodes.df$tpq, periodes.df$tpq, periodes.df$taq, periodes.df$taq),
                               y = c(hp-1, hp, hp, hp-1),
                               color = colors[hp],
                               line = list(width=1)
          ) %>%
          # the name in the rectangle centre
          plotly::add_annotations(x = mean(per),
                                  y = hp - .25,
                                  text = lbl,
                                  font = list(size = 12),
                                  showarrow = FALSE,
                                  inherit = T)
      }
      # the name of the EAMENA HP
      centre.eamena.id <- mean(c(time.table$ea.duration.taq, time.table$ea.duration.tpq))
      gplotly <- plotly::gplotly %>%
        plotly::add_annotations(x = centre.eamena.id,
                                y = hp-.5,
                                text = a.hp,
                                font = list(size = 16),
                                showarrow = FALSE,
                                inherit = T)
    }
    gplotly <- plotly::gplotly %>%
      plotly::layout(yaxis = list(title = paste0("Cultural ", field),
                                  showlegend = F,
                                  legend = list(orientation = "h"),
                                  showgrid = FALSE,
                                  showticklabels = FALSE,
                                  zeroline = FALSE))
    if(export.plot){
      gout <- paste0(dataDir, "cultural_", field, "_period.html")
      htmlwidgets::saveWidget(htmlwidgets::as_widget(gplotly),
                              gout)
    } else {
      plotly::gplotly
    }
  }
}


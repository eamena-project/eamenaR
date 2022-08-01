#' Create an aoristic analysis of cultural heritage threats from a dataset.
#' @name plot_edtf
#' @description Use an XLSX file where cultural threats are listed and dated with EDTF.
#' R doesn't accept spaces in headers, so the spaces are replaced by dots.
#'
#' @param data_file the path to the dataset. By default "disturbances_edtf.xlsx".
#' @param date_column the column of the EDTF data.
#' @param site_column the column of the sites' names or sites' IDs.
#' @param cause_column the column of the threats causes.
#' @param type_column the column of the type of threats.
#' @param edtf_limits the min and max dates of the time analysis. This interval will be crossed with the boundaries of the dataset. Useful when threats are recorded "before" (end date) or "after" (start date) a specific date.
#' @param edtf_span the time unit of analysis: "myd" = years, months and days (default); "my" = years and months; "y" = year.
#' @param edtf_analyse type of analysis. If "all" will sum the different categories of threats, if "category" will plot all categories of threats separately, on a same graphic. By default both: `edtf_analyse = c("all", "category")`
#' @param edtf_analyse_category the field name of the threat category that will be analysed: "Disturbance.Type" (default), or "Disturbance.Cause".
#' @param edft_round round precision for density, default 4 decimal places.
#' @param export.plot if TRUE, saves the plot, if FALSE (default), displays the plot.
#' @param type.plot file extension of the plot to be exported. Either "plotly" for a dynamic HTML plot, or "png" for a static plot.
#' @param id.filter if not NA, will filter on this subset of sites (e.g. `id.filter = c("AM009")`)
#' @param dataOut the path to the folder where the plot file will be created if `export.plot` is TRUE
#' @param file_out the name of the plot that will be created if `export.plot` is TRUE
#'
#' @return One to several plots
#'
#' @examples
#'
#' library(dplyr)
#' plot_edtf(edtf_span = "ym", edtf_analyse = "category")
#'
#' @export
plot_edtf <- function(data_file = paste0(system.file(package = "eamenaR"), "/extdata/disturbances_edtf.xlsx"),
                      date_column = "EDTF",
                      site_column = "S_ID",
                      cause_column = "Disturbance.Cause",
                      type_column = "Disturbance.Type",
                      edft_limits = "2004-01-01..2019-12-31",
                      edtf_span = "myd",
                      edtf_analyse = c("all", "category"),
                      edtf_analyse_category = "Disturbance.Type",
                      edft_round = 4,
                      id.filter = NA,
                      type.plot = c("plotly"),
                      export.plot = FALSE,
                      file_out = "df_out2",
                      dirOut = paste0(system.file(package = "eamenaR"), "/results/")){
  df <- openxlsx::read.xlsx(data_file,
                                  sheet = 1)
  cat_column <- edtf_analyse_category
  if(!is.na(id.filter)){
    df <- df[df$S_ID %in% id.filter, ]
    file_out <- paste0(file_out, "_", paste0(as.character(id.filter), collapse = "_"))
  }
  df.out <- data.frame(
    site = character(),
    date = character(),
    cat = character(),
    density = integer())
  for(i in seq_len(nrow(df))){
    if (i %% 50 == 0){print(message(paste0("read dates ", i, "/", nrow(df))))}
    dates <- messydates::md_intersect(messydates::as_messydate(edft_limits),
                                      messydates::as_messydate(df[i, date_column]))
    if("ymd" %in% edtf_span){
      dates <- dates
    }
    if("ym" %in% edtf_span){
      dates <- unique(format(as.Date(dates), "%Y-%m"))
    }
    if("y" %in% edtf_span){
      dates <- unique(format(as.Date(dates), "%Y"))
    }
    n.dates <- length(dates)
    df.damage <- data.frame(
      site = rep(df[i, site_column], n.dates),
      date = dates,
      cat = rep(df[i, cat_column], n.dates),
      density = 1/n.dates)
    df.out <- rbind(df.out, df.damage)
  }

  if("all" %in% edtf_analyse){
    df.out.general <- df.out %>%
      group_by(date) %>%
      summarise(density = sum(density))
  }
  if("category" %in% edtf_analyse){
    df.out.cat <- df.out %>%
      group_by(date, cat) %>%
      summarise(density = sum(density))
  }
  if("plotly" %in% type.plot){
    if("all" %in% edtf_analyse){
      p <- plotly::plot_ly(df.out.general,
                           type = 'scatter',
                           x = ~date,
                           y = ~round(density, edft_round),
                           mode = 'line')
      if(export.plot){
        dir.create(dirOut, showWarnings = FALSE)
        pout <- paste0(dirOut, file_out, "_threat.html")
        htmlwidgets::saveWidget(plotly::as_widget(p),
                                pout)
        print(paste0("the plotly plot '", pout, "' has been saved to '", dirOut,"'"))
      } else {
        print(p)
      }
    }
    if("category" %in% edtf_analyse){
      # type
      p <- plotly::plot_ly(df.out.cat,
                           type = 'scatter',
                           x = ~date,
                           y = ~round(density, 4),
                           color = ~cat,
                           mode = 'line')
      if(export.plot){
        dir.create(dirOut, showWarnings = FALSE)
        pout <- paste0(dirOut, file_out, "_threats_types.html")
        htmlwidgets::saveWidget(plotly::as_widget(p),
                                pout)
        print(paste0("the plotly plot '", pout, "' has been saved to '", dirOut,"'"))
      } else {
        print(p)
      }
    }
  }
  if("png" %in% type.plot){
    pout <- paste0(dirOut, file_out, ".png")
    png(pout, width = 18, height = 12, res = 300, units = "cm")
    plot(density ~ date, df.out, xaxt = "n", type = "l")
    axis(1, df.out$date, format(df.out$date, "%b %y"), cex.axis = .7)
    dev.off()
    print(paste0("the PNG plot '", pout, "' has been saved to '", dirOut,"'"))
  }
}



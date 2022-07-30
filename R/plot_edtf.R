#' Create an aoristic analysis of threats from a dataset
#' @name plot_edtf
#' @description Use an XLSX file where R doesn't accept spaces in headers, so the spaces are replaced by dots.
#'
#' @param data_file the path to the file where the XLSX is
#' @param date_column the column of the EDTF data
#' @param site_column the column of the site names
#' @param cause_column the column of the Causes
#' @param type_column the column of the Type of threats
#' @param edtf_limits the min and max of the temporal anaylsis. Intersect with the limits of the dataset
#' @param edtf_span the time span of the analysis: "myd" = years, months and days (by default);
#' "my" = years and months, etc.
#' @param edtf_analyse type of analysis. If "all" will sum the different categories,
#' if "category" will plot all categories of threats
#' @param edtf_analyse_category the field name of the threats category that will be analysed.
#' "Disturbance.Type" (by default), or "Disturbance.Cause".
#' @param edft_round round density. By default 4 decimals
#' @param export.plot if TRUE will export, if FALSE (by default) will show
#' @param type.plot file extension of the plot to export. Either "plotly" for an HTML dynamic plot,
#'  or "png" for a static one.
#'
#' @param id.filter if not NA, will filter on this subset of sites (ex: id.filter = c("AM009"))
#' @param wkt_column the column of the WKT coordinates. Useful if the X,Y coordinates are already in
#' @param dataOut the path to the folder where the GeoJSON file will be created.
#' @param geojson.name the name of the GeoJSON that will be created
#'
#' @return a plot
#'
#' @examples
#'
#' library(dplyr)
#' plot_edtf(data_file = "C:/Rprojects/eamena-arches-dev/data/time/Disturbances_EDTF.xlsx")
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
                      file_out = "df_syria_out2",
                      dirOut = paste0(system.file(package = "eamenaR"), "/results/")){
  df_syria <- openxlsx::read.xlsx(data_file,
                                  sheet = 1)
  cat_column <- edtf_analyse_category
  if(!is.na(id.filter)){
    df_syria <- df_syria[df_syria$S_ID %in% id.filter, ]
    file_out <- paste0(file_out, "_", paste0(as.character(id.filter), collapse = "_"))
  }
  df_syria.out <- data.frame(
    site = character(),
    date = character(),
    cat = character(),
    # cause = character(),
    # type = character(),
    density = integer())
  for(i in seq_len(nrow(df_syria))){
    if (i %% 50 == 0){print(message(paste0("read dates ", i, "/", nrow(df_syria))))}
    dates <- messydates::md_intersect(messydates::as_messydate(edft_limits),
                                      messydates::as_messydate(df_syria[i, date_column]))
    if("ymd" %in% edtf_span){
      dates <- dates
    }
    if("ym" %in% edtf_span){
      dates <- unique(format(as.Date(dates), "%Y-%m"))
    }
    n.dates <- length(dates)
    df.damage <- data.frame(
      site = rep(df_syria[i, site_column], n.dates),
      date = dates,
      cat = rep(df_syria[i, cat_column], n.dates),
      # cause = rep(df_syria[i, cause_column], n.dates),
      # type = rep(df_syria[i, type_column], n.dates),
      density = 1/n.dates)
    df_syria.out <- rbind(df_syria.out, df.damage)
  }

  if("all" %in% edtf_analyse){
    df_syria.out.general <- df_syria.out %>%
      group_by(date) %>%
      summarise(density = sum(density))
  }
  if("category" %in% edtf_analyse){
    df_syria.out.cat <- df_syria.out %>%
      group_by(date, cat) %>%
      summarise(density = sum(density))
  }
  if("plotly" %in% type.plot){
    if("all" %in% edtf_analyse){
      # general
      p <- plotly::plot_ly(df_syria.out.general,
                           type = 'scatter',
                           x = ~date,
                           y = ~round(density, edft_round),
                           mode = 'line')
      if(export.plot){
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
      p <- plotly::plot_ly(df_syria.out.cat,
                           type = 'scatter',
                           x = ~date,
                           y = ~round(density, 4),
                           color = ~cat,
                           mode = 'line')
      if(export.plot){
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
    plot(density ~ date, df_syria.out, xaxt = "n", type = "l")
    axis(1, df_syria.out$date, format(df_syria.out$date, "%b %y"), cex.axis = .7)
    dev.off()
    print(paste0("the PNG plot '", pout, "' has been saved to '", dirOut,"'"))
  }
}

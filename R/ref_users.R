#' Statistics about Arches project users
#'
#' @name ref_users
#'
#' @description statistics about EAMENA users (total number, activities, etc.)
#'
#' @param db.con the parameters for the Postgresql EAMENA DB, in a `RPostgres::dbConnect()` format.
#' @param db.name the name of the Arches database, by default `EAMENA`.
#' @param d a hash() object (a Python-like dictionary).
#' @param stat the type of statistic that will be computed. This is also the the hash dictionnary (`d`) field name that will be filled with this statistics, e.g. "total_users", "date_joined", etc., or "all". By default: "all".
#' @param stat.type the type of stat chart, or diagram that will be plotted. Choice: "edtf" for cumulative function, etc. By default "all".
#' @param stat.name the name of the output file. By default "users".
#' @param plot.g if TRUE will create a plot (FALSE by default).
#' @param export.plot.g if TRUE will export the plot (FALSE by default).
#' @param dirOut the folder where the outputs will be saved. By default: '/results'. If it doesn't exist, it will be created. Only useful is export.plot.g is TRUE.
#' @param date.after the date after which the calculation is made. Usefull to limit the analysis. By default, NA.
#' @param date.before the date before which the calculation is made. Usefull to limit the analysis. By default, the current date (`Sys.Date()`)
#' @param fig.width,fig.height dimension of the exported plot in cm.
#' @param verbose if TRUE (by default), print messages
#'
#' @return a hash() object. If plot.g and export.plot.g are set to TRUE will also create and save plots
#'
#' @examples
#'
#' d <- hash::hash()
#' my_con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
#'                                user = 'xxx',
#'                                password = 'xxx',
#'                                dbname = 'eamena',
#'                                host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
#'                                port = 5432)
#' d <- ref_users(db.con = my_con,
#'                d = d,
#'                date.after = "2020-08-01",
#'                plot.g = T,
#'                export.plot.g = T,
#'                fig.width = 14)
#'
#' @export
ref_users <- function(db.con = NA,
                      d = NA,
                      db.name = "EAMENA",
                      stat = c("all"),
                      stat.type = c("all"),
                      stat.name = "users",
                      plot.g = F,
                      export.plot.g = F,
                      dirOut = paste0(system.file(package = "eamenaR"),
                                      "/results/"),
                      date.after = NA,
                      date.before = Sys.Date(),
                      fig.width = 8,
                      fig.height = 8,
                      verbose = TRUE){
  if(verbose){print("*start users' statistics")}
  if("total_users" %in% stat  | "all" %in% stat){
    if(verbose){print(paste0("    - run statistic: '", "total_users", "'"))}
    sqll <- "
      SELECT count(*) FROM auth_user WHERE last_login IS NOT NULL
            "
    d[["total_users"]] <- DBI::dbGetQuery(db.con, sqll)
  }
  if("date_joined" %in% stat | "all" %in% stat){
    if(verbose){print(paste0("    - run statistic: '", "date_joined", "'"))}
    sqll <- "
      SELECT date_joined FROM auth_user WHERE last_login IS NOT NULL
            "
    d[["date_joined"]] <- DBI::dbGetQuery(db.con, sqll)
    users.tot <- nrow(d[["date_joined"]])
    if(plot.g){
      gtit <- paste0("Evolution of the number of users of the", db.name, "database")
      date_joined <- format(d$date_joined, "%Y-%m-%d")
      dates.ymd <- lubridate::ymd(date_joined$date_joined)
      dates.ym <- format(dates.ymd, "%Y-%m") # to character
      if(!is.na(date.after)){
        date.after <- lubridate::ymd(date.after)
        date.after <- format(date.after, "%Y-%m") # to character
        # dates.ym <- dates.ym[dates.ym > date.after] # OK
      }
      #date.before <- lubridate::ymd(date.before)
      if(lubridate::is.Date(date.before)){
        date.before <- format(date.before, "%Y-%m")
      }
      # interval
      dates.ym <- dates.ym[dates.ym > date.after & dates.ym < date.before]
      dates.ym <- lubridate::ym(dates.ym)
      gsubtit <- paste0(length(dates.ym), " users who logged at least once between ",
                        date.after, " and ", date.before, " (total users: ", users.tot, ")")
      if("ecdf" %in% stat | "all" %in% stat.type){
        df <- data.frame(dates = dates.ym)
        dates.date.joined <- ggplot2::ggplot(df, ggplot2::aes(dates)) +
          ggplot2::stat_bin(ggplot2::aes(y = cumsum(..count..)), geom = "step") +
          # ggplot2::ggtitle() +
          # m <- m + theme()
          # ggplot2::xlab() +
          # ggplot2::ylab() +
          ggplot2::scale_x_date(labels = scales::date_format("%Y-%m"), date_breaks = "1 month") +
          ggplot2::theme_bw() +
          ggplot2::labs(x = "date joined",
                        y = "nb of users",
                        title = gtit,
                        subtitle = gsubtit) +
          ggplot2::theme(legend.position = "bottom",
                         axis.text.x = ggplot2::element_text(angle = 90, vjust = .5, hjust = .5))
      }
      if(FALSE == TRUE){
        # TODO: non-cumulative ; store ggplot into list()
        dates.date.joined <- ggplot2::ggplot() +
          ggplot2::geom_histogram(ggplot2::aes(x = dates.ym), binwidth = 5) +
          ggplot2::xlab("date joined") +
          ggplot2::ylab("nb of users") +
          ggplot2::scale_y_log10(breaks = c(1, 2, 3, 4, 5, 10, 20, 30, 50, 100)) +
          ggplot2::scale_x_date(labels = scales::date_format("%Y-%m"), date_breaks = "1 month") +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "bottom",
                         axis.text.x = ggplot2::element_text(angle = 90, vjust = .5, hjust = .5))
      }
      if(export.plot.g){
        dir.create(dirOut, showWarnings = FALSE)
        gout <- paste0(dirOut, stat.name, ".png")
        ggplot2::ggsave(gout,
                        dates.date.joined,
                        width = fig.width,
                        height = fig.height)
        if(verbose){print(paste0("    - plot exported to: '", gout))}
      } else {
        print(dates.date.joined)
      }
    }
  }
  if(verbose){print("*end users' statistics")}
  return(d)
}

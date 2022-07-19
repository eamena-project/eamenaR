#' List the name of all the cultural period of a given HP
#' @name list_culturalper
#' @description With a given concept UUID (v. Reference Data Manager), find all
#' the cultural periods, subperiods, etc., of a given HP
#'
#' @param db the name of the database or dataset, by default 'eamena'.
#' If 'eamena': will connect the Pg database.
#' If 'geojson', will read the GeoJSON file path
#' recorded in the parameter 'geojson.path'
#' @param d a hash() object (a Python-like dictionary)
#' @param field the field name that will be created in the a hash() object
#' @param uuid the UUID of the HP, only useful if db = 'eamena'
#' @param geojson.path the path of the GeoJSON file
#'
#' @return A hash() with listed cultural periods names
#'
#' @examples
#'
#' d_sql <- hash::hash()
#' d_sql <- uuid_from_eamenaid("eamena", "EAMENA-0187363", d_sql, "uuid")
#' d_sql <- list_culturalper("eamena", d_sql, "culturalper", d_sql$uuid)
#'
#' @export
list_culturalper <- function(db = 'eamena', d, field, uuid = NA, geojson.path = NA, raw.GH = "https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/"){
  # TODO: field is useful?
  # d <- d_sql ; uuid <- '12053a2b-9127-47a4-990f-7f5279cd89da'; field <- "culturalper"
  # d <- d_sql ; uuid <- d_sql[["uuid"]]; field <- "culturalper"
  # d <- d_sql ; uuid <- d_sql[["uuid"]]; field <- "culturalper" ; db = 'geojson' ; geojson.path = 'https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/geojson/caravanserail.geojson'
  df.periods.template <- data.frame(eamenaid = character(0),
                                    periods = character(0),
                                    periods.certain = character(0),
                                    name.periods = character(0),
                                    name.periods.certain = character(0)
  )
  df.subperiods.template <- data.frame(eamenaid = character(0),
                                       subperiods = character(0),
                                       subperiods.certain = character(0),
                                       name.subperiods = character(0),
                                       name.subperiods.certain = character(0)
  )
  #length(uuid)
  if(db == "eamena"){
    for (i in seq(1, length(uuid))){
      # i <- 2
      # print(i)
      if(i %% 10 == 0){print(paste("*read:", i, "/", length(uuid)))}
      a_uuid <- uuid[i]
      a_eamenaid <- d$eamenaid[i]
      sqll <- stringr::str_interp("
      SELECT
      '${a_eamenaid}' AS eamenaid,
      tiledata ->> '38cff73b-c77b-11ea-a292-02e7594ce0a0' AS periods,
      tiledata ->> '38cff738-c77b-11ea-a292-02e7594ce0a0' AS periods_certain,
      tiledata ->> '38cff73c-c77b-11ea-a292-02e7594ce0a0' AS subperiods,
      tiledata ->> '38cff73a-c77b-11ea-a292-02e7594ce0a0' AS subperiods_certain
      FROM tiles
      WHERE resourceinstanceid = '${a_uuid}'
                     ")
      con <- my_con(db) # load the Pg connection
      df.part <- DBI::dbGetQuery(con, sqll)
      DBI::dbDisconnect(con)
    }
  }
  if(db == "geojson"){
    eamenaid <- geojson_get_field(geojson.path, "EAMENA.ID")
    periods <- geojson_get_field(geojson.path, "Cultural.Period.Type")
    periods_certain <- geojson_get_field(geojson.path, "Cultural.Period.Certainty")
    subperiods <- geojson_get_field(geojson.path, "Cultural.Sub.period.Type")
    subperiods_certain <- geojson_get_field(geojson.path, "Cultural.Sub.period.Certainty")
    df.part <- data.frame(eamenaid = eamenaid,
                          periods = periods,
                          periods_certain = periods_certain,
                          subperiods = subperiods,
                          subperiods_certain = subperiods_certain)
  }
  # con <- my_con(db) # load the Pg connection
  # df <- dbGetQuery(con, sqll)
  # dbDisconnect(con)
  periods <- df.part[!(is.na(df.part$periods) | df.part$periods == ""), ]
  if(nrow(periods) > 0){
    # df.periods <- data.frame(eamenaid = periods$eamenaid,
    #                          periods = periods$periods,
    #                          periods.certain = periods$periods_certain,
    #                          name.periods = rep(NA, nrow(periods)),
    #                          name.periods.certain = rep(NA, nrow(periods))
    # )
    df.periods <- data.frame(eamenaid = periods$eamenaid,
                             periods = periods$periods,
                             periods.certain = periods$periods_certain
    )
    cultural_periods <- read.table(paste0(raw.GH, "data/time/results/cultural_periods.tsv"),
                                   sep = "\t", header = T)
    df.periods.template <- merge(df.periods, cultural_periods, by.x = "periods", by.y = "ea.name", all.x = TRUE)
    # df.periods <- name_from_uuid(db = db, df = df.periods,
    #                              uuid.in = "periods", field.out = "name.periods")
    # df.periods <- name_from_uuid(db = db, df = df.periods,
    #                              uuid.in = "periods.certain", field.out = "name.periods.certain")
    # df.periods.template <- rbind(df.periods.template, df.periods)
  }
  subperiods <- df.part[!(is.na(df.part$subperiods) | df.part$subperiods == ""), ]
  if(nrow(subperiods) > 0){
    df.subperiods <- data.frame(eamenaid = subperiods$eamenaid,
                                subperiods = subperiods$subperiods,
                                subperiods.certain = subperiods$subperiods_certain
    )
    # df.subperiods <- data.frame(eamenaid = subperiods$eamenaid,
    #                             subperiods = subperiods$subperiods,
    #                             subperiods.certain = subperiods$subperiods_certain,
    #                             name.subperiods = rep(NA, nrow(subperiods)),
    #                             name.subperiods.certain = rep(NA, nrow(subperiods))
    # )
    # df.subperiods <- name_from_uuid(db = db, df = df.subperiods,
    #                                 uuid.in = "subperiods", field.out = "name.subperiods")
    # df.subperiods <- name_from_uuid(db = db, df = df.subperiods,
    #                                 uuid.in = "subperiods.certain", field.out = "name.subperiods.certain")
    # df.subperiods.template <- rbind(df.subperiods.template, df.subperiods)
    cultural_periods <- read.table(paste0(raw.GH, "data/time/results/cultural_periods.tsv"),
                                   sep = "\t", header = T)
    df.subperiods.template <- merge(df.subperiods, cultural_periods, by.x = "subperiods", by.y = "ea.name", all.x = TRUE)
  }
  # clean
  df.periods.template <- df.periods.template[df.periods.template$eamenaid != "NA", ]
  df.periods.template <- df.periods.template[df.periods.template$periods != "Unknown", ]

  df.subperiods.template <- df.subperiods.template[df.subperiods.template$eamenaid != "NA", ]
  df.subperiods.template <- df.subperiods.template[df.subperiods.template$subperiods != "Unknown", ]

  # store in tibble
  ifelse(nrow(df.periods.template) > 0, periods.out <- df.periods.template, periods.out <- NA)
  ifelse(nrow(df.subperiods.template) > 0, subperiods.out <- df.subperiods.template, subperiods.out <- NA)

  df.tibble <- tidyr::tibble(
    period = periods.out,
    # subperiods = subperiods.out
  )
  d[["period"]] <- tidyr::tibble(period = periods.out)# subperiods = subperiods.out
  d[["subperiod"]] <- tidyr::tibble(subperiod = subperiods.out)# subperiods = subperiods.out
  return(d)
}

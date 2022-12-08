#' Statistics about EAMENA Heritage places (spatial distribution)
#'
#' @name ref_hps
#'
#' @description statistics about EAMENA Heritage places
#'
#' @param db.con the parameters for the Postgresql EAMENA DB, in a `RPostgres::dbConnect()` format.
#' @param d a hash() object (a Python-like dictionary).
#' @param stat.name the name of the output file. By default "eamena-hps".
#' @param stat.format the extension of the geographic file (".geojson", ".shp"). GeoJSON by default.
#' @param plot.map if TRUE will plot a map (FALSE by default).
#' @param export.map if TRUE will export the map (FALSE by default).
#' @param dirOut the folder where the outputs will be saved. By default: '/results'. If it doesn't exist, it will be created. Only useful is export.map is TRUE.
#' @param date.after the date after which the calculation is made. Usefull to limit the analysis. By default, NA.
#' @param date.before the date before which the calculation is made. Usefull to limit the analysis. By default, the current date (`Sys.Date()`)
#' @param verbose if TRUE (by default), print messages
#'
#' @return a hash() object. If plot.map and export.map are set to TRUE will also create and save maps
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
#' d <- ref_hps(db.con = my_con,
#'             d = d,
#'             date.after = '2021-12-31',
#'             date.before = '2023-01-01',
#'             stat.name = "eamena-hps-2022",
#'             stat.format = ".shp",
#'             dirOut = 'C:/Rprojects/eamena-arches-dev/data/geojson/',
#'             export.map = TRUE)
#'
#' @export
ref_hps <- function(db.con = NA,
                    d = NA,
                    stat.name = "eamena-hps",
                    stat.format = ".geojson",
                    plot.map = F,
                    export.map = F,
                    dirOut = paste0(system.file(package = "eamenaR"), "/results/"),
                    date.after = NA,
                    date.before = Sys.Date(),
                    verbose = TRUE){
  if(verbose){print("*start HPS' distribution")}
  date.before <- as.character(date.before)
  sqll.body <- "
      SELECT
      ST_X(ST_AsText(ST_Centroid(ST_GeomFromGeoJSON(tiledata -> '5348cf67-c2c5-11ea-9026-02e7594ce0a0' -> 'features' -> 0 -> 'geometry')))) x,
      ST_Y(ST_AsText(ST_Centroid(ST_GeomFromGeoJSON(tiledata -> '5348cf67-c2c5-11ea-9026-02e7594ce0a0' -> 'features' -> 0 -> 'geometry')))) y
      FROM tiles
            "
  # limit on HPs creation dates
  if(!is.na(date.after) & !is.na(date.before)){
    if(verbose){print("   - limit on dates of creation")}
    sqll.cond.creationdate <- stringr::str_interp("
      --WHERE resourceinstanceid::text LIKE '5b147157-271e-4fe9-bf23-8e34e847b75b'
      WHERE resourceinstanceid::text IN (
          SELECT resourceinstanceid::text FROM resource_instances
          WHERE graphid::text LIKE '34cfe98e-c2c0-11ea-9026-02e7594ce0a0'
          AND createdtime::date > '%${date.after}%'::date
          AND createdtime::date < '%${date.before}%'::date
      	-- LIMIT 300
      )
      AND tiledata -> '5348cf67-c2c5-11ea-9026-02e7594ce0a0' IS NOT NULL
                                              ")
    sqll <- paste0(sqll.body, "\n", sqll.cond.creationdate)
  }
  coords <- d[[stat.name]] <- DBI::dbGetQuery(db.con, sqll)
  DBI::dbDisconnect(db.con)
  hps.geojson <- sf::st_as_sf(coords,
                              coords = c("x", "y"),
                              crs = 4326,
                              agr = "constant",
                              na.fail = FALSE)
  if(export.map){
    if(verbose){print("*write file")}
    outFile <- paste0(dirOut, stat.name, stat.format)
    sf::st_write(hps.geojson, outFile, append = FALSE)
  }
  return(d)
}

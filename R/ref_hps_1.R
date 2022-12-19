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
#' @param date.after the date after which the calculation is made. Useful to limit the analysis. By default, NA.
#' @param date.before the date before which the calculation is made. Useful to limit the analysis. By default, the current date (`Sys.Date()`)
#' @param team.name only the HPs from this team. Useful to limit the analysis. By default, NA (all the teams). The possible values to limit the analysis are: "EAMENA Project Staff", "MarEA Project Staff", "موظف في مشروع ""ايمينا""", "Government Authority/Staff", "Volunteer/Independent Researcher", "مؤسسة غير حكومية (مؤسسة أهلية)", "Student/Trainee", "باحث أكاديمي", "Academic Researcher", "Private Sector", "Non-Governmental Organisation (NGO)", "مؤسسة حكومية/ موظف".
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
ref_hps_1 <- function(db.con = NA,
                    d = NA,
                    stat.name = "eamena-hps",
                    stat.format = ".geojson",
                    plot.map = F,
                    export.map = F,
                    dirOut = paste0(system.file(package = "eamenaR"), "/results/"),
                    date.after = NA,
                    date.before = Sys.Date(),
                    team.name = NA,
                    verbose = TRUE){
  if(verbose){print("*start HPS' distribution")}
  date.before <- as.character(date.before)
  sqll <- "
SELECT ids.ri, ids.ei, staff.teamname, coords.x, coords.y, created.cdate  FROM (
	-- EAMENA ID
    SELECT * FROM (
    SELECT
    resourceinstanceid::TEXT AS ri,
	tiledata ->> '34cfe992-c2c0-11ea-9026-02e7594ce0a0'::text as ei
    FROM tiles
    ) AS x
    WHERE ei IS NOT NULL
) AS ids,
(
	-- team
    SELECT * FROM (
	SELECT
	u.resourceinstanceid::TEXT as ri,
	m.value::text as teamname
	FROM tiles u
	JOIN values m ON (u.tiledata -> 'd2e1ab96-cc05-11ea-a292-02e7594ce0a0'::text ->> 0 = m.valueid::text)
    ) AS x
    WHERE teamname IS NOT NULL
) AS staff,
(
	-- coordinates
    SELECT * FROM (
    SELECT
    resourceinstanceid::TEXT AS ri,
    ST_X(ST_AsText(ST_Centroid(ST_GeomFromGeoJSON(tiledata -> '5348cf67-c2c5-11ea-9026-02e7594ce0a0' -> 'features' -> 0 -> 'geometry')))) x,
    ST_Y(ST_AsText(ST_Centroid(ST_GeomFromGeoJSON(tiledata -> '5348cf67-c2c5-11ea-9026-02e7594ce0a0' -> 'features' -> 0 -> 'geometry')))) y
    FROM tiles
    ) AS x
    WHERE x IS NOT NULL AND y IS NOT NULL
) AS coords
            "
  # limit on HPs creation dates
  if(!is.na(date.after) & !is.na(date.before)){
    if(verbose){print("   - limit on dates of creation")}
    sqll.cond <- stringr::str_interp("
          ,
          (
          	-- date of creation
              SELECT
              resourceinstanceid::TEXT AS ri,
          	createdtime::date AS cdate
              FROM resource_instances
          	WHERE createdtime::date > '%${date.after}%'::date
              AND createdtime::date < '%${date.before}%'::date
          ) AS created

          WHERE ids.ri = staff.ri AND ids.ri = coords.ri AND ids.ri = created.ri
                                              ")
    sqll <- paste0(sqll, "\n", sqll.cond)
  }

  # limit on HPs creation dates
  if(is.na(date.after)){
    if(verbose){print("   - limit on dates of creation")}
    sqll.cond <- stringr::str_interp("
        WHERE ids.ri = staff.ri AND ids.ri = coords.ri
                                              ")
    sqll <- paste0(sqll, "\n", sqll.cond)
  }

  # limit on teams
  if(!is.na(team.name)){
    if(verbose){print("   - limit on team names")}
    # TODO: authorise different team names
    sqll.cond <- stringr::str_interp("
        AND staff.teamname LIKE '%${team.name}%'
                                              ")
    sqll <- paste0(sqll, "\n", sqll.cond)
  }
  coords <- d[[stat.name]] <- DBI::dbGetQuery(db.con, sqll)
  DBI::dbDisconnect(db.con)
  hps.geojson <- sf::st_as_sf(coords,
                              coords = c("x", "y"),
                              crs = 4326,
                              agr = "constant",
                              na.fail = FALSE)
  if(export.map){
    if(verbose){print("* write file")}
    outFile <- paste0(dirOut, stat.name, stat.format)
    sf::st_write(hps.geojson, outFile, append = FALSE)
  }
  return(d)
}

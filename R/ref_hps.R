#' Statistics about EAMENA Heritage places (spatial distribution, nb of HP by grids)
#'
#' @name ref_hps
#'
#' @description statistics about EAMENA Heritage places. For example the HPs created in 2022, number of HP by grids.
#'
#' @param db.con the parameters for the Postgresql EAMENA DB, in a `RPostgres::dbConnect()` format.
#' @param d a hash() object (a Python-like dictionary).
#' @param stat the type of statistic that will be computed. Default: "spat" (spatial).
#' @param stat.name the name of the output file. By default "eamena_hps".
#' @param stat.format the extension of the geographic file (".geojson", ".shp"). GeoJSON by default.
#' @param plot.map if TRUE will plot a map (FALSE by default).
#' @param export.data if TRUE will export the map (FALSE by default).
#' @param dirOut the folder where the outputs will be saved. By default: '/results'. If it doesn't exist, it will be created. Only useful is export.data is TRUE.
#' @param date.after the date after which the calculation is made. Useful to limit the analysis. By default, NA.
#' @param date.before the date before which the calculation is made. Useful to limit the analysis. By default, the current date (`Sys.Date()`)
#' @param team.name only the HPs from this team. Useful to limit the analysis. By default, NA (all the teams). For examples, the possible values to limit the analysis for the EAMENA DB are: "EAMENA Project Staff", "MarEA Project Staff", "Government Authority/Staff", "Volunteer/Independent Researcher", "Student/Trainee", "Academic Researcher", "Private Sector", "Non-Governmental Organisation (NGO)", etc.
#' @param verbose if TRUE (by default), print messages.
#'
#' @return a hash() object. If plot.map and export.data are set to TRUE will also create and save maps (SHP or GeoJSON).
#'
#' @examples
#'
#' # Hash dictionary and Postgres connection (change the 'xxx' with usernames and password)
#' d <- hash::hash()
#' my_con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
#'                                user = 'xxx',
#'                                password = 'xxx',
#'                                dbname = 'eamena',
#'                                host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
#'                                port = 5432)
#'
#' # Heritage places created during the year 2022 as a SHP file
#' d <- ref_hps(db.con = my_con,
#'             d = d,
#'             date.after = '2021-12-31',
#'             date.before = '2023-01-01',
#'             stat.name = "eamena_hps_2022",
#'             stat.format = ".shp",
#'             dirOut = 'C:/Rprojects/eamena-arches-dev/data/geojson/',
#'             export.data = TRUE)
#'
#' # Number of HP by grids, export as CSV
#' d <- hash::hash()
#' d <- ref_hps(db.con = my_con,
#'              d = d,
#'              stat.name = "eamena_hps_by_grids",
#'              export.data = TRUE,
#'              dirOut = 'C:/Rprojects/eamena-arches-dev/data/grids/')
#'
#' @export
ref_hps <- function(db.con = NA,
                    d = NA,
                    stat = c("spat"),
                    stat.name = "eamena_hps",
                    stat.format = ".geojson",
                    plot.map = FALSE,
                    export.data = FALSE,
                    dirOut = paste0(system.file(package = "eamenaR"),
                                    "/results/"),
                    date.after = NA,
                    date.before = Sys.Date(),
                    team.name = NA,
                    verbose = TRUE){
  if("spat" %in% stat){
    if(verbose){print("Spatial distributon")}
    # find EAMENA ID UUID
    db.name <- eamenaR::ref_ids("hp.id")
    uuid <- eamenaR::ref_ids(db.name, "db.uuid")
    # find other UUIDs
    Investigator.Role.Type.uuid <- eamenaR::ref_ids(in.value = "Investigator Role Type", choice = "db.uuid")
    Geometric.Place.Expression.uuid <- eamenaR::ref_ids(in.value = "Geometric Place Expression", choice = "db.uuid")
    if(verbose){print("*start HPS' distribution")}
    date.before <- as.character(date.before)
    sqll <- stringr::str_interp("
  SELECT ids.ri, ids.ei, staff.teamname, coords.x, coords.y, created.cdate  FROM (
  	-- EAMENA ID
      SELECT * FROM (
      SELECT
      resourceinstanceid::TEXT AS ri,
  	tiledata ->> '${uuid}'::text as ei
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
	JOIN values m ON (u.tiledata -> '${Investigator.Role.Type.uuid}'::text ->> 0 = m.valueid::text)
    ) AS x
    WHERE teamname IS NOT NULL
) AS staff,
(
	-- coordinates
    SELECT * FROM (
    SELECT
    resourceinstanceid::TEXT AS ri,
    ST_X(ST_AsText(ST_Centroid(ST_GeomFromGeoJSON(tiledata -> '${Geometric.Place.Expression.uuid}' -> 'features' -> 0 -> 'geometry')))) x,
    ST_Y(ST_AsText(ST_Centroid(ST_GeomFromGeoJSON(tiledata -> '${Geometric.Place.Expression.uuid}' -> 'features' -> 0 -> 'geometry')))) y
    FROM tiles
    ) AS x
    WHERE x IS NOT NULL AND y IS NOT NULL
) AS coords
            "
    )
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
    if(export.data){
      if(verbose){print("* write file")}
      outFile <- paste0(dirOut, stat.name, stat.format)
      sf::st_write(hps.geojson, outFile, append = FALSE)
    }
    return(d)
  }
  if("grid" %in% stat){
    if(verbose){print("Number of HP by grids")}
    gridid <- eamenaR::ref_ids("Grid.ID", choice = "db.concept.uuid")
    hpgrid <- eamenaR::ref_ids("hp.grid", choice = "db.concept.uuid")
    sqll <- stringr::str_interp("
SELECT q1.grid_id, q1.nb_hp, q2.grid_num
FROM (
    SELECT COUNT(resourceinstanceid::text) AS nb_hp,
        tiledata -> '${hpgrid}' #>> '{0, resourceId}' AS grid_id
    FROM tiles
    WHERE tiledata ->> '${hpgrid}' IS NOT NULL
    GROUP BY grid_id
) q1
INNER JOIN(
    SELECT resourceinstanceid::text AS grid_id,
        tiledata -> '${gridid}' -> 'en' ->> 'value' AS grid_num
    FROM tiles
    WHERE tiledata -> '${gridid}' IS NOT NULL
) q2
ON q1.grid_id = q2.grid_id;
          "
    )
    d[[stat.name]] <- DBI::dbGetQuery(db.con, sqll)
    if(export.data){
      if(verbose){print("* write file")}
      outFile <- paste0(dirOut, stat.name, ".csv")
      write.csv2(d[[stat.name]], outFile, row.names = F)
    }
  }
}

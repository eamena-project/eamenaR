#' Statistics about EAMENA Heritage places (spatial distribution, nb of HP by grids, etc.)
#'
#' @name ref_hps
#'
#' @description statistics about EAMENA Heritage places. For example the HPs created in 2022, number of HP by grids, type of disturbances, overall condition state type of heritage places. This function is the backend counterpart of `geojson_stat()` for GeoJSON files.
#'
#' @param db.con the parameters for the Postgres EAMENA DB, in a `RPostgres::dbConnect()` format.
#' @param d a hash() object (a Python-like dictionary).
#' @param stat the type of statistic that will be computed. Default: "spat" (spatial).Possible values: "grid" for stats on grids; "hist" for stats on categories having a large number of possible values (ex: Disturbances); "pie" for stats on categories having a small number of possible values (ex: Condition)
#' @param stat.name the name of the output file. By default "eamena_hps".
#' @param stat.format the extension of the geographic file (".geojson", ".shp"). GeoJSON by default.
#' @param plot.map if TRUE will plot a map (FALSE by default).
#' @param export.data if TRUE will export the map (FALSE by default).
#' @param dirOut the folder where the outputs will be saved. By default: '/results'. If it doesn't exist, it will be created. Only useful is export.data is TRUE.
#' @param date.after the date after which the calculation is made. Useful to limit the analysis. By default, NA.
#' @param date.before the date before which the calculation is made. Useful to limit the analysis. By default, the current date (`Sys.Date()`)
#' @param on.date whether to limit on "createdate" (a Postgres inner field) or on the field "Assessment Activity Date" (default)
#' @param max.num The maximum number of values (eg categories) to display, for example in an histogram. Default: NA.
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
#' # Heritage places created during the year 2023 as a GeoJSON file
#' d <- ref_hps(db.con = my_con,
#'              d = d,
#'              date.after = '2022-12-31',
#'              date.before = '2024-01-01',
#'              stat.name = "eamena_hps_2023",
#'              stat.format = ".geojson",
#'              team.name = "EAMENA Project Staff",
#'              dirOut = 'C:/Rprojects/eamena-arches-dev/data/geojson/',
#'              export.data = TRUE)
#'
#' # Number of HP by grids, export as CSV
#' d <- hash::hash()
#' d <- ref_hps(db.con = my_con,
#'              d = d,
#'              stat.name = "eamena_hps_by_grids",
#'              date.before = NA,
#'              export.data = TRUE,
#'              stat.format = ".csv",
#'              dirOut = 'C:/Rprojects/eamena-arches-dev/data/grids/')
#'
#' # histogram on Disturbance Cause Category Type
#' d <- ref_hps(db.con = db.con,
#'              d = d,
#'              stat = "hist",
#'              stat.name = "Disturbance Cause Category Type",
#'              max.num = 20,
#'              export.data = TRUE,
#'              dirOut = "C:/Rprojects/eamenaR/results/",
#'              fig.width = 14,
#'              fig.height = 8)
#'
#' # pie chart on Overall Condition State Type
#' d <- ref_hps(db.con = db.con,
#'              d = d,
#'              stat = "pie",
#'              stat.name = "Overall Condition State Type",
#'              export.data = TRUE,
#'              dirOut = "C:/Rprojects/eamenaR/results/",
#'              fig.width = 12,
#'              fig.height = 6)
#'
#'
#' @export
ref_hps <- function(db.con = NA,
                    d = NA,
                    stat = c("spat"),
                    stat.name = "eamena_hps",
                    stat.format = ".geojson",
                    plot.map = FALSE,
                    export.data = FALSE,
                    fig.width = 6,
                    fig.height = 6,
                    fig.dev = "png",
                    dirOut = paste0(system.file(package = "eamenaR"),
                                    "/results/"),
                    date.after = NA,
                    date.before = Sys.Date(),
                    on.date = "assess.activity.date",
                    max.num = NA,
                    team.name = NA,
                    verbose = TRUE){
  blank_theme <- ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )
  if("spat" %in% stat){
    if(verbose){print("Spatial distributon")}
    # find EAMENA ID UUID
    db.name <- eamenaR::ref_ids("hp.id")
    uuid <- eamenaR::ref_ids(concept.name = db.name,
                             choice = "db.concept.uuid")
    # find other UUIDs
    Investigator.Role.Type.uuid <- eamenaR::ref_ids(concept.name = "Investigator Role Type",
                                                    choice = "db.concept.uuid")
    Geometric.Place.Expression.uuid <- eamenaR::ref_ids(concept.name = "Geometric Place Expression",
                                                        choice = "db.concept.uuid")
    Assessment.Activity.Date.uuid <- eamenaR::ref_ids(concept.name = "Assessment Activity Date",
                                                      choice = "db.concept.uuid")
    if(verbose){print("*start HPS' distribution")}
    date.before <- as.character(date.before)
    sqll <- stringr::str_interp(
      "
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
      if(verbose & on.date == "createdtime"){
        print("      - limit on 'createdtime'")
        sqll.cond <- stringr::str_interp(
          "
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
      if(verbose & on.date == "assess.activity.date"){
        print("      - limit on 'Assessment Activity Date'")
        sqll.cond <- stringr::str_interp("
          ,
          (
          	-- date of creation
          	-- Nb: some errors when using ::date formats, so I used ::text (see commented lines)
              SELECT
              resourceinstanceid::TEXT AS ri,
              tiledata ->> '${Assessment.Activity.Date.uuid}'::text AS cdate
              -- TO_DATE(tiledata ->> '${Assessment.Activity.Date.uuid}'::text, 'YYYYMMDD') AS cdate
          	  -- createdtime::date AS cdate
              FROM tiles
          	  WHERE tiledata ->> '${Assessment.Activity.Date.uuid}'::text  > '%${date.after}%'
              AND tiledata ->> '${Assessment.Activity.Date.uuid}'::text  < '%${date.before}%'
          	  -- WHERE TO_DATE('${Assessment.Activity.Date.uuid}'::text, 'YYYYMMDD')  > '%${date.after}%'::date
              -- AND TO_DATE('${Assessment.Activity.Date.uuid}'::text, 'YYYYMMDD')  < '%${date.before}%'::date
          ) AS created

          WHERE ids.ri = staff.ri AND ids.ri = coords.ri AND ids.ri = created.ri
                                              ")
        sqll <- paste0(sqll, "\n", sqll.cond)
      }
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

    if(verbose){print("SQL =")}
    if(verbose){cat(sqll)}
    # run the SQL
    coords <- d[[stat.name]] <- DBI::dbGetQuery(db.con, sqll)
    coords <- coords[!duplicated(coords[["ri"]]),] # rm duplicated ri (resource id)
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
  # Grids
  if("grid" %in% stat){
    if(verbose){print("Number of HP by grids")}
    gridid <- eamenaR::ref_ids("Grid.ID",
                               choice = "db.concept.uuid")
    hpgrid <- eamenaR::ref_ids("hp.grid",
                               choice = "db.concept.uuid")
    sqll <- stringr::str_interp(
      "
    SELECT q1.nb_hp, q2.grid_num
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
    if(verbose){print("SQL =")}
    if(verbose){cat(sqll)}
    d[[stat.name]] <- DBI::dbGetQuery(db.con, sqll)
    # df <- d[[stat.name]]
    # df <- df[!duplicated(df[["ri"]]),] # rm duplicated ri (resource id)
    if(export.data){
      if(verbose){print("* write file")}
      outFile <- paste0(dirOut, stat.name, ".csv")
      write.csv(d[[stat.name]], outFile, row.names = F)
    }
  }
  if("hist" %in% stat){
    # TODO: generalise the SQL for other categories. Currently it only deals with disturbances (UUID=34cfea68-c2c0-11ea-9026-02e7594ce0a0)
    sqll <- "
    SELECT v.value AS categ_type, COUNT(v.value) AS nb
    FROM tiles t
    JOIN values v ON t.tiledata ->> '34cfea68-c2c0-11ea-9026-02e7594ce0a0'::text IS NOT NULL
    AND v.valueid = (t.tiledata ->> '34cfea68-c2c0-11ea-9026-02e7594ce0a0'::text)::uuid
    GROUP BY v.valueid
    ORDER BY nb DESC
    "
    d[[stat.name]] <- DBI::dbGetQuery(db.con, sqll)
    if(export.data){
      df <- d[[stat.name]]
      warp.at <- 25
      df$categ_type <- stringr::str_wrap(df$categ_type, width = warp.at)
      if(!is.na(max.num)){
        # limit
        df <- head(df, max.num)
      }
      df$categ_type <- factor(df$categ_type, levels = df$categ_type)
      gg <- ggplot2::ggplot(df, ggplot2::aes(x = categ_type, y = nb)) +
        ggplot2::geom_bar(stat = "identity", fill = "lightblue") +
        blank_theme +
        ggplot2::labs(title = paste0(stat.name),
                      caption = paste0("Data source: EAMENA database ",
                                       Sys.Date())
        ) +
        ggplot2::ylab(paste0(stat.name)) +
        ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = warp.at)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
                       axis.title.y = ggplot2::element_text(angle = 90),
                       plot.margin = ggplot2::margin(0, 0, 1, 1, "cm"))
      if (export.data) {
        dir.create(dirOut, showWarnings = FALSE)
        gout <- paste0(dirOut, stat.name, "_", stat, ".", fig.dev)
        ggplot2::ggsave(gout, gg,
                        width = fig.width,
                        height = fig.height)
        if(verbose){print(paste(gout, "has been exported"))}
      }
    }
    return(d)
  }
  if("pie" %in% stat){
    # TODO: generalise the SQL for other categories. Currently it only deals with Overall Condition State Type (UUID=34cfe9f5-c2c0-11ea-9026-02e7594ce0a0)
    sqll <-
      "
      SELECT v.value AS categ_type, COUNT(v.value) AS nb
      FROM tiles t
      JOIN values v ON t.tiledata ->> '34cfe9f5-c2c0-11ea-9026-02e7594ce0a0'::text IS NOT NULL
                   AND v.valueid = (t.tiledata ->> '34cfe9f5-c2c0-11ea-9026-02e7594ce0a0'::text)::uuid
      GROUP BY v.valueid
      ORDER BY nb DESC
      "
    d[[stat.name]] <- DBI::dbGetQuery(db.con, sqll)
    if(export.data){
      df <- d[[stat.name]]
      overall.levels <- c("Good", "Fair", "Poor", "Very Bad", "Destroyed", "Unknown")
      df <- subset(df, categ_type %in% overall.levels) # filter
      df$categ_type <- factor(df$categ_type, levels = overall.levels)
      df$nb <- as.integer(df$nb)
      # colors
      color_ramp <- RColorBrewer::brewer.pal(9, "Set1")
      num_colors <- length(overall.levels) - 1 # avoid Unknown
      selected_colors <- color_ramp[1:num_colors]
      selected_colors <- c(selected_colors, "#d3d3d3")
      # ggplot2::ggplot(df, ggplot2::aes(x = "",
      #                                  y = nb,
      #                                  fill = selected_colors)) +
      gg <- ggplot2::ggplot(df, ggplot2::aes(x = "",
                                             y = nb,
                                             fill = categ_type)) +
        ggplot2::geom_bar(width = 1, stat = "identity") +
        ggplot2::geom_text(ggplot2::aes(label = scales::comma(nb)),
                           position = ggplot2::position_stack(vjust = 0.5)) +
        ggplot2::coord_polar("y", start = 0) +
        ggplot2::scale_fill_manual(values = selected_colors) +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        # ggplot2::labs(fill = stringr::str_wrap(stat.name, width = 15)) +
        ggplot2::labs(fill = stringr::str_wrap(stat.name, width = 15),
                      caption = paste0("Data source: EAMENA database ",
                                       Sys.Date())) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
                       plot.margin = ggplot2::margin(0, 0, 1, 1, "cm")) +
        blank_theme
      dir.create(dirOut, showWarnings = FALSE)
      gout <- paste0(dirOut, stat.name, "_", stat, ".", fig.dev)
      ggplot2::ggsave(gout, gg,
                      width = fig.width,
                      height = fig.height)
      if(verbose){print(paste(gout, "has been exported"))}
    }
    return(d)
  }
}

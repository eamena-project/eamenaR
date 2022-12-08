

d <- hash::hash()
db.con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
                               user = 'postgres',
                               password = 'postgis',
                               dbname = 'eamena',
                               host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
                               port = 5432)

# sqll <- "
#       SELECT resourceinstanceid FROM resource_instances
#       WHERE graphid::text LIKE '34cfe98e-c2c0-11ea-9026-02e7594ce0a0'
#       AND createdtime::date > '2020-12-31'::date
#       AND createdtime::date < '2022-01-01'::date
#       -- LIMIT 30
#             "
# d[["resourceinstanceid"]] <- DBI::dbGetQuery(db.con, sqll)
# resourceinstanceids <- paste0(d[["resourceinstanceid"]][, 1], collapse = "|")
# resourceinstanceids <- "5b147157-271e-4fe9-bf23-8e34e847b75b|aa725fa5-94c7-4fc1-b4b7-5773ad17dffb"
# sqll <- stringr::str_interp("
#       SELECT
#       tiledata ->> '34cfe992-c2c0-11ea-9026-02e7594ce0a0' AS eamenaid,
#       tiledata ->> '5348cf67-c2c5-11ea-9026-02e7594ce0a0' AS geom
#       FROM tiles
#       WHERE resourceinstanceid::text LIKE '5b147157-271e-4fe9-bf23-8e34e847b75b'
#       -- WHERE resourceinstanceid::text SIMILAR TO '%${resourceinstanceids}%'
#       AND (
#         tiledata -> '34cfe992-c2c0-11ea-9026-02e7594ce0a0' IS NOT NULL
#         OR
#         tiledata -> '5348cf67-c2c5-11ea-9026-02e7594ce0a0' IS NOT NULL
#         )
#                         ")

sqll.body <-


# return as WKT
sqll <- stringr::str_interp("
      SELECT
      ST_AsText(ST_Centroid(ST_GeomFromGeoJSON(tiledata -> '5348cf67-c2c5-11ea-9026-02e7594ce0a0' -> 'features' -> 0 -> 'geometry'))) geom
      FROM tiles
      --WHERE resourceinstanceid::text LIKE '5b147157-271e-4fe9-bf23-8e34e847b75b'
      WHERE resourceinstanceid::text IN (
          SELECT resourceinstanceid::text FROM resource_instances
          WHERE graphid::text LIKE '34cfe98e-c2c0-11ea-9026-02e7594ce0a0'
          AND createdtime::date > '2020-12-31'::date
          AND createdtime::date < '2022-01-01'::date
      	-- LIMIT 300
      )
      AND tiledata -> '5348cf67-c2c5-11ea-9026-02e7594ce0a0' IS NOT NULL
                        ")
d[["geom"]] <- DBI::dbGetQuery(db.con, sqll)
View(head(d[["geom"]]))
# eamena.id <- as.character(na.omit(d[["eaid_and_geom"]][ , 1]))
# eamena.geom <- as.character(na.omit(d[["eaid_and_geom"]][ , 2]))
# geom.wkt <- geojsonsf::geojson_wkt(eamena.geom)
# spat <- rgeos::readWKT(unlist(geom.wkt[1, "geometry"]))
# sf::st_as_sf(geom.wkt, wkt = unlist(geom.wkt[1, "geometry"]))

# states <- geojsonio::geojson_read(eamena.geom)
# 5b147157-271e-4fe9-bf23-8e34e847b75b
# aa725fa5-94c7-4fc1-b4b7-5773ad17dffb

#

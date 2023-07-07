library(eamenaR)
library(dplyr)


library(sf)

geojson_grid <- function(geojson.path, rows, cols) {
  if(verbose){print(paste0("Read", geojson.path))}
  polygon <- sf::st_read(geojson.path)
  grid.id <- polygon$Grid.ID
  nb.subgrids <- cols*rows
  subgrids.ids <- paste0(grid.id, '.', seq(1, nb.subgrids))
  if(verbose){print(paste0("  * nb of subgrids:", nb.subgrids,
                           "(", head(subgrids.ids, 2), " ... ", tail(subgrids.ids, 1), ")"))}
  polygon_sf <- sf::st_as_sf(polygon)
  bbox <- sf::st_bbox(polygon_sf)
  cell_width <- (bbox[3] - bbox[1]) / cols
  cell_height <- (bbox[4] - bbox[2]) / rows
  #  of points
  sfc.polygons <- sf::st_make_grid(bbox,
                                   cellsize = c(cell_width, cell_height))
  if(verbose){print(paste0("Subgrids created"))}
  polygons <- sf::st_as_sf(sfc.polygons)
  polygons <- sf::st_cast(polygons, "POLYGON")
  polygons$id <- rev(subgrids.ids)
  # # Intersect the grid polygons with the original polygon
  # intersected_polygons <- sf::st_intersection(polygons, polygon_sf)
  if(export){
    sf::st_write(polygons,
                 "C:/Rprojects/eamenaR/results/_divided_grid.geojson",
                 delete_dsn = TRUE)
    if(verbose){print(paste0("Exported"))}
  } else {
    return(polygons)
  }
}

# geojson_polygon <- '{"type": "Polygon", "coordinates": [[[0, 0], [10, 0], [10, 10], [0, 10], [0, 0]]]}'

geojson_polygon <- "https://raw.githubusercontent.com/eamena-project/eamena-arches-dev/main/data/grids/test/E42N30-42.geojson"

grid <- geojson_grid(geojson.path = paste0(system.file(package = "eamenaR"),
                                           "/extdata/E42N30-42.geojson"),
                     rows = 10,
                     cols = 20)
# # Divide the polygon into a grid of smaller polygons
# grid <- divide_polygon_into_grid(polygon, 70, 140)

# Print the resulting grid
print(grid)

##################

ggsheet <- 'https://docs.google.com/spreadsheets/d/1nXgz98mGOySgc0Q2zIeT1RvHGNl4WRq1Fp9m5qB8g8k/edit#gid=1083097625'
list_mapping_bu(bu.path = "C:/Rprojects/eamena-arches-dev/data/bulk/bu/",
                job = "mk",
                verb = T,
                mapping.file = ggsheet,
                mapping.file.ggsheet = T)

####
geom_bbox(dataDir = "C:/Rprojects/eamena-arches-dev/data/bulk/bu/mk/",
          x_column = "xcoord",
          y_column = "ycoord",
          geojson.name = "grids_bbox_mk_f17.geojson")


######################

library(devtools)

use_testthat()
use_test("geojson_addZ")
use_coverage(type = c("codecov"))

library(covr)
codecov()

######################################

# work with GET() + cookies

http.req <- "https://database.eamena.org/en/api/search/export_results?paging-filter=1&tiles=true&format=geojson&precision=6&total=223&term-filter=%5B%7B%22context%22%3A%22%22%2C%22context_label%22%3A%22Heritage%20Place%20-%20Resource%20Name%22%2C%22id%22%3A1%2C%22text%22%3A%22CVNS-IR%22%2C%22type%22%3A%22term%22%2C%22value%22%3A%22CVNS-IR%22%2C%22inverted%22%3Afalse%7D%5D"
r <- httr::GET(http.req,
               config = list(csrftoken="tzkEIDM1WP1yxoqzIT6ozXZmlJlvQfrhFwT0LrzXEYn25klUpS5iuyN5AsDser2L"))
rr <- httr::content(r)


############################
d <- hash::hash()
my_con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
                               user = 'postgres',
                               password = 'postgis',
                               dbname = 'eamena',
                               host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
                               port = 5432)
ref_users(db.con = my_con,
          d = d,
          date.after = "2022-01-01",
          date.before = "2022-12-01",
          plot.g = T)



stat.name = "orientations_path"
stat.name = "boxplot_by_routes"
geojson.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail.geojson"
csv.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail_paths.csv"
ids = eamenaR::ref_ids("hp.id", ids.path = "C:/Rprojects/eamenaR/inst/extdata/ids.csv")
dirOut = "C:/Rprojects/eamenaR/results/"
stat = "stats"
chart.type = "radar"
field.names = c("Resource Orientation")
by = "route"
write.stat = F

paths <- read.csv(csv.path)
df1 <- paths[ , c("from", "route")]
names(df1) <- c("id", "route")
df2 <- paths[ , c("to", "route")]
names(df2) <- c("id", "route")
df <- rbind(df1, df2)
head(df)
df <- df[!duplicated(df), ]

hp_routes <- ref_routes(geojson.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail.geojson",
                        csv.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail_paths.csv",
                        by = "route")



geojson_stat(stat.name = stat.name,
             geojson.path = geojson.path,
             csv.path = csv.path,
             ids = ids,
             dirOut = dirOut,
             stat = stat,
             chart.type = chart.type,
             field.names = field.names,
             by = by,
             write.stat = write.stat)

library(eamenaR)
library(dplyr)


knitr::kable(read.csv("https://raw.githubusercontent.com/eamena-project/eamenaR/main/results/bu_append_hp_ir.csv"))

# geojson_polygon <- '{"type": "Polygon", "coordinates": [[[0, 0], [10, 0], [10, 10], [0, 10], [0, 0]]]}'

my_con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
                               user = 'postgres',
                               password = 'postgis',
                               dbname = 'eamena',
                               host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
                               port = 5432)

d <- hash::hash()
# "https://raw.githubusercontent.com/eamena-project/eamenaR/main/inst/extdata/information_resources_list.csv"
bu.to.append <- "C:/Users/Thomas Huet/Desktop/BU test/ULVS_INFO_Relations_part1.xlsx"
df <- openxlsx::read.xlsx(bu.to.append)
for(i in seq(1, nrow(head(df)))){
  print(paste0("line: ", i))
  eamenaid.from <- df[i, "RESOURCEID_FROM"]
  eamenaid.to <- df[i, "RESOURCEID_TO"]
  d <- uuid_id(db.con = my_con,
               d = d,
               id = eamenaid.from,
               disconn = FALSE,
               verbose = FALSE)
  print(paste0("   - from: ", eamenaid.from, " <-> ", d$uuid))
  d <- uuid_id(db.con = my_con,
               d = d,
               id = eamenaid.to,
               id.prj.patt = "^INFO",
               rm = "ir",
               disconn = FALSE,
               verbose = FALSE)
  print(paste0("   - to: ", eamenaid.to, " <-> ", d$uuid))
  cat("\n")
}

d <- hash::hash()
uuids <- c()
ct <- 0
for(i in df[ , "RESOURCEID_FROM"]){
  ct <- ct + 1
  if(ct %% 50 == 0){print(ct)}
  # eamenaid.from <- df[i, "RESOURCEID_FROM"]
  d <- uuid_id(db.con = my_con,
               d = d,
               id = i,
               disconn = FALSE,
               verbose = FALSE)
  uuids <- c(uuids, d$uuid)
}
anyNA(uuids)

DBI::dbDisconnect(my_con)

####

geojson_polygon <- "https://raw.githubusercontent.com/eamena-project/eamena-arches-dev/main/data/grids/test/E42N30-42.geojson"

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

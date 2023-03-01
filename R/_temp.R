
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

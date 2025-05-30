% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/_geojson_stat_1.R, R/geojson_imagery.R,
%   R/geojson_stat.R
\name{geojson_stat}
\alias{geojson_stat}
\alias{geojson_imagery}
\title{Basic statistics on GeoJSON file like lists or charts}
\usage{
geojson_stat(
  stat.name = "stat",
  geojson.path = paste0(system.file(package = "eamenaR"),
    "/extdata/caravanserail.geojson"),
  csv.path = paste0(system.file(package = "eamenaR"), "/extdata/caravanserail_paths.csv"),
  ids = eamenaR::ref_ids("hp.id"),
  concept.name = "hp.id",
  stat = c("list_fields"),
  chart.type = c("pie"),
  field.names = NA,
  by = NA,
  ref.periods =
    "https://raw.githubusercontent.com/achp-project/cultural-heritage/main/periodo-projects/cultural_periods.tsv",
  verbose = TRUE
)

geojson_imagery(
  geojson.path = paste0(system.file(package = "eamenaR"),
    "/extdata/caravanserail.geojson"),
  export.plot = FALSE,
  dirOut = paste0(system.file(package = "eamenaR"), "/results/"),
  verbose = TRUE
)

geojson_stat(
  stat.name = "stat",
  geojson.path = paste0(system.file(package = "eamenaR"),
    "/extdata/caravanserail.geojson"),
  csv.path = paste0(system.file(package = "eamenaR"), "/extdata/caravanserail_paths.csv"),
  ids = eamenaR::ref_ids("hp.id"),
  concept.name = "hp.id",
  stat = c("list_fields"),
  chart.type = c("pie"),
  field.names = NA,
  by = NA,
  ref.periods =
    "https://raw.githubusercontent.com/achp-project/cultural-heritage/main/periodo-projects/cultural_periods.tsv",
  verbose = TRUE
)
}
\arguments{
\item{stat.name}{name of output file. Default "stat".}

\item{geojson.path}{path of GeoJSON file. Default 'caravanserail.geojson'}

\item{ids}{IDs of resources, default eamenaR correspondence of "id", see `ref_ids()`.}

\item{concept.name}{name of field used to store IDs. Default `hp.id`.}

\item{stat}{type of statistic that will be computed. Default `list_fields` (list the fields). Other options are: `list_ids` list EAMENA IDs. Use `stat` to diplay charts like pie chart or histograms, etc., see option `chart.type`}

\item{chart.type}{either "`pie`" for pie chart, or "`hist`" for histogram, "`radar`" for radar diagrams. Only useful if the option `stat` is set to `stats` (`stat = "stats"`).}

\item{field.names}{field name on which statistic will be performed. Only useful if the option `stat` is set to `stats` (`stat = "stats"`).}

\item{ref.periods}{the periods reference table.}

\item{verbose}{if TRUE (by default), print messages.}

\item{export.plot}{if TRUE, export tats in new file, if FALSE will only display it}

\item{dirOut}{folder where outputs will be saved. Default: '/results'. If it doesn't exist, will be created. Only useful is export.plot is TRUE.}

\item{color.set}{the RBrewer color set. Default "Set1".}

\item{fig.width, fig.height}{size of output chart.}

\item{fig.dev}{format of image: "png" (default), "jpg", "svg", etc.}

\item{export.stat}{if TRUE return stats to be stored in new variable}

\item{name}{of field on which paths will be grouped. For example "route". Will create as many plots as there are different categories. Default NA.}
}
\value{
Show or export basic statistics on GeoJSON file

Show or export basic statistics on GeoJSON file

Show or export basic statistics on GeoJSON file
}
\description{
Basic descriptive statistics on GeoJSON file. This function is used by `geojson_format_path()` and is the front counterpart of `ref_hps()` for a Postgres query.

Basic descriptive statistics on GeoJSON file. This function is used by `geojson_format_path()` and is the front counterpart of `ref_hps()` for a Postgres query.

Basic descriptive statistics on GeoJSON file. This function is used by `geojson_format_path()`.
}
\examples{

geojson_stat(stat.name = "geojson_fields", export.stat = T)

# list HP names
geojson_stat(stat.name = "geojson_fields", stat = "list_ids")

# Pie chart on 'Overall Condition Assessment"
geojson_stat(stat.name = "overall_condition",
             stat = "stats",
             field.names = c("Overall Condition State Type"),
             export.plot = T)

# Do the same, but export in SVG
geojson_stat(stat.name = "overall_cond",
             stat = "stats",
             field.names = c("Overall Condition State Type"),
             fig.dev = "svg",
             export.plot = T)

# Histogram on 'Disturbance Cause Type'
geojson_stat(stat.name = "distrub",
             stat = "stats",
             chart.type = "hist",
             field.names = c("Disturbance Cause Type"),
             fig.width = 10,
             fig.height = 9,
             export.plot = T)

Radar chart on 'Resource Orientation'
geojson_stat(stat.name = "orientations",
             stat = "stats",
             chart.type = "radar",
             field.names = c("Resource Orientation"),
             fig.width = 9,
             fig.height = 8,
             export.plot = T)

Histogram on 'GE Imagery Acquisition Date' (dates)
geojson_stat(stat.name = "orientations",
             stat = "stats",
             chart.type = "hist",
             field.names = c("GE Imagery Acquisition Date"),
             fig.width = 9,
             fig.height = 8,
             export.plot = F)



# list HP names
geojson_stat(stat.name = "geojson_fields", stat = "list_ids")



geojson_stat(stat.name = "geojson_fields")

# list HP names
geojson_stat(stat.name = "geojson_fields", stat = "list_ids")

# Pie chart on 'Overall Condition Assessment"
geojson_stat(stat.name = "overall_condition",
             stat = "stats",
             field.names = c("Overall Condition State Type"),
             export.plot = T)

# Do the same, but export in SVG
geojson_stat(stat.name = "overall_cond",
             stat = "stats",
             field.names = c("Overall Condition State Type"),
             fig.dev = "svg",
             export.plot = T)

# Histogram on 'Disturbance Cause Type'
geojson_stat(stat.name = "distrub",
             stat = "stats",
             chart.type = "hist",
             field.names = c("Disturbance Cause Type"),
             fig.width = 10,
             fig.height = 9,
             export.plot = T)

Radar chart on 'Resource Orientation'
geojson_stat(stat.name = "orientations",
             stat = "stats",
             chart.type = "radar",
             field.names = c("Resource Orientation"),
             fig.width = 9,
             fig.height = 8,
             export.plot = T)

}

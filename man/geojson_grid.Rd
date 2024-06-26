% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geojson_grid.R
\name{geojson_grid}
\alias{geojson_grid}
\title{Divide a Grid Square into a selected number of subgrids}
\usage{
geojson_grid(
  geojson.path = paste0(system.file(package = "eamenaR"), "/extdata/E42N30-42.geojson"),
  rows = 140,
  cols = 60,
  boustrop = TRUE,
  export = TRUE,
  fileOut = NA,
  dirOut = paste0(system.file(package = "eamenaR"), "/results/"),
  verbose = TRUE
)
}
\arguments{
\item{geojson.path}{path of GeoJSON file. Default 'E42N30-42.geojson'}

\item{rows}{number of cells in the X dimension. By default 140.}

\item{cols}{number of cells in the Y dimension. By default 60.}

\item{boustrop}{if TRUE (Default) will numbered in a boustrophedon way, starting from the upper left subgrid cell.}

\item{export}{if TRUE, export the GeoJSON file, if FALSE will only return it}

\item{fileOut}{name of the exported GeoJSON with extension (ex: "E42N30-42_subgrids.geojson"). Default `NA`, will add "_subgrids" to the input GeoJSON}

\item{dirOut}{folder where outputs will be saved. Default: '/results'. If it doesn't exist, will be created. Only useful is export.plot is TRUE.}

\item{verbose}{if TRUE (by default), print messages.}
}
\value{
A GeoJSON file with the same extent as the input GeoJSON file, but divided in many smaller polygons numbered from 1 to ...
}
\description{
Divide a Grid Square (GS) into a selected number of subgrids (rows, cols). This function is useful for anyone who want to survey systematicaly a GS (~ 24km * 28 km) by dividing this GS in many smaller grids (numbered from GS.1 to GS.n in a boustrophedon order). By default, GS are divided into 140 rows and 60 columns to fit with a Google Earth scale ("distance to your eye") that correspond to 300 m in a way each subgrid is entirely visible on a computer screen
}
\examples{

# Create a grid of 10 rows by 20 columns
geojson_grid(geojson.path = paste0(system.file(package = "eamenaR"),
                                          "/extdata/E42N30-42.geojson"),
             rows = 10,
             cols = 20)
}

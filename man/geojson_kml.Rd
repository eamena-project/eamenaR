% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geojson_kml.R
\name{geojson_kml}
\alias{geojson_kml}
\title{Convert between GeoJSON and KMZ/KML}
\usage{
geojson_kml(
  geom.path = paste0(system.file(package = "eamenaR"), "/extdata/Waypoints.kmz"),
  geom.types = c("POINT", "LINE", "POLYGON"),
  export = T,
  dirOut = paste0(system.file(package = "eamenaR"), "/extdata/"),
  out.name = "Waypoints",
  select.name = "EAMENA ID",
  select.fields = c("EAMENA ID", "Resource Name", "resourceid"),
  verbose = T
)
}
\arguments{
\item{geom.path}{path to KML, KMZ or GeoJSON file.}

\item{geom.types}{types of geometries ("POINT", "LINE", "POLYGON") that will be selected. Default all: `c("POINT", "LINE", "POLYGON")`.}

\item{export}{if TRUE, will export KML/KMZ file in GeoJSON format, or GeoJSON file as a KML, if FALSE simple plot.}

\item{dirOut}{path to folder where KML/KMZ/GeoJSON file will be created.}

\item{out.name}{name of KML/KMZ/GeoJSON that will be created without extension.}

\item{select.name}{for KML export only. Field selected to be KML name of heritage place, default "EAMENA ID".}

\item{select.fields}{for KML export only. KML conversion remove large number of GeoJSON fields. This variable is used to select fields to preserve. Default: c("EAMENA ID","Resource Name", "resourceid").}

\item{verbose}{if TRUE (by default) then display different messages.}
}
\value{
GeoJSON file or KML file, depending on input file.
}
\description{
Convert from KML/KMZ to GeoJSON, or from GeoJSON to KML. Geometries drawn in Google Earth or Google Earth pro can be exported as KML or KMZ (ie, compressed KML). At the same time, geometries drawn in EAMENA can be exported as GeoJSON. For a given HP, this data can be then imported into the EAMENA DB.
}
\examples{

library(dplyr)

# from KMZ to GeoJSON, with default 'Waypoints.kmz' input file
library(dplyr)
geojson_kml(geom.types = "POINT")

# from GeoJSON to KML
geojson_kml(geom.path = "C:/Rprojects/eamenaR/inst/extdata/kites.geojson",
                   dirOut = "C:/Rprojects/eamenaR/inst/extdata/",
                   export = T,
                   out.name = "kites_outKML")

}

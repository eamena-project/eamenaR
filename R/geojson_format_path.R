#' Format data on paths
#' @name geojson_format_path
#' @description Use a dataframe of heritage places, and a file of paths between these heritage places, to format a new dataframe
#'
#' @param geojson.path the path of the GeoJSON file.
#' @param csv.path the path to the CSV where the edges between two heritage places are recorded.
#'
#' @return A dataframe with the appropriate format
#'
#' @examples
#'
#' @export
geojson_format_path <- function(geojson.path = geojson.path.,
                                csv.path = csv.path.){
  df <- eamenaR::geojson_stat(stat = c("list_ids"),
                              geojson.path = geojson.path,
                              export.stat = T)
  df$id <- rownames(df)
  paths <- read.table(csv.path, sep = ",", header = T)
  hp.in.paths <- unique(unique(paths$from), unique(paths$to))
  in.paths.only <- setdiff(df$id, hp.in.paths)
  paths <- paths[!(paths$from %in% in.paths.only | paths$to %in% in.paths.only), ]
  hp.geom.sf <- geojsonsf::geojson_sf(geojson.path)
  paths$path.wkt <- paths$dist.m <- paths$from.id <- paths$to.id <- paths$from.geom <- paths$to.geom <- NA
  for(i in seq(1, nrow(paths))){
    from <- hp.geom.sf[hp.geom.sf$`EAMENA ID` == paths[i, "from"], ]
    to <- hp.geom.sf[hp.geom.sf$`EAMENA ID` == paths[i, "to"], ]
    paths[i, "from.id"] <- df[df$ea.ids == from$`EAMENA ID`, "id"]
    paths[i, "to.id"] <- df[df$ea.ids == to$`EAMENA ID`, "id"]
    paths[i, "from.geom"] <- sf::st_as_text(from$geometry)
    paths[i, "to.geom"] <- sf::st_as_text(to$geometry)
    paths[i, "dist.m"] <- as.numeric(sf::st_distance(from, to))
    paths[i, "path.wkt"] <- sf::st_as_text(sf::st_cast(sf::st_union(from$geometry, to$geometry), "LINESTRING"))
  }
  paths <- paths[ , c("from.id", "from", "to.id", "to", "from.geom", "to.geom", "path.wkt", "dist.m", "route")]
  return(paths)
}

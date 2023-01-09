#' Format data on paths
#'
#' @name geojson_format_path
#'
#' @description Use a dataframe of heritage places (ie, places, vertices), and a file of paths (as edges) between these heritage places, to format a new dataframe. The heritage places (HP) are stored in a GeoJSON file. The paths between these HP are stored in a CSV file. The function cleans the datasets removing existing paths linking two heritage places that don't not exist in the GeoJSON. This new dataframe is used by the functions `geojson_map_path()`, `geojson_boxplot_path()` to model the dataset with network analysis.
#'
#' @param geojson.path the path of the GeoJSON file. By default 'caravanserail.geojson'.
#' @param csv.path the path to the CSV where the edges between two heritage places are recorded. By default 'caravanserail_paths.csv'.
#' @param verbose if TRUE (by default), print messages.
#'
#' @return A dataframe with the appropriate columns: "from.id", "from", "to.id", "to", "from.geom", "to.geom", "path.wkt", "dist.m", "route"
#'
#' @examples
#'
#' @export
geojson_format_path <- function(geojson.path = paste0(system.file(package = "eamenaR"),
                                                      "/extdata/caravanserail.geojson"),
                                csv.path = paste0(system.file(package = "eamenaR"),
                                                  "/extdata/caravanserail_paths.csv"),
                                verbose = TRUE){
  df <- eamenaR::geojson_stat(stat = c("list_ids"),
                              geojson.path = geojson.path,
                              export.stat = T)
  df$id <- rownames(df)
  paths <- read.table(csv.path, sep = ",", header = T)
  hp.in.paths <- unique(unique(paths$from),
                        unique(paths$to))
  in.paths.only <- setdiff(df$id, hp.in.paths)
  paths <- paths[!(paths$from %in% in.paths.only | paths$to %in% in.paths.only), ]
  hp.geom.sf <- geojsonsf::geojson_sf(geojson.path)
  paths$path.wkt <- paths$dist.m <- paths$from.id <- paths$to.id <- paths$from.geom <- paths$to.geom <- NA
  for(i in seq(1, nrow(paths))){
    path.from <- paths[i, "from"]
    path.to <- paths[i, "to"]
    from <- hp.geom.sf[hp.geom.sf[["EAMENA ID"]] == path.from, ]
    to <- hp.geom.sf[hp.geom.sf[["EAMENA ID"]] == path.to, ]
    tryCatch(
      #try to do this
      {
        paths[i, "from.id"] <- df[df$ea.ids == from[["EAMENA ID"]], "id"]
      },
      #if an error occurs, tell me the error
      error = function(e) {
        message(paste0("An Error Occurred: the HP '", path.from,
                       "' listed in the paths doesn't exist in the HPs GeoJSON (maybe)"))
        print(e)
      }
      #if a warning occurs, tell me the warning
      # warning=function(w) {
      #   message('A Warning Occurred')
      #   print(w)
      #   return(NA)
      # }
    )
    # paths[i, "from.id"] <- df[df$ea.ids == from[["EAMENA ID"]], "id"]
    paths[i, "to.id"] <- df[df$ea.ids == to[["EAMENA ID"]], "id"]
    paths[i, "from.geom"] <- sf::st_as_text(from$geometry)
    paths[i, "to.geom"] <- sf::st_as_text(to$geometry)
    paths[i, "dist.m"] <- as.numeric(sf::st_distance(from, to))
    paths[i, "path.wkt"] <- sf::st_as_text(sf::st_cast(sf::st_union(from$geometry, to$geometry), "LINESTRING"))
  }
  paths <- paths[ , c("from.id", "from", "to.id", "to", "from.geom", "to.geom", "path.wkt", "dist.m", "route")]
  return(paths)
}

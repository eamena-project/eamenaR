#' Alignment of heritage place data and heritage place paths data
#'
#' @name geojson_format_path
#'
#' @description Use dataframe of heritage places (ie, places, vertices), and file of paths (as edges) between these heritage places, to format new dataframe. Heritage places (HP) are stored in GeoJSON file. Paths between HP are stored in CSV file. Function cleans the datasets, removing existing paths linking two heritage places that don't not exist in the GeoJSON. New dataframe is used by functions `geojson_map_path()`, `geojson_boxplot_path()` to model dataset with network analysis.
#'
#' @param geojson.path path of GeoJSON file. Default 'caravanserail.geojson'.
#' @param csv.path path to CSV where edges between two heritage places are recorded. Default 'caravanserail_paths.csv'.
#' @param name of field on which paths will be grouped. Example "route". Will create as many plots as there are different categories. Default NA.
#' @param concept.name concept that will be retrieve from the `ids.csv` file. Default "hp.id".
#' @param verbose if TRUE (default), print messages.
#'
#' @return Dataframe with appropriate columns: "from.id", "from", "to.id", "to", "from.geom", "to.geom", "path.wkt", "dist.m", "route"
#'
#' @examples
#'
#' @export
geojson_format_path <- function(geojson.path = paste0(system.file(package = "eamenaR"),
                                                      "/extdata/caravanserail.geojson"),
                                csv.path = paste0(system.file(package = "eamenaR"),
                                                  "/extdata/caravanserail_paths.csv"),
                                by = NA,
                                concept.name = "hp.id",
                                verbose = TRUE){
  r.id <- eamenaR::ref_ids(concept.name)
  df <- eamenaR::geojson_stat(stat = c("list_ids"),
                              geojson.path = geojson.path,
                              concept.name = concept.name,
                              export.stat = T)
  df$id <- rownames(df)
  paths <- read.table(csv.path, sep = ",", header = T)
  hp.in.paths <- unique(unique(paths$from),
                        unique(paths$to))
  in.paths.only <- setdiff(hp.in.paths, df$hp.id)
  # clean df with existing HP having Zs and existing in path
  paths <- paths[!(paths$from %in% in.paths.only | paths$to %in% in.paths.only), ]
  hp.geom.sf <- geojsonsf::geojson_sf(geojson.path)
  paths$path.wkt <- paths$dist.m <- paths$from.id <- paths$to.id <- paths$from.geom <- paths$to.geom <- NA
  if(verbose){print(paste0("Read the path file and collect geometries from the heritage places dataframe"))}
  for(i in seq(1, nrow(paths))){
    # i <- 1
    if(verbose){print(paste0(" * read line ", i, "/", nrow(paths)))}
    path.from <- paths[i, "from"]
    path.to <- paths[i, "to"]
    # from <- hp.geom.sf[hp.geom.sf[["EAMENA ID"]] == path.from, ]
    # to <- hp.geom.sf[hp.geom.sf[["EAMENA ID"]] == path.to, ]
    from <- hp.geom.sf[hp.geom.sf[[r.id]] == path.from, ]
    to <- hp.geom.sf[hp.geom.sf[[r.id]] == path.to, ]
    # an HP could have more than one geometry, we select the first one having a POINT format if exists. If not, no point geometry exist, we create it (st_centroid)
    point.geometry <- c("POINT", "MULTIPOINT")
    for (a.hp in c("from", "to")){
      # a.hp <- "to"
      if(a.hp == "from"){fromto.hp <- from}
      if(a.hp == "to"){fromto.hp <- to}
      hp.name <- fromto.hp[[r.id]][1]
      hp.nb.geom <- length(fromto.hp[[r.id]])
      # exist a POINT geometry
      if(any(sf::st_geometry_type(fromto.hp) == point.geometry)){
        if(verbose){print(paste0("  - ", hp.name, " (", a.hp,") has a POINT or MULTIPOINT geometry"))}
        geom.is.point <- match(TRUE, sf::st_geometry_type(fromto.hp) == point.geometry)
        # only the first one
        geom.is.point <- geom.is.point[1]
        fromto.hp <- fromto.hp[geom.is.point, ]
      }
      # doesn't exist a POINT geometry
      if(!any(sf::st_geometry_type(fromto.hp) == point.geometry)){
        if(verbose){print(paste0("  - ", hp.name, " (", a.hp,") doesn't have a POINT or MULTIPOINT geometry"))}
        # centroid to get a point of the first geometry
        fromto.hp <- sf::st_centroid(fromto.hp[1, ])
        if(verbose){print(paste0("    ... geometry conversion (centroid) done"))}
      }
      # only one gemetry in "from" and "to"
      if(a.hp == "from"){from <- fromto.hp}
      if(a.hp == "to"){to <- fromto.hp}
    }

    # correspondences checks
    flag <- T
    df.ids <- unique(df[, concept.name])
    if(from[[r.id]] %in% df.ids){
      paths[i, "from.id"] <- rownames(from)
    } else {
      warning(paste0("An Error Occurred: the HP '", path.from,
                     "' listed in the paths doesn't exist in the HPs GeoJSON (maybe)")
      )
      flag <- F
    }
    if(to[[r.id]] %in% df.ids){
      paths[i, "to.id"] <- rownames(to)
    } else {
      warning(paste0("An Error Occurred: the HP '", path.to,
                     "' listed in the paths doesn't exist in the HPs GeoJSON (maybe)")
      )
      flag <- F
    }
    # paths[i, "to.id"] <- df[df$ea.ids == to[["EAMENA ID"]], "id"]
    # paths[i, "to.id"] <- df[df[[concept.name]] == to[[r.id]], "id"]
    if(flag){
      paths[i, "from.geom"] <- sf::st_as_text(from$geometry)
      paths[i, "to.geom"] <- sf::st_as_text(to$geometry)
      paths[i, "dist.m"] <- as.numeric(sf::st_distance(from, to))
      paths[i, "path.wkt"] <- sf::st_as_text(sf::st_cast(sf::st_union(from$geometry, to$geometry), "LINESTRING"))
    }
  }
  if(verbose){print(paste0(".. done"))}
  print(colnames(paths))
  if(!is.na(by)){
    if(verbose){print(paste0("will stratify on the 'by' column values"))}
    # a reminder: by is the column to stratify data. If the users doesn't have a column by, the latter is instancied to by = 1 for facets. So if the column by as the same valeu as the by variable, value by ...
    paths <- paths[ , c("from.id", "from", "to.id", "to",
                        "from.geom", "to.geom", "path.wkt", "dist.m",
                        by)]
    if(verbose){
      print(paste0("the count of heritage places by '", by, "' is:"))
      print(table(paths[[by]]))
    }
  } else {
    if(verbose){print(paste0("will NOT be stratify on the 'by' column values"))}
    paths <- paths[ , c("from.id", "from", "to.id", "to",
                        "from.geom", "to.geom", "path.wkt", "dist.m")]
    if(verbose){
      print(paste0("the count of heritage places is:"))
      print(nrow(paths))
    }
  }
  n.before.na.rm <- nrow(paths)
  # rm rows having NA values
  paths <- paths[complete.cases(paths), ]
  n.after.na.rm <- nrow(paths)
  if(verbose){
    print(paste0(n.before.na.rm - n.after.na.rm, " row(s) having NA value have been removed"))
  }
  return(paths)
}

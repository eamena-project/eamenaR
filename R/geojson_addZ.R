#' Add a Z value to geometries
#'
#' @name geojson_addZ
#'
#' @description Use a geoserver API to recover the Z for a given set of points stored in a GeoJSON file. The geoserver API can lead to a Timeout.
#'
#' @param geojson.path the path of the GeoJSON file. By default 'caravanserail.geojson'.
#' @param geojson.out the name of the output file. By default NA, will add the suffix 'Z' to the the name of the GeoJSON file (variable `geojson.path`).
#' @param elevation.api the geoserver API that will be used to collect the elevation, by default 'open-elevation' (https://api.open-elevation.com/). Another option is 'gmrt' (https://www.gmrt.org).
#' @param timeout the threshold below which the expectation of a response is accepted, in seconds. By default, 30.
#' @param sleep the time delay between two API request in seconds. By default 0.3.
#' @param dirOut the folder where the outputs will be saved. By default: '/results'. If it doesn't exist, it will be created. Only useful is export plot is TRUE.
#' @param verbose if TRUE (by default): verbose.
#'
#' @return
#'
#' @examples
#'
#' df <- geojson_addZ(geojson.path = "C:/Rprojects/eamenaR/inst/extdata/caravanserail.geojson",
#'                    dirOut = "C:/Rprojects/eamenaR/inst/extdata/")
#'
#'
#' @export
geojson_addZ <- function(geojson.path = paste0(system.file(package = "eamenaR"),
                                               "/extdata/caravanserail.geojson"),
                         geojson.out = NA,
                         elevation.api = "open-elevation",
                         timeout = 30,
                         sleep = .3,
                         dirOut = paste0(system.file(package = "eamenaR"),
                                         "/results/"),
                         verbose = T){
  if(verbose){print(paste0("* get Z from ", elevation.api, " elevation API"))}
  ea.geojson <- geojsonsf::geojson_sf(geojson.path)
  # ea.geojson <- sf::st_read(geojson.path)
  # ea.geojson.src <- jsonlite::fromJSON(geojson.path)
  # ea.geojson.src[[1]][2]$properties$Z <- NA
  my.geom <- ea.geojson
  my.geom$Z <- NA
  n.row <- nrow(my.geom)
  if(verbose){print(paste0("    read ", n.row, " geometries"))}
  df.profile <- data.frame(x = double(),
                           y = double(),
                           z = double())
  Zs <- c()
  for (i in 1:n.row){
    # i <- 7
    # hp.is <- my.geom$hp[i]
    # bc.is <- my.geom$bc[i]
    if(verbose){
      if(i == 1 | i%%1 == 0){
        print(paste0("     - read ", i, "/", n.row))
      }
    }
    a.geom <- my.geom[i, ]
    # type of geometries
    if(sf::st_geometry_type(a.geom$geometry) %in% c("MULTIPOINT", "POLYGON", "MULTIPOLYGON")){
      # centroids
      coordinates <- sf::st_coordinates(sf::st_centroid(a.geom))
      # coordinates <- jsonlite::toJSON(sf::st_coordinates(sf::st_centroid(a.geom)))
    }
    if(sf::st_geometry_type(a.geom$geometry) == "POINT"){
      coordinates <- sf::st_coordinates(a.geom)
      # coordinates <- jsonlite::toJSON(sf::st_coordinates(a.geom))
    }
    X <- coordinates[1]
    Y <- coordinates[2]
    # elevation API
    if (elevation.api == 'gmrt'){
      http.req <- paste0("https://www.gmrt.org/services/ProfileServer?boundspath=",
                         coordinates,
                         "&format=json")
    }
    if (elevation.api == 'open-elevation'){
      http.req <- paste0(#"https://api.open-elevation.com/api/v1/lookup?locations=41.161758,-8.583933",
        "https://api.open-elevation.com/api/v1/lookup?locations=", X, ",", Y)
    }
    r <- httr::GET(http.req, httr::timeout(timeout))
    rr <- httr::content(r)
    if (elevation.api == 'gmrt'){
      rrr <- xml2::as_list(rr)
      rrrr <- rrr$html$body$p[[1]]
      rrrrr <- unlist(stringr::str_split(string = rrrr, pattern = "\\],\\["))
      rrrrrr <- gsub("\\]", "", rrrrr)
      rrrrrrr <- gsub("\\[", "", rrrrrr)
      val <- unlist(stringr::str_split(rrrrrrr[1], pattern = ","))
      df.profile[nrow(df.profile) + 1, ] <- val[1:3]
    }
    if (elevation.api == 'open-elevation'){
      Z <- rr$results[[1]]$elevation
      # ea.geojson.src[[1]][2]$properties$Z[i] <- Z
      # print(Z)
      Zs <- c(Zs, Z)
      # my.geom$Z[[i]] <- Z
      # print("UP")
      # df.profile[nrow(df.profile) + 1, ] <- c(X, Y, Z)
      # jsonlite::toJSON(ea.geojson.src, )
    }
    Sys.sleep(sleep)
  }
  if(verbose){
    print(paste0("  A total of '", length(Zs), "' Z have been calculated:"))
    print(head(Zs))
  }
  # print(Zs)
  my.geom$Z <- Zs
  if(is.na(geojson.out)){
    fileOutName <- paste0(DescTools::SplitPath(geojson.path)$filename,
                          "Z.geojson")
  } else {
    fileOutName <- geojson.out
  }
  fileOut <- paste0(dirOut, fileOutName)
  sf::st_write(my.geom, fileOut, delete_dsn = TRUE)
  if(verbose){print(paste0("* '", fileOutName, "'has been exported to '", dirOut, "'"))}
  return(df.profile)
}

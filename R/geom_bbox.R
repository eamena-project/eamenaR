#' Create bounding box of bulk uploads (BU) geometries and save it as GeoJSON file
#'
#' @name geom_bbox
#'
#' @description Read one or various XLSX worksheets within folder. Collect the xmin, xmax, ymin, xmax in coordinates and creates minimum bounding box (MBR), that is, extent of geometries. Export is GeoJSON file that can be copy/paste in the Map filter of EAMENA database to recover selected grid cells(\link[eamenaR]{list_mapping_bu.R}). Can also be used to get precise Stamen basemap extent (ggmap), etc.
#'
#' @param dataDir path to folder where XLSXs are.
#' @param x_column,y_column column of X and Y coordinates, if these arguments are set to 'NA', the function will read the field 'wkt'.
#' @param wkt_column column of WKT coordinates, if NA (default) function will read the field 'x_column' and 'y_column'.
#' @param dirOut folder where GeoJSON will be saved. Default: '/results'. If it doesn't exist, will be created.
#' @param geojson.name name of the GeoJSON that will be created, Default 'mbr' (minimum bound rectangle)
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return bounding box of the dataset as a GeoJSON file
#'
#' @examples
#'
#' geom_bbox(dataDir = "C:/Rprojects/eamena-arches-dev/data/bulk/bu/mk2")
#'
#' geom_bbox(dataDir = "C:/Rprojects/eamena-arches-dev/data/bulk/bu/mk2/",
#'           x_column = "Longitude.",
#'           y_column = "Latitude",
#'           geojson.name = "grids_bbox_mk2.geojson")
#'
#' @export
geom_bbox <- function(dataDir = NA,
                      x_column = NA,
                      y_column = NA,
                      wkt_column = NA,
                      dirOut = paste0(system.file(package = "eamenaR"),
                                      "/results/"),
                      geojson.name = "mbr.geojson",
                      verbose = TRUE){
  # options(java.parameters = "-Xmx1000m")
  # l.bus <- list.files(dataDir, full.names = T)
  l.bus <- setdiff(list.files(dataDir),
                   list.dirs(dataDir,
                             recursive = FALSE, full.names = FALSE))
  l.bus <- l.bus[grep(".xlsx", l.bus)]
  if(verbose){print(paste0("* work with the job/folder: ", dataDir))}
  if(verbose){print(paste0("  - there are/is '", length(l.bus),"' different XLSX to read"))}
  if(length(l.bus) == 0){
    stop("There is no XLSX file(s) in the folder")
  }
  l.bus.ext <- DescTools::SplitPath(l.bus)$extension
  # l.bus <- l.bus[l.bus.ext[] == "geojson"]
  wktcoord <- xcoord <- ycoord <- c()
  for(bu.name in l.bus){
    # bu.name <- "AAA_f10_text.xlsx"; bu.name <- "37.xlsx" ; bu.name <- "40_51 Kenawi_modif_out_headers.xlsx" ; bu.name <- "AAA-f-5_Kenawi.xlsx"
    bu.path <- paste0(dataDir, bu.name)
    if(verbose){print(paste0("* read: ", bu.path))}
    df <- xlsx::read.xlsx(bu.path, sheetIndex = 1)
    if(verbose){print(paste0("     nb of rows: ", nrow(df)))}
    if(!is.na(wkt_column)){
      if(!(wkt_column %in% colnames(df))){
        stop(paste0("The WKT column '", wkt_column,"' doesn't exist in the XLSX file"))
      }
      wktcoord <- c(wktcoord, df[ , wkt_column])
    }
    if(is.na(wkt_column)){
      xcoord <- c(xcoord, df[ , x_column])
      ycoord <- c(ycoord, df[ , y_column])
    }
  }
  # pt1 <- sf::st_point(c(min(xcoord, na.rm = T), min(ycoord, na.rm = T)))
  # pt2 <- sf::st_point(c(max(xcoord, na.rm = T), min(ycoord, na.rm = T)))
  # pt3 <- sf::st_point(c(max(xcoord, na.rm = T), max(ycoord, na.rm = T)))
  # pt4 <- sf::st_point(c(min(xcoord, na.rm = T), max(ycoord, na.rm = T)))
  if(!is.na(wkt_column)){
    wktcoord.nona <- as.character(na.omit(wktcoord))
    df.wkt <- data.frame(geom = wktcoord.nona)
    df.wkt <- sf::st_as_sf(df.wkt, wkt = "geom")
    bbox <- sf::st_bbox(df.wkt)
  }
  if(is.na(wkt_column)){
    bbox <- c(xmin = min(xcoord, na.rm = T),
              ymin = min(ycoord, na.rm = T),
              xmax = max(xcoord, na.rm = T),
              ymax = max(ycoord, na.rm = T)
    )
  }
  bbox.sfc <- sf::st_as_sfc(sf::st_bbox(bbox))
  # bbox.sfc <- sf::st_as_sfc(sf::st_bbox(pt1, pt2, pt3, pt4))
  bbox.sf <- sf::st_as_sf(bbox.sfc)
  if(is.na(dirOut)){
    dirOut <- dataDir
  } else {
    dir.create(dirOut, showWarnings = FALSE)
    }
  sf::st_write(bbox.sf, dsn = paste0(dirOut, geojson.name))
  if(verbose){print(paste0("the GeoJSON file '",
                           geojson.name,"' as been created in '",
                           dirOut,"'"))}
}

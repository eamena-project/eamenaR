#' Create the bounding box of BUs' geometries and save it as a GeoJSON file
#' @name geom_bbox
#' @description Read one or various XLSX worksheets within a folder.
#' Collect the xmin, xmax, ymin, xmax and creates the bounding box,
#' that is to say, the extent of the geometries
#'
#' @param dataDir the path to the folder where the XLSXs are
#' @param x_column the column of the X coordinates, if NA the function will look the field 'wkt'
#' @param y_column the column of the Y coordinates, if NA the function will look the field 'wkt'
#' @param wkt_column the column of the WKT coordinates. Useful if the X,Y coordinates are already in
#' @param dataOut the path to the folder where the GeoJSON file will be created.
#' @param geojson.name the name of the GeoJSON that will be created
#'
#' @return a GeoJSON file
#'
#' @examples
#'
#' geom_bbox(dataDir = "C:/Rprojects/eamena-arches-dev/data/bulk/bu/")
#'
#' @export
geom_bbox <- function(dataDir = NA,
                      x_column = "xcoord",
                      y_column = "ycoord",
                      wkt_column = NA,
                      dataOut = dataDir,
                      geojson.name = "geojson.geojson"){
  l.bus <- list.files(dataDir, full.names = T)
  xcoord <- ycoord <- c()
  for(bu.name in l.bus){
    # bu.name <- "AAA_f10_text.xlsx"
    print(paste0("*read: ", DescTools::SplitPath(bu.name)$fullfilename))
    df <- xlsx::read.xlsx(bu.name, sheetIndex = 1)
    print(paste0("     nb of rows: ", nrow(df)))
    if(!is.na(wkt_column)){
      print(paste0("   TODO; will read a WKT geometry"))
    } else {
      xcoord <- c(xcoord, df[ , x_column])
      ycoord <- c(ycoord, df[ , y_column])
    }
    # max(xcoord, na.rm = T)
    # max(ycoord, na.rm = T)
    # min(xcoord, na.rm = T)
    # min(ycoord, na.rm = T)
  }
  pt1 <- sf::st_point(c(min(xcoord, na.rm = T), min(ycoord, na.rm = T)))
  pt2 <- sf::st_point(c(max(xcoord, na.rm = T), min(ycoord, na.rm = T)))
  pt3 <- sf::st_point(c(max(xcoord, na.rm = T), max(ycoord, na.rm = T)))
  pt4 <- sf::st_point(c(min(xcoord, na.rm = T), max(ycoord, na.rm = T)))
  bbox.sfc <- sf::st_as_sfc(sf::st_bbox(pt1, pt2, pt3, pt4, pt5))
  bbox.sf <- sf::st_as_sf(bbox.sfc)
  sf::st_write(bbox.sf, dsn = paste0(dataOut, geojson.name))
  print(paste0("the GeoJSON file '", geojson.name,"' as been created in '", DescTools::SplitPath(bu.name)$dirname,"'"))
}



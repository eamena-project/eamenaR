#' Read a GeoJSON file
#'
#' @name geojson_read
#'
#' @description Read a GeoJSON file from a local path or a Zenodo deposit
#'
#' @param geojson.path path of GeoJSON file. Default 'caravanserail.geojson'.
#'
#' @return A list with a sf object (the GeoJSON file) and a caption
#'
#' @examples
#'
#'
#' @export
geojson_read <- function(geojson.path = paste0(system.file(package = "eamenaR"),
                                              "/extdata/caravanserail.geojson")){
  # field.names <- "Overall Condition State Type"
  # TODO: generalise from point to other geometries: centroid Polygon, Lines
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  if(is.character(geojson.path) & DescTools::SplitPath(geojson.path)$extension == "geojson"){
    if(verbose){
      print(paste0("ROI: Reads a GeoJSON path"))
    }
    ea.geojson <- sf::read_sf(geojson.path, quiet = TRUE)
    capt <- SplitPath(geojson.path)$fullfilename
  }
  if(grep('zenodo', geojson.path)){
  # if(grep('zenodo.*zip', geojson.path)){
    if(verbose){
      print(paste0("ROI: Download a ZIP file hosted on Zenodo"))
    }
    geojson.path.api <- gsub("doi.org/.*/zenodo.", replacement = "zenodo.org/api/records/", geojson.path)
    if(verbose){
      print(paste0("The Zenodo DOI has been converted into an URL API"))
    }
    temp <- tempfile()
    response <- httr::GET(geojson.path.api)
    response_content <- httr::content(response, "text")
    response_json <- jsonlite::fromJSON(response_content)
    geo.file <- as.character(response_json$files$links)
    download.file(geo.file,
                  destfile = temp, quiet = TRUE, mode = "wb")
    td <- tempdir()
    lfiles <- unzip(temp, exdir = td)
    if(verbose){
      cat("Downloaded file(s):", lfiles, "\n")
    }
    # filter to find the GeoJSON
    geojson_paths <- lfiles[grepl("\\.geojson$", lfiles, ignore.case = TRUE)]
    if(length(geojson_paths) == 0){
      stop("No GeoJSON files have been found in the Zenodo URL")
    }
    # compute the Zenodo citation (APA)
    bib.authors <- as.character(response_json$metadata$creators[1])
    bib.title <- response_json$title
    bib.url <- response_json$doi_url
    bib.year <- format(as.Date(response_json$metadata$publication_date, format = "%Y-%m-%d"), "%Y")
    bib.type <- response_json$metadata$resource_type$type
    capt <- paste0(bib.authors, ". (", bib.year, "). ", bib.title, " [", bib.type, "]. Zenodo. ", bib.url)
    # read only the first GeoJSON as it is expected to have one GeoJSON by ZIP
    ea.geojson <- sf::read_sf(geojson_paths[1], quiet = TRUE)
  }
  geojson.read <- list(ea.geojson, capt)
  return()
}


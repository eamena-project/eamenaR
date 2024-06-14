#' Plot the map distribution of Heritage Places created by year
#'
#' @name plot_hps
#'
#' @param df A dataframe created with the `ref_hps()` function
#' @param stadia_map_token A StadiaMap token for the maps background
#'
#' @return A list of ggplot
#'
#' @examples
#'
#'
#' @export
plot_hps <- function(df = NA,
                     stadia_map_token = "aa5c9739-90c7-410b-9e9b-6c904df6e4dd",
                     buff = 2,
                     verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  df <- df %>%
    dplyr::mutate(cdate = as.POSIXct(cdate)) %>%
    dplyr::group_by(ei) %>%
    dplyr::filter(cdate == min(cdate)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cdate = format(cdate, "%Y")) %>%
    dplyr::select(-ei, -teamname) %>%
    dplyr::arrange(desc(cdate))
  sf_df <- sf::st_as_sf(df, coords = c("x", "y"), crs = 4326)
  sf_df <- sf::st_transform(sf_df, 3857)

  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  GSpath <- "https://raw.githubusercontent.com/eamena-project/eamena-arches-dev/main/data/grids/"
  GSnew <- sf::st_read(paste0(GSpath, "EAMENA_Grid_contour.geojson"))
  GSold <- sf::st_read(paste0(GSpath, "(archives)/EAMENA_Grid_contour.geojson"))

  if(!is.na(stadia_map_token)){
    ggmap::register_stadiamaps(stadia_map_token)
    stamenbck <- ggmap::get_stadiamap(bbox = c(left = as.numeric(sf::st_bbox(GSnew)$xmin),
                                               bottom = as.numeric(sf::st_bbox(GSnew)$ymin),
                                               right = as.numeric(sf::st_bbox(GSnew)$xmax),
                                               top = as.numeric(sf::st_bbox(GSnew)$ymax)),
                                      maptype = "stamen_terrain_background",
                                      crop = FALSE,
                                      zoom = 5)
  }
  if(is.na(stadia_map_token)){
    # TODO
  }
  lg <- list()
  for(year in unique(sf_df$cdate)){
    GS <- GSold
    if(as.integer(year) > 2022){
      GS <- GSnew
    }
    sf_df1 <- sf_df[sf_df$cdate == year, ]
    nb_hp <- nrow(sf_df1)
    ggy <- ggmap::ggmap(stamenbck) +
      # ggplot2::geom_sf(data = ea.geojson, fill = 'red', inherit.aes = FALSE) +
      ggplot2::geom_sf(data = GS, fill = NA, color = "black", size = 0.5, inherit.aes = FALSE) +
      ggplot2::geom_sf(data = sf_df1, color = "black", size = 1, inherit.aes = FALSE) +
      ggplot2::labs(title = year,
                    caption = paste0("Heritage places created = ", nb_hp),
                    x = "",
                    y = "") +
      ggplot2::theme_minimal()
    lg[[length(lg) + 1]] <- ggy
    if(verbose){
      print(paste("map: ", year, " is done"))
    }
  }
  return(lg)
}



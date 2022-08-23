#' Basic statistics on a GeoJSON file
#' @name geojson_stat
#' @description Basic descriptive statistics on a GeoJSON file
#'
#' @param stat.name the name of the output file. By default "stat".
#' @param geojson.path the path of the GeoJSON file. By default 'caravanserail.geojson'
#' @param ids the IDs of the resources, by default "EAMENA.ID" (n.b: R fieldname format, without spaces)
#' @param stat the statistic that will be computed. By default 'list_fields' (list the fields)
#' The other options are: "list_ids" list EAMENA IDs ; etc.
#' @param export.stat if TRUE return the stats to be stored in a new variable
#' @param write.stat if TRUE, export the stats in a new file, if FALSE will only display it
#' @param dirOut the folder where the outputs will be saved. By default: '/results'.
#' If it doesn't exist, it will be created. Only useful is write.stat is TRUE
#'
#' @return Show or export basic statistics on the GeoJSON file
#'
#' @examples
#'
#' geojson_stat(stat.name = "geojson_fields", export.stat = T)
#'
#' @export
geojson_stat <- function(stat.name = "stat",
                         geojson.path = paste0(system.file(package = "eamenaR"), "/extdata/caravanserail.geojson"),
                         ids = "EAMENA.ID",
                         stat = c("list_fields"),
                         export.stat = F,
                         write.stat = F,
                         dirOut = paste0(system.file(package = "eamenaR"), "/results/")
){
  # geojson.path = paste0(system.file(package = "eamenaR"), "/extdata/caravanserail.geojson") ;
  # stat.name = "stat" ; stat = c("list_ids") ; export.stat = F ;
  # dirOut = paste0(system.file(package = "eamenaR"), "/results/")
  ea.geojson <- sf::st_read(geojson.path)
  if("list_fields" %in% stat){
    field.names <- colnames(ea.geojson)[! colnames(ea.geojson) %in% "geometry"]
    if (export.stat) {
      dir.create(dirOut, showWarnings = FALSE)
      tout <- paste0(dirOut, stat.name, "_list_fields.tsv")
      df <- data.frame(src.geojson = DescTools::SplitPath(geojson.path)$filename,
                       field.names = field.names)
      write.table(df, tout, sep = "\t", row.names = F)
      print(paste(tout, "is exported"))
    } else {
      cat(field.names, sep = "\n")
    }
  }
  if("list_ids" %in% stat){
    df <- data.frame(id = row.names(ea.geojson),
                     ea.ids = ea.geojson[[ids]])
    if (write.stat) {
      dir.create(dirOut, showWarnings = FALSE)
      tout <- paste0(dirOut, stat.name, "_list_ids.tsv")
      write.table(df, tout, sep = "\t", row.names = F)
      print(paste(tout, "is exported"))
    }
    if (export.stat) {
      rownames(df) <- df$id
      df$id <- NULL
      return(df)
    }
    if (!export.stat & !write.stat){
      cat(paste0(df$id,": ",df$ea.ids), sep =", ")
    }
  }
}

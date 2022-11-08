#' Identify if two or more EAMENA HP are real duplicates or not
#' @name ref_are_duplicates
#' @description values coming from different records will be put one against another to facilitate the comparisons between possible duplicates
#'
#' @param db.con the parameters for the Postgresql EAMENA DB, in a RPostgres::dbConnect() format. If null, will use a GeoJSON file.
#' @param d a hash() object (a Python-like dictionary).
#' @param resourceid.list a list with the ResourcesID to compare. By default: c("563567f7-eef0-4683-9e88-5e4be2452f80", "fb0a2ef4-023f-4d13-b931-132799bb7a6c")
#' @param export.table if TRUE will export the table of duplicates (FALSE by default).
#' @param dirOut the folder where the outputs will be saved. By default: '/results'. If it doesn't exist, it will be created. Only useful is export.plot.g is TRUE.
#' @param verbose if TRUE (by default), print messages
#'
#' @return a hash() object. If export.table is set to TRUE it will also create an XLSX table with the potential duplicates
#'
#' @examples
#'
#' d <- hash::hash()
#' my_con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
#'                                user = 'xxx',
#'                                password = 'xxx',
#'                                dbname = 'eamena',
#'                                host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
#'                                port = 5432)
#' @export

# 563567f7-eef0-4683-9e88-5e4be2452f80 and fb0a2ef4-023f-4d13-b931-132799bb7a6c : have the ~ same POLYGONS
ref_are_duplicates <- function(db.con = NA,
                               d = NA,
                               resourceid.list = c("563567f7-eef0-4683-9e88-5e4be2452f80",
                                                   "fb0a2ef4-023f-4d13-b931-132799bb7a6c"),
                               export.table = F,
                               dirOut = paste0(system.file(package = "eamenaR"), "/results/"),
                               verbose = TRUE){
  if(verbose){print("*check values of potential duplicates")}
  if(verbose){print("*end of check duplicates")}
  return(d)
}

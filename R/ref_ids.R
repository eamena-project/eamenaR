#' Find the equivalences between eamenaR concepts and specific Arches instances
#'
#' @name ref_ids
#'
#' @description This function read a table of correspondences between labels used in this R package, and labels used of a given instance of Arches. For example 'id' in this eamenaR package refers the 'EAMENA ID' in the EAMENA DB. This is useful to generalise the function to Arches instances having different labels. See the table here: https://github.com/eamena-oxford/eamenaR/blob/main/inst/extdata/ids.csv
#'
#' @param r.name the eamenaR label, by default NA.
#' @param ids.path the path to the correspondance table.
#'
#' @return A string which is the label used in the DB
#'
#' @examples
#'
#' ref_ids("id")
#' # "EAMENA ID"
#'
#' @export
ref_ids <- function(r.name = NA,
                    ids.path = paste0(system.file(package = "eamenaR"),
                                      "/extdata/ids.csv")){
  ids <- read.csv(ids.path)
  db.name <- ids[ids$r.name == r.name, "db.name"]
  return(db.name)
}



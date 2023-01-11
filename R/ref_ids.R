#' Find the equivalences between eamenaR concepts and specific Arches instances (called DB) both for labels and UUIDs
#'
#' @name ref_ids
#'
#' @description This function read a table of correspondences (a CSV file) between labels used in this eamenaR package (called `r.name`), and labels used of a given instance of Arches (called `db.name` or `db.uuid`). This function is useful to generalise the operability of this R package to other Arches instances than EAMENA. Indeed, each label or UUID is specific to a project. The only constant is the value of the label of Heritage Places ID in this package. For example 'id' in this eamenaR package refers the 'EAMENA ID' label in the EAMENA Arches instance. This latter label is recorded in the `db.name`. The UUID of this label is recorded in the `db.uuid` field. See the CSV correspondence table here: https://github.com/eamena-oxford/eamenaR/blob/main/inst/extdata/ids.csv
#'
#' @param value the eamenaR label, by default NA.
#' @param choice the output value, by default the name in the database (`"db.name"`).
#' @param ids.path the path to the correspondence table.
#'
#' @return A string which is the label used in the DB
#'
#' @examples
#'
#' # get the name in the database from the eamenaR name
#' ref_ids("id")
#' ## "EAMENA ID"
#'
#' # the same, with explicit options
#' ref_ids(in.value = "id", choice = "db.name")
#' ## "EAMENA ID"
#'
#' # get the opposite (name in the eamenaR package from the name in the DB)
#' ref_ids(in.value = "EAMENA ID", choice = "r.name")
#' ## "id"
#'
#' # get the "EAMENA ID" label UUID
#' ref_ids(in.value = "EAMENA ID", choice = "db.uuid")
#' ## "34cfe992-c2c0-11ea-9026-02e7594ce0a0"
#'
#' # the same with implicit options
#' ref_ids("EAMENA ID", "db.uuid")
#' ## "34cfe992-c2c0-11ea-9026-02e7594ce0a0"
#'
#' # other correspondences stored in variables
#' Investigator.Role.Type.uuid <- ref_ids(in.value = "Investigator Role Type", choice = "db.uuid")
#' Investigator.Role.Type.uuid
#' ## "d2e1ab96-cc05-11ea-a292-02e7594ce0a0"
#'
#' Geometric.Place.Expression.uuid <- ref_ids(in.value = "Geometric Place Expression", choice = "db.uuid")
#' Geometric.Place.Expression.uuid
#' ## "5348cf67-c2c5-11ea-9026-02e7594ce0a0"
#'
#' @export
ref_ids <- function(in.value = NA,
                    choice = "db.name",
                    ids.path = paste0(system.file(package = "eamenaR"),
                                      "/extdata/ids.csv")){
  ids <- read.csv(ids.path)
  if(in.value %in% ids$r.name){
    out.value <- ids[ids$r.name == in.value, choice]
  }
  if(in.value %in% ids$db.name){
    out.value <- ids[ids$db.name == in.value, choice]
  }
  if(in.value %in% ids$db.uuid){
    out.value <- ids[ids$db.uuid == in.value, choice]
  }
  return(out.value)
}

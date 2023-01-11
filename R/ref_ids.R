#' Find the equivalences between eamenaR concepts and specific Arches instances (called DB) both for concepts and UUIDs
#'
#' @name ref_ids
#'
#' @description This function read the table of correspondences `ids.csv`. This CSV file list the correspondences between concepts used in this eamenaR package (called `r.concept.name`), and concepts used of a given instance of Arches (called `db.concept.name` or their UUIDs: `db.concept.uuid`). This function is useful to generalise the operability of this R package to other Arches instances than EAMENA. Indeed, each concept or UUID is specific to a project. The only constant is the value of the concept of Heritage Places ID in this package. For example 'id' in this eamenaR package refers the 'EAMENA ID' concept in the EAMENA Arches instance. This latter concept is recorded in the `db.concept.name`. The UUID of this concept is recorded in the `db.concept.uuid` field. See the CSV correspondence table here: https://github.com/eamena-oxford/eamenaR/blob/main/inst/extdata/ids.csv
#'
#' @param value the eamenaR concept, by default NA.
#' @param choice the output value, by default the concept label name in the database (`"db.concept.name"`).
#' @param ids.path the path to the correspondence table.
#'
#' @return A string which is the concept used in the DB.
#'
#' @examples
#'
#' # get the name in the database from the eamenaR name
#' ref_ids("id")
#' ## "EAMENA ID"
#'
#' # the same, with explicit options
#' ref_ids(in.value = "id", choice = "db.concept.name")
#' ## "EAMENA ID"
#'
#' # get the opposite (name in the eamenaR package from the name in the DB)
#' ref_ids(in.value = "EAMENA ID", choice = "r.concept.name")
#' ## "id"
#'
#' # get the "EAMENA ID" concept UUID
#' ref_ids(in.value = "EAMENA ID", choice = "db.concept.uuid")
#' ## "34cfe992-c2c0-11ea-9026-02e7594ce0a0"
#'
#' # the same with implicit options
#' ref_ids("EAMENA ID", "db.concept.uuid")
#' ## "34cfe992-c2c0-11ea-9026-02e7594ce0a0"
#'
#' # the UUID of 'Cultural Period'
#' ref_ids(in.value = "Cultural Period", choice = "db.concept.uuid")
#' ## "5348cf67-c2c5-11ea-9026-02e7594ce0a0"
#'
#' # other correspondences stored in variables
#' Investigator.Role.Type.uuid <- ref_ids(in.value = "Investigator Role Type",
#'                                        choice = "db.concept.uuid")
#' Investigator.Role.Type.uuid
#' ## "d2e1ab96-cc05-11ea-a292-02e7594ce0a0"
#'
#' Geometric.Place.Expression.uuid <- ref_ids(in.value = "Geometric Place Expression",
#'                                            choice = "db.concept.uuid")
#' Geometric.Place.Expression.uuid
#' ## "5348cf67-c2c5-11ea-9026-02e7594ce0a0"
#'
#' @export
ref_ids <- function(in.value = NA,
                    choice = "db.concept.name",
                    ids.path = paste0(system.file(package = "eamenaR"),
                                      "/extdata/ids.csv")){
  ids <- read.csv(ids.path)
  if(in.value %in% ids$r.concept.name){
    out.value <- ids[ids$r.concept.name == in.value, choice]
  }
  if(in.value %in% ids$db.concept.name){
    out.value <- ids[ids$db.concept.name == in.value, choice]
  }
  if(in.value %in% ids$db.concept.uuid){
    out.value <- ids[ids$db.concept.uuid == in.value, choice]
  }
  return(out.value)
}

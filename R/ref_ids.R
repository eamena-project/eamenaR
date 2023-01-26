#' Find the equivalences between eamenaR concepts and specific Arches instances (called DB) both for concepts and UUIDs
#'
#' @name ref_ids
#'
#' @description This function read the table of correspondences `ids.csv`. This CSV file list the correspondences between concepts used in this eamenaR package (called `concept.name`), and concepts used of a given instance of Arches (called `db.concept.name` or their UUIDs: `db.concept.uuid`). All the values are uniques (no duplicates), as it, it is possible to find the exact matches over all the columns. This function is close to the CLI command `python manage.py whatisthis`, for example `python manage.py whatisthis 5b3489c0-cb8f-11ea-a292-02e7594ce0a0` returns `Measurement Number` This function is useful to generalise the operability of this R package to other Arches instances than EAMENA. Indeed, each concept or UUID is specific to a project. The only constant is the value of the concept of Heritage Places ID in this package. For example 'id' in this eamenaR package refers the 'EAMENA ID' concept in the EAMENA Arches instance. This latter concept is recorded in the `db.concept.name`. The UUID of this concept is recorded in the `db.concept.uuid` field. See the CSV correspondence table here: https://github.com/eamena-project/eamenaR/blob/main/inst/extdata/ids.csv
#'
#' @param concept.name a value existing in the `ids.csv` file. By default NA.
#' @param choice the output value, by default the concept label name in the database (`"db.concept.name"`).
#' @param ids.path the path to the correspondence table.
#'
#' @return A string which is the concept used in the DB.
#'
#' @examples
#'
#' # get the name in the database from the eamenaR name
#' ref_ids("hp.id")
#' ## "EAMENA ID"
#'
#' # the same, with explicit options
#' ref_ids(concept.name = "hp.id", choice = "db.concept.name")
#' ## "EAMENA ID"
#'
#' # get the opposite (name in the eamenaR package from the name in the DB)
#' ref_ids(concept.name = "EAMENA ID", choice = "concept.name")
#' ## "id"
#'
#' # get the "EAMENA ID" concept UUID
#' ref_ids(concept.name = "EAMENA ID", choice = "db.concept.uuid")
#' ## "34cfe992-c2c0-11ea-9026-02e7594ce0a0"
#'
#' # the same with implicit options
#' ref_ids("EAMENA ID", "db.concept.uuid")
#' ## "34cfe992-c2c0-11ea-9026-02e7594ce0a0"
#'
#' # the UUID of 'Cultural Period'
#' ref_ids(concept.name = "Cultural Period", choice = "db.concept.uuid")
#' ## "5348cf67-c2c5-11ea-9026-02e7594ce0a0"
#'
#' # other correspondences stored in variables
#' Investigator.Role.Type.uuid <- ref_ids(concept.name = "Investigator Role Type",
#'                                        choice = "db.concept.uuid")
#' Investigator.Role.Type.uuid
#' ## "d2e1ab96-cc05-11ea-a292-02e7594ce0a0"
#'
#' Geometric.Place.Expression.uuid <- ref_ids(concept.name = "Geometric Place Expression",
#'                                            choice = "db.concept.uuid")
#' Geometric.Place.Expression.uuid
#' ## "5348cf67-c2c5-11ea-9026-02e7594ce0a0"
#'
#' @export
ref_ids <- function(concept.name = NA,
                    choice = "db.concept.name",
                    ids.path = paste0(system.file(package = "eamenaR"),
                                      "/extdata/ids.csv")){
  ids <- read.csv(ids.path)
  # check if exist, by columns
  if(concept.name %in% ids$r.concept.name){
    out.value <- ids[ids$r.concept.name == concept.name, choice]
  }
  if(concept.name %in% ids$db.concept.name){
    out.value <- ids[ids$db.concept.name == concept.name, choice]
  }
  if(concept.name %in% ids$db.concept.uuid){
    out.value <- ids[ids$db.concept.uuid == concept.name, choice]
  }
  if(!exists("out.value")){
    stop(paste0("The value '", concept.name, "' doesn't exists in the table of correspondences '",
                DescTools::SplitPath(ids.path)$fullfilename, "' \n",
                "Check '", ids.path,"')"))
  }
  return(out.value)
}

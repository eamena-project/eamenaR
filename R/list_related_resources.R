#' List the connected components of an HP
#'
#' @name list_related_resources
#'
#' @description With a given HP, find all the connected components.
#'
#' @param db.con a `dbConnect` connection to the database.
#' @param d a hash() object (a Python-like dictionary).
#' @param field the field name that will be created in the `d` hash() object.
#' @param id the ID of an HP, either an UUID or an EAMENA ID. By default, NA.
#' @param relationshiptype the type of relation existing betwen an HP and a BC. By default, the CIDOC-CRM 'PX_is_related_to'.
#' @param disconn if TRUE (by defalut), will disconnect from the DB.
#' @param verbose if TRUE (by default), print messages.
#'
#' @return A dataframe with the IDs and UUIDs of HP and CC
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
#'
#' df <- list_related_resources(db.con = my_con,
#'                              d = d,
#'                              id = "EAMENA-0164943")
#' df
#' ##            hp.id                              hp.uuid             cc.id                              cc.uuid
#' ## 1 EAMENA-0164943 d4feb830-10c7-4d80-a19e-e608f424be4c COMPONENT-0000141 90400bb6-ff54-4afd-8183-65c67fa97448
#' ## 2 EAMENA-0164943 d4feb830-10c7-4d80-a19e-e608f424be4c COMPONENT-0000143 0dab164a-6d3a-443c-954a-50d93efbff35
#'
#' @export
list_related_resources <- function(db.con = NA,
                                   d = NA,
                                   field = NA,
                                   id = NA,
                                   relationshiptype = 'PX_is_related_to',
                                   disconn = TRUE,
                                   verbose = TRUE){
  # id = "EAMENA-0164943" ; db.con = my_con ;  relationshiptype = 'PX_is_related_to'

  # get the UUID of the HP
  d.id <- hash::hash()
  d.id <- uuid_id(db.con = my_con,
                  d = d.id,
                  id = id,
                  disconn = F,
                  verbose = verbose)
  # concept.uuid <- eamenaR::ref_ids(in.value = concept.name,
  #                                  choice = "db.concept.uuid")
  # get the UUID of the connected components of a given HP
  if (!is.na(d.id$uuid)) {
    sqll <- stringr::str_interp("
      SELECT resourceinstanceidfrom
      FROM resource_x_resource
      WHERE resourceinstanceidto::text = '${d.id$uuid}'
      AND relationshiptype LIKE '%${relationshiptype}%'
                       ")
  } else {
    stop("Nothing found: the function stops")
  }
  connected.components <- RPostgres::dbGetQuery(db.con, sqll)
  nb.cc <- length(connected.components$resourceinstanceidfrom)
  if(verbose){
    print(paste0(d.id$id, " has ", nb.cc,
                 " Related Resource(s)"))
  }
  cc.ids <- c()
  for(cc in connected.components$resourceinstanceidfrom){
    # cc <- '90400bb6-ff54-4afd-8183-65c67fa97448'
    dcc.id <- hash::hash()
    dcc.id <- uuid_id(db.con = my_con,
                      d = dcc.id,
                      id = cc,
                      rm = "cc",
                      disconn = F)
    cc.ids <- c(cc.ids, dcc.id$id)
  }
  df <- data.frame(hp.id = rep(d.id$id, nb.cc),
                   hp.uuid = rep(d.id$uuid, nb.cc),
                   cc.id = cc.ids,
                   cc.uuid = connected.components$resourceinstanceidfrom)
  if(disconn){
    DBI::dbDisconnect(db.con)
    if(verbose){print(paste0("disconnected from the DB"))}
  }
  return(df)
}

# d <- hash::hash()
# my_con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
#                                user = 'postgres',
#                                password = 'postgis',
#                                dbname = 'eamena',
#                                host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
#                                port = 5432)
#
# df <- list_related_resources(db.con = my_con,
#                              d = d,
#                              relationshiptype = 'L33_has_maker',
#                              id = "EAMENA-0164943")
# df

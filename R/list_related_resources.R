#' List the related resources of an HP
#'
#' @name list_related_resources
#'
#' @description With a given HP, find all the related resources.
#'
#' @param db.con a `dbConnect` connection to the database.
#' @param d a hash() object (a Python-like dictionary).
#' @param field the field name that will be created in the `d` hash() object.
#' @param id the ID of an HP, either an UUID or an EAMENA ID. By default, NA.
#' @param relationshiptype the type of relation existing between an HP and a BC (for exmple the CIDOC-CRM 'PX_is_related_to').
#' @param relationshipwith the RM on which the related resources will be retrieved: "hp", "ir", "po", "cc", ...
#' @param disconn if TRUE (by defalut), will disconnect from the DB.
#' @param verbose if TRUE (by default), print messages.
#'
#' @return A dataframe with the IDs and UUIDs of HP and rr
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
#' ##            hp.id                              hp.uuid             rr.id                              rr.uuid
#' ## 1 EAMENA-0164943 d4feb830-10c7-4d80-a19e-e608f424be4c COMPONENT-0000141 90400bb6-ff54-4afd-8183-65c67fa97448
#' ## 2 EAMENA-0164943 d4feb830-10c7-4d80-a19e-e608f424be4c COMPONENT-0000143 0dab164a-6d3a-443c-954a-50d93efbff35
#'
#' @export
list_related_resources <- function(db.con = NA,
                                   d = NA,
                                   field = NA,
                                   id = NA,
                                   relationshipwith = NA,
                                   relationshiptype = NA,
                                   relationshipdirect = c("from", "to"),
                                   disconn = TRUE,
                                   verbose = TRUE){
  # TODO generalise this function, not only for HP
  # id = "EAMENA-0164943" ; db.con = my_con ;  relationshiptype = 'PX_is_related_to'
  if(!(relationshipwith %in% c('po', 'ir', 'cc', 'hp'))){
    stop("Select an approriate RM")
  }
  # get the UUID of the HP
  d.id <- hash::hash()
  d.id <- uuid_id(db.con = my_con,
                  d = d.id,
                  id = id,
                  rm = "hp",
                  disconn = F,
                  verbose = verbose)
  # concept.uuid <- eamenaR::ref_ids(in.value = concept.name,
  #                                  choice = "db.concept.uuid")
  # get the UUID of the connected components of a given HP
  if (!is.na(d.id$uuid)) {
    sqll <- stringr::str_interp("
      SELECT resourceinstanceidfrom, resourceinstanceidto
      FROM resource_x_resource
                       ")
    if("from" %in% relationshipdirect & !("to" %in% relationshipdirect)){
      sqll <- paste0(sqll,
                     stringr::str_interp("
                                         WHERE resourceinstanceidfrom::text = '${d.id$uuid}'
                                         ")
      )
    }
    if("to" %in% relationshipdirect & !("from" %in% relationshipdirect)){
      sqll <- paste0(sqll,
                     stringr::str_interp("
                                         WHERE resourceinstanceidto::text = '${d.id$uuid}'
                                         ")
      )
    }
    if("from" %in% relationshipdirect & "to" %in% relationshipdirect){
      sqll <- paste0(sqll,
                     stringr::str_interp("
                                         WHERE resourceinstanceidfrom::text = '${d.id$uuid}'
                                         OR resourceinstanceidto::text = '${d.id$uuid}'
                                         ")
      )
    }
    if(!is.na(relationshiptype)){
      sqll <- paste0(sqll,
                     stringr::str_interp("
                                         AND relationshiptype LIKE '%${relationshiptype}%'
                                         ")
      )
    }
    if(verbose){
      print("SQL:\n") ; cat(sqll)
    }
  } else {
    stop("Nothing found: the function stops")
  }
  rel.rsrc <- RPostgres::dbGetQuery(db.con, sqll)
  nb.rr <- length(rel.rsrc$resourceinstanceidfrom)
  if(verbose){
    print(paste0(d.id$id, " has ", nb.rr,
                 " Related Resource(s)"))
  }
  rr.ids <- c()
  for(rr in rel.rsrc$resourceinstanceidfrom){
    # rr <- '90400bb6-ff54-4afd-8183-65c67fa97448'
    drr.id <- hash::hash()
    drr.id <- uuid_id(db.con = my_con,
                      d = drr.id,
                      id = rr,
                      rm = relationshipwith,
                      disconn = F)
    rr.ids <- c(rr.ids, drr.id$id)
  }
  df <- data.frame(hp.id = rep(d.id$id, nb.rr),
                   hp.uuid = rep(d.id$uuid, nb.rr),
                   rr.id = rr.ids,
                   rr.uuid = rel.rsrc$resourceinstanceidfrom)
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

#' Return the UUID of a specific Heritage Place, or Connected Component, from its ID, or the opposite by connecting the DB.
#'
#' @name uuid_id
#'
#' @description Return the ResourceID of a feature in a Resource Model (ex: an Heritage Place, a Connected Component) from its EAMENA ID, or the opposite: the ResourceID from the EAMENA ID, and store these ID into a hash() object. A connection with the EAMENA database is needed. The ResourceID is a UUID. This function uses the `ref_ids()` one for interoperability purposes.
#'
#' @param db.con a `dbConnect` connection to the database.
#' @param d a hash() object (a Python-like dictionary).
#' @param id a project ID (eg. "EAMENA-0187363") or a ResourceID (eg. "12053a2b-9127-47a4-990f-7f5279cd89da").
#' @param id.prj.patt a regex matching with the project IDs, by default `"^EAMENA-"`.
#' @param field.id the name of the field that will be created in the a hash() object for the EAMENA ID. By default 'id'.
#' @param field.uuid the name of the field that will be created in the a hash() object for the UUID. By default 'uuid'.
#' @param rm the Resource Model (ex: HP, connected components). The available values are: "hp" for Heritage places, "cc" for connected compobents (ex: Built component). By default "hp".
#' @param disconn if TRUE (default) will disconnect from the DB once done. If FALSE, the user has to disconnect (eg. DBI::dbDisconnect(my_con)).
#' @param verbose if TRUE (by default) verbose.
#'
#' @return a hash() object (a Python-like dictionary) with EAMENA ID and ResourceID. If a given ID doesn't exist, will fill the value of the hash dictionary with NA.
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
#' ## Heritage places
#' # from the EAMENA ID to the UUID
#' d <- uuid_id(db.con = my_con,
#'                    d = d,
#'                    id = "EAMENA-0187363",
#'                    disconn = FALSE)
#' d$uuid
#' # [1] "12053a2b-9127-47a4-990f-7f5279cd89da"
#'
#' # from the UUID to the EAMENA ID
#' d <- hash::hash()
#' d <- uuid_id(db.con = my_con,
#'                    d = d,
#'                    id = "12053a2b-9127-47a4-990f-7f5279cd89da",
#'                    disconn = FALSE)
# d$id
#' # [1] "EAMENA-0187363"
#'
#' ## Built Components
#'
#' d <- hash::hash()
#'
#' # from the COMPONENT ID to the UUID
#' d <- uuid_id(db.con = my_con,
#'                    d = d,
#'                    id = "COMPONENT-0000141",
#'                    id.prj.patt = "^COMPONENT-",
#'                    rm = "cc",
#'                    disconn = FALSE)
#' d$uuid
#' # [1] "90400bb6-ff54-4afd-8183-65c67fa97448"
#'
#' # from the UUID to the COMPONENT ID
#' d <- hash::hash()
#' d <- uuid_id(db.con = my_con,
#'                    d = d,
#'                    id = "90400bb6-ff54-4afd-8183-65c67fa97448",
#'                    rm = "cc",
#'                    disconn = TRUE)
#' d$id
#' # [1] "COMPONENT-0000141"
#'
#' @export
# TODO: rename to `uuid_id()`
uuid_id <- function(db.con = NA,
                          d = NA,
                          id = NA,
                          field.id = "id",
                          field.uuid = "uuid",
                          id.prj.patt = "^EAMENA-",
                          rm = "hp",
                          disconn = TRUE,
                          verbose = TRUE){
  # id = '90400bb6-ff54-4afd-8183-65c67fa97448'
  # id = 'COMPONENT-0000141' ; id.prj.patt = "^COMPONENT-"
  if(rm == "hp"){
    db.name <- eamenaR::ref_ids("hp.id")
  }
  if(rm == "cc"){
    db.name <- eamenaR::ref_ids("cc.id")
  }
  uuid <- eamenaR::ref_ids(db.name, "db.concept.uuid")

  # from project ID (like the "EAMENA ID") to UUID
  if(grepl(id.prj.patt, id)){
    # return the ResourceID
    # id <-  "EAMENA-0187363"
    # id <-  c("EAMENA-0187363", "EAMENA-0184752", "EAMENA-0076769")
    if (length(id) == 1) {
      sqll <- stringr::str_interp("
      SELECT
      resourceinstanceid AS resourceid
      FROM tiles
      WHERE tiledata ->> '${uuid}'::text LIKE '%${id}%'
                       ")
    }
    ## TODO??: does it work for only 1 UUID?
    # if (length(id) > 1) {
    #   ids <- paste0(id, collapse = "|")
    #   sqll <- stringr::str_interp("
    #   SELECT
    #   resourceinstanceid AS resourceid
    #   FROM tiles
    #   WHERE tiledata ->> '${uuid}'::text SIMILAR TO '%(${ids})%'
    #                    ")
    # }
    df <- RPostgres::dbGetQuery(db.con, sqll)
    if(length(as.character(df$resourceid)) == 0){
      d[[field.uuid]] <- NA
      if(verbose){
        print("WWW")
        warning(paste0("the ID doesn't match: found nothing in the DB. Maybe a typo in: '", id,"'"))
      }
    } else {
      d[[field.uuid]] <- as.character(df$resourceid)
      if(verbose){
        print("found id")
      }
    }
    d[[field.id]] <- id
  }

  # from UUID to project ID
  if(!grepl(id.prj.patt, id)){
    # return the EAMENA ID
    if (length(id) == 1) {
      sqll <- stringr::str_interp("
      SELECT
      tiledata ->> '${uuid}'::text AS dbid
      FROM tiles
      WHERE resourceinstanceid::text LIKE '%${id}%'
      AND tiledata -> '${uuid}' IS NOT NULL
                       ")
    }
    df <- RPostgres::dbGetQuery(db.con, sqll)
    if(length(as.character(df$dbid)) == 0){
      d[[field.id]] <- NA
    } else {
        d[[field.id]] <- as.character(df$dbid)
        }
    d[[field.uuid]] <- id
  }
  if(disconn){
    DBI::dbDisconnect(db.con)
  }
  return(d)
}

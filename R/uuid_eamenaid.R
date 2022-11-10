#' Return the UUID of a HP from EAMENA ID, or the opposite
#'
#' @name uuid_id
#' @description Return the ResourceID (which is a UUID) of a HP from EAMENA id, or the opposite, and store these ID into a hash() object. A connection with the EAMENA database is needed.
#'
#' @param db.name the name of the database, by default 'eamena'.
#' @param db.user the name of the user, by default 'postgres'.
#' @param db.password the user database password.
#' @param db.host the name, or the IP, of the database.
#' @param db.port the database port number, by default: 5432.
#' @param d a hash() object (a Python-like dictionary).
#' @param id a EAMENA ID (eg. "EAMENA-0187363") or a ResourceID (eg. "12053a2b-9127-47a4-990f-7f5279cd89da").
#' @param field.uuid the name of the field that will be created in the a hash() object for the UUID. By default 'uuid'.
#' @param field.id the name of the field that will be created in the a hash() object for the EAMENA ID. By default 'id'.
#' @param disconn if TRUE (default) will disconnect from the DB once done. If FALSE, the user has to disconnect (eg. DBI::dbDisconnect(my_con))
#'
#' @return a hash() object (a Python-like dictionary) with EAMENA ID and ResourceID. If a given ID doesn't exist, will fill the value of the hash dictionary with NA.
#'
#' @examples
#'
#'d <- hash::hash()
#' my_con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
#'                                user = 'postgres',
#'                                password = 'postgis',
#'                                dbname = 'eamena',
#'                                host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
#'                                port = 5432)
#'
#' # from the EAMENA ID
#' d <- uuid_eamenaid(db.con = my_con,
#'                    d = d,
#'                    id = "EAMENA-0187363",
#'                    disconn = FALSE)
#' d$resourceid
#' # [1] "12053a2b-9127-47a4-990f-7f5279cd89da"
#'
#' # from the ResourceID
#' d <- uuid_eamenaid(db.con = my_con,
#'                    d = d,
#'                    id = "12053a2b-9127-47a4-990f-7f5279cd89da")
#' d$eamenaid
#' # [1] "EAMENA-0187363"
#'
#' @export
uuid_eamenaid <- function(db.con = NA,
                          d = NA,
                          id = NA,
                          field.eamenaid = "eamenaid",
                          field.resourceid = "resourceid",
                          disconn = TRUE){
  if(grepl("^EAMENA-", id)){
    # return the ResourceID
    # id <-  c("EAMENA-0187363", "EAMENA-0184752", "EAMENA-0076769")
    if (length(id) == 1) {
      sqll <- stringr::str_interp("
      SELECT
      resourceinstanceid AS resourceid
      FROM tiles
      WHERE tiledata ->> '34cfe992-c2c0-11ea-9026-02e7594ce0a0'::text LIKE '%${id}%'
                       ")
    }
    if (length(id) > 1) {
      # TODO: does it work for only 1 UUID?
      ids <- paste0(id, collapse = "|")
      sqll <- stringr::str_interp("
      SELECT
      resourceinstanceid AS resourceid
      FROM tiles
      WHERE tiledata ->> '34cfe992-c2c0-11ea-9026-02e7594ce0a0'::text SIMILAR to '%(${ids})%'
                       ")
    }
    df <- RPostgres::dbGetQuery(db.con, sqll)
    if(length(as.character(df$resourceid)) == 0){
      d[[field.resourceid]] <- NA
    } else {
      d[[field.resourceid]] <- as.character(df$resourceid)
    }
    d[[field.eamenaid]] <- id
  }
  if(!grepl("^EAMENA-", id)){
    # return the EAMENA ID
    if (length(id) == 1) {
      sqll <- stringr::str_interp("
      SELECT
      tiledata ->> '34cfe992-c2c0-11ea-9026-02e7594ce0a0'::text AS eamenaid
      FROM tiles
      WHERE resourceinstanceid::text LIKE '%${id}%'
      AND tiledata -> '34cfe992-c2c0-11ea-9026-02e7594ce0a0' IS NOT NULL
                       ")
    }
    df <- RPostgres::dbGetQuery(db.con, sqll)
    if(length(as.character(df$eamenaid)) == 0){
      d[[field.eamenaid]] <- NA
    } else {
        d[[field.eamenaid]] <- as.character(df$eamenaid)
        }
    d[[field.resourceid]] <- id
  }
  if(disconn){
    DBI::dbDisconnect(db.con)
  }
  return(d)
}

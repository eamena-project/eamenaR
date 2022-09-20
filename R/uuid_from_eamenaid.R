#' Return the UUID of a HP from EAMENA ID
#' @name uuid_from_eamenaid
#' @description Return the UUID of a HP from EAMENA id and store it into a
#' hash() object alongside the EAMENA id. A connection with the database is needed
#'
#' @param db.name the name of the database, by default 'eamena'.
#' @param db.user the name of the user, by default 'postgres'.
#' @param db.password the user database password.
#' @param db.host the name, or the IP, of the database.
#' @param db.port the database port number, by default: 5432.
#' @param d a hash() object (a Python-like dictionary).
#' @param eamenaid a EAMENA ID (eg. "EAMENA-0187363").
#' @param field.uuid the name of the field that will be created in the a hash() object for the UUID. By default 'uuid'.
#' @param field.eamenaid the name of the field that will be created in the a hash() object for the EAMENA ID. By default 'eamenaid'.
#' @return a hash() object (a Python-like dictionary) with EAMENA ID and UUID.
#'
#' @examples
#' d_sql <- hash::hash()
#' d_sql <- uuid_from_eamenaid(db.name = 'eamena',
#'                             db.password = 'postgis',
#'                             db.host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
#'                             d = d_sql,
#'                             eamenaid = "EAMENA-0187363")
#' d_sql$uuid
#' # [1] "12053a2b-9127-47a4-990f-7f5279cd89da"
#'
#' @export
uuid_from_eamenaid <- function(db.name = 'eamena',
                               db.user = 'postgres',
                               db.password = NA,
                               db.host = NA,
                               db.port = 5432,
                               d,
                               eamenaid,
                               field.uuid = "uuid",
                               field.eamenaid = "eamenaid"){
  # eamenaid <-  c("EAMENA-0187363", "EAMENA-0184752", "EAMENA-0076769")
  if (length(eamenaid) == 1) {
    sqll <- stringr::str_interp("
    SELECT
    resourceinstanceid
    FROM tiles
    WHERE tiledata ->> '34cfe992-c2c0-11ea-9026-02e7594ce0a0'::text LIKE '%${eamenaid}%'
                       ")
  }
  if (length(eamenaid) > 1) {
    # TODO: does it work for only 1 UUID?
    eamenaids <- paste0(eamenaid, collapse = "|")
    sqll <- stringr::str_interp("
    SELECT
    resourceinstanceid
    FROM tiles
    WHERE tiledata ->> '34cfe992-c2c0-11ea-9026-02e7594ce0a0'::text SIMILAR to '%(${eamenaids})%'
                       ")
  }
  drv <- RPostgres::Postgres()
  con <- DBI::dbConnect(drv,
                        user = db.user,
                        password = db.password,
                        dbname = db.name,
                        host = db.host,
                        port = db.port)
  df <- DBI::dbGetQuery(con, sqll)
  d[[field.eamenaid]] <- eamenaid
  d[[field.uuid]] <- as.character(df$resourceinstanceid)
  DBI::dbDisconnect(con)
  return(d)
}

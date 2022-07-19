#' Return the UUID of a HP from EAMENA ID
#' @name uuid_from_eamenaid
#' @description Return the UUID of a HP from EAMENA id and store it into a
#' hash() object alongside the EAMENA id. A connection with the database is needed
#'
#' @param db the name of the database, by default 'eamena'
#' @param d a hash() object (a Python-like dictionary)
#' @param eamenaid a EAMENA ID (eg. "EAMENA-0187363")
#' @param field.uuid the name of the field that will be created in the a
#' hash() object #' for the UUID
#' @param field.eamenaid the name of the field that will be created in the a
#' hash() object #' for the EAMENA ID
#' @return a hash() object (a Python-like dictionary) with EAMENA ID and UUID
#'
#' @examples
#' d_sql <- hash::hash() # hash instance to store the results
#' d_sql <- uuid_from_eamenaid("eamena", d_sql, "EAMENA-0187363")
#'
#' @export
uuid_from_eamenaid <- function(db, d, eamenaid, field.uuid = "uuid", field.eamenaid = "eamenaid"){
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
  con <- my_con(db) # load the Pg connection
  df <- DBI::dbGetQuery(con, sqll)
  d[[field.eamenaid]] <- eamenaid
  d[[field.uuid]] <- as.character(df$resourceinstanceid)
  DBI::dbDisconnect(con)
  return(d)
}

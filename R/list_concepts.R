#' List the name of all the child-concepts below a certain Concept node
#' @name list_concepts
#' @description With a given concept UUID (v. Reference Data Manager), find all
#' the childs. This function has been tested only for Cultural and Subcultural periods
#'
#' @param db.con a `dbConnect` connection to the database
#' @param d a hash() object (a Python-like dictionary)
#' @param field the field name that will be created in the a hash() object
#' @param uuid the UUID of the Concept parent. For example, '3b5c9ac7-5615-3de6-9e2d-4cd7ef7460e4' is the UUID of ..
#'
#' @return A dataframe stored in the input hash() object, under the selected 'field' name. This dataframe will with listed child-concepts in the provided field name. The UUID of each sub-concept will be stored into the 'field.uuid' column of the dataframe
#'
#' @examples.
#'
#' # in the connection parameters, replace 'xxx' by the password
#' d <- hash::hash()
#' my_con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
#'                     user = 'postgres',
#'                     password = xxx,
#'                     dbname = 'eamena',
#'                     host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
#'                     port = 5432)
#' # Cultural periods
#' d <- list_concepts(db.con = my_con, d = d, field = "cultural_periods", uuid = '3b5c9ac7-5615-3de6-9e2d-4cd7ef7460e4')
#'
#' # Subcultural periods
#' d <- list_concepts(db.con = my_con, d = d, field = "subcultural_periods", uuid = '16cb160e-7b31-4872-b2ca-6305ad311011')
#'
#' # disconnect
#' RPostgres::dbDisconnect(my_con)
#'
#' @export
list_concepts <- function(db.con = NA,
                          d = NA,
                          field = NA,
                          uuid = NA){
  # field <- "cultural_period" ; uuid <- '3b5c9ac7-5615-3de6-9e2d-4cd7ef7460e4' ;  db.con = my_con
  sqll <- "
  SELECT conceptidfrom as from, conceptidto as to FROM relations
  "
  relations <- RPostgres::dbGetQuery(db.con, sqll)
  # subset the Concepts graph on the selected UUID
  g <- igraph::graph_from_data_frame(relations, directed = TRUE)
  nodes.subgraph <- igraph::subcomponent(g, uuid, mode = "out")
  subgraph.names <- subgraph.uuid <- igraph::subgraph(g, nodes.subgraph)
  # get the name of the nodes from their UUID
  l.uuids <- igraph::as_ids(igraph::V(subgraph.uuid))
  for(uuid_ in l.uuids){
    # uuid_ <- "ea784c69-d61d-4bfc-9aa9-b3fb0bfa1b42"
    sqll <- stringr::str_interp("
    SELECT value FROM values
    WHERE conceptid = '${uuid_}'
    AND languageid = 'en-US'
    AND valuetype = 'prefLabel'
                       ")
    uuid_name <- RPostgres::dbGetQuery(db.con, sqll)
    uuid_name <- as.character(uuid_name)
    igraph::V(subgraph.names)$name[igraph::V(subgraph.names)$name == uuid_] <- uuid_name
  }
  d[[field]] <- subgraph.names
  field.uuid <- paste0(field, ".uuid")
  d[[field.uuid]] <- subgraph.uuid
  # RPostgres::dbDisconnect(db.con)
  return(d)
}

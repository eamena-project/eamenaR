#' List the name of all the child-concepts below a certain Concept node
#'
#' @name list_child_concepts
#'
#' @description With a given concept UUID (v. Reference Data Manager), find all the child nodes.
#'
#' @param db.con a `dbConnect` connection to the database.
#' @param d a hash() object (a Python-like dictionary).
#' @param field the field name that will be created in the `d` hash() object.
#' @param concept.name a concept label name (either `r.concept.name` or `db.concept.name`). This `concept.name` value coming from the `ids.csv` file (see `ref_ids()`). By default, NA.
#' @param disconn if TRUE (by default), will disconnect from the DB.
#' @param verbose if TRUE (by default), print messages.
#'
#' @return A `igraph` object stored in the input hash() object, under the selected 'field' name. This dataframe will with listed child-concepts in the provided field name. The UUID of each sub-concept will be stored into the 'field.uuid' column of the dataframe
#'
#' @examples.
#'
#' # create a Postgres connection (replace 'xxx' by the password)
#' d <- hash::hash()
#' my_con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
#'                                user = 'xxx',
#'                                password = 'xxx',
#'                                dbname = 'eamena',
#'                                host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
#'                                port = 5432)
#'
#' # Disturbance Extent Type
#' d <- list_child_concepts(db.con = my_con,
#'                          d = d,
#'                          field = "Disturbance Extent Type",
#'                          concept.name = "Disturbance Extent Type",
#'                          disconn = F)
#'
#' # Cultural periods & Subcultural periods and disconnect from the DB
#' d <- list_child_concepts(db.con = my_con,
#'                          d = d,
#'                          field = "cultural_periods",
#'                          concept.name = "Cultural Period",
#'                          disconn = F)
#' d <- list_child_concepts(db.con = my_con,
#'                          d = d,
#'                          field = "subcultural_periods",
#'                          concept.name = "Cultural Sub-Period",
#'                          disconn = T)
#'
#' # see this latter subgraph
#' d$subcultural_periods
#' ## IGRAPH 9ceb33f DN-- 256 467 --
#' ## + attr: name (v/c)
#' ## + edges from 9ceb33f (vertex names):
#' ## [1] Classical/Protohistoric/Pre-Islamic (North Africa)->Roman Imperial (North Africa)
#' ## [2] Classical/Protohistoric/Pre-Islamic (North Africa)->Roman Imperial (North Africa)
#' ## [3] Classical/Protohistoric/Pre-Islamic (North Africa)->Vandal (Maghreb)
#' ## [4] Classical/Protohistoric/Pre-Islamic (North Africa)->Vandal (Maghreb)
#' ## [5] Classical/Protohistoric/Pre-Islamic (North Africa)->Roman/Late Antique (North Africa)
#' ## [6] Classical/Protohistoric/Pre-Islamic (North Africa)->Roman/Late Antique (North Africa)
#' ## [7] Classical/Protohistoric/Pre-Islamic (North Africa)->Protohistoric, Late (Mauritania)
#' ## [8] Classical/Protohistoric/Pre-Islamic (North Africa)->Protohistoric, Late (Mauritania)
#' ## + ... omitted several edges
#'
#'
#' # Structural Component
#' d <- list_child_concepts(db.con = my_con,
#'                          d = d,
#'                          field = "Structural.Component",
#'                          concept.name = "Structural Component",
#'                          disconn = F)
#' df <- unique(igraph::as_edgelist(d$Structural.Component, names = TRUE))
#' df[order(df[ , 2], decreasing = F), ]
#' ## [,1]                   [,2]
#' ## [1,] "Structural Component" "Arch"
#' ## [2,] "Structural Component" "Balcony"
#' ## [3,] "Structural Component" "Ceiling"
#' ## [4,] "Structural Component" "character(0)"
#' ## [5,] "Structural Component" "Column"
#' ## [6,] "Structural Component" "Cornice"
#' ## [7,] "Structural Component" "Dome"
#' ## [8,] "Structural Component" "Floor"
#' ## [9,] "Structural Component" "Foundation"
#' ## [10,] "Structural Component" "Gate"
#' ## ...
#'
#' @export
list_child_concepts <- function(db.con = NA,
                                d = NA,
                                field = NA,
                                concept.name = NA,
                                disconn = TRUE,
                                verbose = TRUE){
  concept.uuid <- eamenaR::ref_ids(in.value = concept.name,
                                   choice = "db.concept.uuid")
  if(verbose){print(paste0("*read relations between concepts"))}
  sqll <- "
  SELECT conceptidfrom as from, conceptidto as to FROM relations
  "
  relations <- RPostgres::dbGetQuery(db.con, sqll)
  if(verbose){print(paste0("   ... done"))}
  if(verbose){print(paste0("create an `igraph` and get the appropriate subgraph"))}
  # subset the Concepts graph on the selected UUID
  g <- igraph::graph_from_data_frame(relations, directed = TRUE)
  # a particular subgraph
  nodes.subgraph <- igraph::subcomponent(g, concept.uuid, mode = "out")
  subgraph.names <- subgraph.uuid <- igraph::subgraph(g, nodes.subgraph)
  # get the name of the nodes from their UUID
  l.uuids <- igraph::as_ids(igraph::V(subgraph.uuid))
  for(uuid_ in l.uuids){
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
  if(disconn){
    DBI::dbDisconnect(db.con)
    if(verbose){print(paste0("disconnected from the DB"))}
  }
  d[[field]] <- subgraph.names
  field.uuid <- paste0(field, ".uuid")
  d[[field.uuid]] <- subgraph.uuid
  return(d)
}

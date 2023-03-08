#' List name of all child-concepts below certain Concept node
#'
#' @name list_child_concepts
#'
#' @description With given concept UUID (v. Reference Data Manager), find all child nodes.
#'
#' @param db.con `dbConnect` connection to database.
#' @param d hash() object (Python-like dictionary).
#' @param field field name that will be created in `d` hash() object.
#' @param concept.name concept label name (either `r.concept.name` or `db.concept.name`). This `concept.name` value coming from `ids.csv` file (see `ref_ids()`). Default, NA.
#' @param disconn if TRUE (Default), will disconnect from DB.
#' @param verbose if TRUE (Default), print messages.
#'
#' @return return two `igraph` objects listing the child-concepts below certain Concept node.
#'
#'
#' @details results are `igraph` objects. The first `igraph` object is stored under the name of the selected `field` argument of the `d` hash() object (`d` argument), for example `Disturbance Extent Type`. The `igraph` object stores the names of the parent (`field` argument) and children (e.g. `Disturbance Extent Type->Unknown`). The second `igraph` object is the same except that the names are replaced by their UUID (e.g. `41488800-6c00-30f2-b93f-785e38ab6251->f7261287-f889-31ff-b198-49733fd000f6`). The latter `igraph` will be stored in `d`, in a field formed by the concatenation of the `field` argument and `.uuid`, e.g. `Disturbance Extent Type.uuid`.
#'
#'
#' @examples
#'
#' # create a Postgres connection (replace 'xxx' by password)
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
#' d$`Disturbance Extent Type`
#' # IGRAPH 050aea5 DN-- 8 7 --
#' # + attr: name (v/c)
#' # + edges from 050aea5 (vertex names):
#' # [1] Disturbance Extent Type->Unknown          Disturbance Extent Type->1-10%
#' # [3] Disturbance Extent Type->11-30%           Disturbance Extent Type->91-100%
#' # [5] Disturbance Extent Type->61-90%           Disturbance Extent Type->No Visible/Known
#' # [7] Disturbance Extent Type->31-60%
#'
#' d$`Disturbance Extent Type.uuid`
#' # IGRAPH 050aea5 DN-- 8 7 --
#' # + attr: name (v/c)
#' # + edges from 050aea5 (vertex names):
#' # [1] 41488800-6c00-30f2-b93f-785e38ab6251->f7261287-f889-31ff-b198-49733fd000f6
#' # [2] 41488800-6c00-30f2-b93f-785e38ab6251->361c4af7-3b3f-3b4a-903d-b8a48e3cd0d6
#' # [3] 41488800-6c00-30f2-b93f-785e38ab6251->a685650c-a31a-3d88-9d5f-00c38eac8e02
#' # [4] 41488800-6c00-30f2-b93f-785e38ab6251->3d205ade-e20a-389b-9714-5f593667d0f6
#' # [5] 41488800-6c00-30f2-b93f-785e38ab6251->118ebd31-979b-3524-af9c-3ef1aa7db9f0
#' # [6] 41488800-6c00-30f2-b93f-785e38ab6251->e13a6594-a72e-31da-aa57-2f07cf0f6afc
#' # [7] 41488800-6c00-30f2-b93f-785e38ab6251->ad5b2225-f785-37c2-89b0-405d853974b8
#'
#'
#' # Cultural periods & Subcultural periods and disconnect from DB
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
#' # see the latter subgraph
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
  concept.uuid <- eamenaR::ref_ids(concept.name = concept.name,
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

# create a Postgres connection (replace 'xxx' by password)
d <- hash::hash()
my_con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
                               user = 'postgres',
                               password = 'postgis',
                               dbname = 'eamena',
                               host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
                               port = 5432)

# Disturbance Extent Type
d <- list_child_concepts(db.con = my_con,
                         d = d,
                         field = "Disturbance Extent Type",
                         concept.name = "Disturbance Extent Type",
                         disconn = F)

d <- list_child_concepts(db.con = my_con,
                         d = d,
                         field = "subcultural_periods",
                         concept.name = "Cultural Sub-Period",
                         disconn = T)


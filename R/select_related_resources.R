#' For a given Heritage Place, find all Connected Components, select the components have a given value in a certain field
#'
#' @name select_related_resources
#'
#' @description For a given Heritage Place, find all Connected Components (ex: a Built Component) having a a given value (ex: "Stable") in a certain field (ex: "Measurement Number"). This function is run after `list_related_resources()`.
#'
#' @param db.con a `dbConnect` connection to the database.
#' @param df a dataframe resultion from the `list_related_resources()` function, having the UUID of Connected Component
#' @param having a vector of values to only keep CC having these values. By default c("Stable"). These values must appear in the `ids.csv` file with their `valueid` (see `python manage.py whatisthis`).
#' @param measure the name of the field where the value of `having` is recorded. By default "Measurement Number".
#' @param disconn if TRUE (by defalut), will disconnect from the DB.
#' @param verbose if TRUE (by default), print messages.
#'
#' @return A dataframe recordind the IDs and UUIDs of the Heritage Place and Connected Components with, for the latter, the selected fields
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
#'                              id = "EAMENA-0164943",
#'                              disconn = F)
#'
#' # get the number of 'Stable' (by default)
#' df.measures <- select_related_resources(db.con = my_con,
#'                                         df = df)
#' df.measures
#' ##            hp.id                              hp.uuid             cc.id                              cc.uuid cc.type
#' ## 1 EAMENA-0164943 d4feb830-10c7-4d80-a19e-e608f424be4c COMPONENT-0000141 90400bb6-ff54-4afd-8183-65c67fa97448  Stable
#' ## Measurement.Number
#' ## 1                 30
#'
#' # get the number of 'Rooms'
#' df.measures <- select_related_resources(db.con = my_con,
#'                                         having = c("Room"),
#'                                         df = df)
#' df.measures
#' ##             hp.id                              hp.uuid             cc.id                              cc.uuid cc.type cc.measure
#' ## 1 EAMENA-0164943 d4feb830-10c7-4d80-a19e-e608f424be4c COMPONENT-0000144 28af281c-e4b9-44ac-aa98-2608581b7540    Room         28
#'
#' @export
select_related_resources <- function(db.con = NA,
                                     df = NA,
                                     having = "Stable",
                                     measure = "Measurement Number",
                                     disconn = TRUE,
                                     verbose = TRUE){
  # id = "EAMENA-0164943" ; db.con = my_con ; having = "Stable" ; relationshiptype = 'PX_is_related_to'
  # df = df[1, "hp.id"]
  # get the UUID of the having values
  hp <- df[1, "hp.id"]
  vals.uuid <- c()
  for(val in having){
    val.uuid <- eamenaR::ref_ids(concept.name = val,
                                 choice = "db.concept.uuid")
    vals.uuid <- c(vals.uuid, val.uuid)
  }
  having.df <- hash::hash()
  having.df[["values"]] <- having
  having.df[["uuid"]] <- vals.uuid
  if(verbose){
    print(paste0("The value '", having, "' has this UUID: '", vals.uuid,"'"))
  }
  uuid.cc <- df$cc.uuid
  uuids.having <- data.frame(resourceinstanceid = character())
  # UUID of BC
  bc.uuid <- eamenaR::ref_ids(concept.name = "Built Component Observation",
                              choice = "db.concept.uuid")
  # get the values of these components
  # loop to find the UUID of the connected component having the selected value
  for(cc in uuid.cc){
    # cc <- "90400bb6-ff54-4afd-8183-65c67fa97448"
    # cc <- "34cfe992-c2c0-11ea-9026-02e7594ce0a0"
    # looping through having uuid (ex: 'Stable' = "17fe354b-7df5-4f21-b21f-182612e73c8d")
    for (hav in length(having.df[["uuid"]])){
      # hav <- 1
      having.uuid <- having.df[["uuid"]][hav]
      sqll <- stringr::str_interp("
      SELECT resourceinstanceid
      FROM tiles
      WHERE resourceinstanceid::text LIKE '${cc}'
      AND tiledata ->> '${bc.uuid}' = '${having.uuid}'
                       ")
      uuid.having <- RPostgres::dbGetQuery(db.con, sqll)
      uuids.having <- rbind(uuids.having, uuid.having)
    }
  }
  uuids.having.all <- uuids.having$resourceinstanceid
  if(verbose){
    print(paste0(hp, " has ", length(uuids.having.all),
                 " Connected Component(s) with the value '", having.df[["values"]], "'")
    )
  }

  measure.uuid <- ref_ids(concept.name = measure,
                          choice = "db.concept.uuid")
  # get the number of having
  cc.havings <- data.frame(hp.id = character(),
                           hp.uuid = character(),
                           cc.id = character(),
                           cc.uuid = character(),
                           cc.type = character(),
                           cc.measure = numeric())
  for(uuids.having.one in uuids.having.all){
    # uuids.having.one <- "90400bb6-ff54-4afd-8183-65c67fa97448"
    sqll <- stringr::str_interp("
    SELECT tiledata ->> '${measure.uuid}' as total FROM tiles
    WHERE resourceinstanceid::text LIKE '${uuids.having.one}'
    AND tiledata ->> '${measure.uuid}' IS NOT NULL
                       ")
    # print(sqll)
    measurements <- RPostgres::dbGetQuery(db.con, sqll)
    if(length(measurements) == 0){
      warning(paste0("There are no '", measure, "' recorded in the Connected Component",
                     "\n The might create an error"))
    }
    # print(measurements)
    # print(df[1, "hp.id"])
    # print(df[1, "hp.uuid"])
    # print(df[df$cc.uuid == uuids.having.all, "cc.id"])
    # print(uuids.having.one)
    # print(as.numeric(measurements[[1]]))
    cc.having <- data.frame(hp.id = df[1, "hp.id"],
                            hp.uuid = df[1, "hp.uuid"],
                            cc.id = df[df$cc.uuid == uuids.having.all, "cc.id"],
                            cc.uuid = uuids.having.one,
                            cc.type = having,
                            cc.measure = as.numeric(measurements[[1]]))
    cc.havings <- rbind(cc.havings, cc.having)
  }
  if(disconn){
    DBI::dbDisconnect(db.con)
    if(verbose){print(paste0("disconnected from the DB"))}
  }
  return(cc.havings)
}

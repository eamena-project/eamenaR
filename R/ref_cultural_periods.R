#' Create a list of child-concepts below Cultural Period of all periods with their duration
#'
#' @name ref_cultural_periods
#'
#' @description create a list concepts below Cultural Period of all periods with their duration. Duration of each period are listed in the 'scopeNote' of this period (see the RDM tab in the EAMENA DB).
#'
#' @param db.con the parameters for the Postgresql EAMENA DB, in a RPostgres::dbConnect() format.
#' @param d a hash() object (a Python-like dictionary).
#' @param field the field of the hash dictionnary (`d`) that will be filled with (sub)cultural periods values, eg. "cultural_periods" or "subcultural_periods".
#' @param disconn if TRUE (by defalut), will disconnect from the DB.
#' @param verbose if TRUE (by default), print messages.
#'
#' @return NA
#'
#' @examples
#'
#' d <- hash::hash()
#' my_con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
#'                     user = 'xxx',
#'                     password = 'xxx',
#'                     dbname = 'eamena',
#'                     host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
#'                     port = 5432)
#'
#'# get cultural periods
#'d <- list_child_concepts(db.con = my_con, d = d,
#'                         field = "cultural_periods",
#'                         uuid = '3b5c9ac7-5615-3de6-9e2d-4cd7ef7460e4')
#'d <- ref_cultural_periods(db.con = my_con, d = d,
#'                          field = "cultural_periods")
#'# get subcultural periods
#'d <- list_child_concepts(db.con = my_con, d = d,
#'                         field = "subcultural_periods",
#'                         uuid = '16cb160e-7b31-4872-b2ca-6305ad311011')
#'d <- ref_cultural_periods(db.con = my_con, d = d,
#'                          field = "subcultural_periods")
#'
#' # export as TSV
#' df.periods <- rbind(d$cultural_periods, d$subcultural_periods)
#' tout <- paste0(paste0(system.file(package = "eamenaR"), "/results/"))
#' dir.create(tout, showWarnings = FALSE)
#' write.table(df.periods, paste0(tout,  "cultural_periods.tsv"),
#'             sep ="\t", row.names = F)
#'
#' @export
ref_cultural_periods <- function(db.con = NA,
                                 d = NA,
                                 field = NA,
                                 disconn = TRUE,
                                 verbose = TRUE){
  g <- d[[field]]
  leaves.names <- igraph::V(g)[igraph::degree(g,
                                              mode="out") == 0]
  leaves.names <- leaves.names$name
  g.uuid <- d[[paste0(field, ".uuid")]]
  leaves.uuid <- igraph::V(g.uuid)[igraph::degree(d[[paste0(field, ".uuid")]],
                                                  mode="out") == 0]
  leaves.uuid <- leaves.uuid$name
  df <- data.frame(ea.uuid = leaves.uuid,
                   ea.name = leaves.names,
                   ea.duration.taq = rep("", length(leaves.names)),
                   ea.duration.tpq = rep("", length(leaves.names)))
  # durations
  for(i in seq(1, length(leaves.names))){
    # i <- 1
    name <- leaves.names[i]
    uuid <- leaves.uuid[i]
    if(verbose){print(paste(i, name))}
    sqll <- stringr::str_interp("
      SELECT conceptid::text FROM values WHERE value = '${name}'
                         ")
    per.conceptid <- DBI::dbGetQuery(db.con, sqll)
    per.conceptid <- per.conceptid$conceptid
    df.name.duration <- data.frame(value = character(),
                                   valuetype = character())
    # there are two concepts for the same value, so it is needed to loop..
    for(conceptid in per.conceptid){
      sqll <- stringr::str_interp("
        SELECT value, valuetype FROM values WHERE conceptid = '${conceptid}'
                           ")
      res <- DBI::dbGetQuery(db.con, sqll)
      df.name.duration <- rbind(df.name.duration, res)
    }
    # The cultural period duration is recorded as "600 1200" in a 'scopeNote'
    culturalper.duration <- df.name.duration[df.name.duration$valuetype == 'scopeNote', "value"]
    if(length(culturalper.duration) > 0){
      # some cultural periods haven't any 'scopeNote', split on \t or [space]
      taq <- stringr::str_split(culturalper.duration, pattern = "\t| ")[[1]][1]
      tpq <- stringr::str_split(culturalper.duration, pattern = "\t| ")[[1]][2]
      if(tpq == 'Present'){
        # replaced by current year
        tpq <- format(Sys.Date(), "%Y")
      }
      # print(uuid)
      df[i, ] <- c(uuid, name, taq, tpq, "")
    } else {
      if(verbose){print(paste(" - The (sub)period",
                              name,
                              "has no 'scopeNote' (ie, no duration)"))}
    }
    # df.name <- df.name.duration[df.name.duration$valuetype == 'scopeNote', "value"]
  }
  if(disconn){
    DBI::dbDisconnect(db.con)
  }
  d[[field]] <- df
  return(d)
}

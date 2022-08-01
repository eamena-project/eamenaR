#' Create a list of child-concepts below Cultural Period of all periods with their duration
#' @name ref_cultural_periods
#' @description create a list concepts below Cultural Period of all periods
#' with their durations. Duration of each period are listed in the 'scopeNote' of this period. A periodo colum is added to the output dataframe. If 'export.table' then write a CSV file
#'
#' @return NA
#'
#' @examples
#'
#' d <- hash::hash()
#' my_con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
#'                     user = 'postgres',
#'                     password = xxx,
#'                     dbname = 'eamena',
#'                     host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
#'                     port = 5432)
#'
#' # get cultural periods
#' d <- list_concepts(db.con = my_con, d = d, field = "cultural_periods", uuid = '3b5c9ac7-5615-3de6-9e2d-4cd7ef7460e4')
#' d <- ref_cultural_periods(db.con = my_con, d = d, field = "cultural_periods")
#'
#' # get subcultural periods
#' d <- list_concepts(db.con = my_con, d = d, field = "subcultural_periods", uuid = '16cb160e-7b31-4872-b2ca-6305ad311011')
#' d <- ref_cultural_periods(db.con = my_con, d = d, field = "subcultural_periods")
#'
#' # and export as TSV
#' df.periods <- rbind(d$cultural_periods, d$subcultural_periods)
#' tout <- paste0(dirOut = paste0(system.file(package = "eamenaR"), "/extdata/"), "cultural_periods.tsv")
#' write.table(df.periods, tout, sep ="\t", row.names = F)
#'
#' # also, remember to disconnect
#' RPostgres::dbDisconnect(my_con)
#'
#' @export
ref_cultural_periods <- function(db.con = NA,
                                 d = NA,
                                 field = NA){
  g <- d[[field]]
  leaves.names <- igraph::V(g)[igraph::degree(g, mode="out") == 0]
  leaves.names <- leaves.names$name
  g.uuid <- d[[paste0(field, ".uuid")]]
  leaves.uuid <- igraph::V(g.uuid)[igraph::degree(d[[field]], mode="out") == 0]
  leaves.uuid <- leaves.uuid$name
  df <- data.frame(ea.uuid = leaves.uuid,
                   ea.name = leaves.names,
                   ea.duration.taq = rep("", length(leaves.names)),
                   ea.duration.tpq = rep("", length(leaves.names)),
                   periodo = rep("", length(leaves.names)))
  print(leaves.uuid)
  # durations
  for(i in seq(1, length(leaves.names))){
    # i <- 1
    name <- leaves.names[i]
    uuid <- leaves.uuid[i]
    print(paste(i, name))
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
      # some cultural periods haven't any 'scopeNote'
      taq <- stringr::str_split(culturalper.duration, pattern = "\t")[[1]][1]
      tpq <- stringr::str_split(culturalper.duration, pattern = "\t")[[1]][2]
      # print(uuid)
      df[i, ] <- c(uuid, name, taq, tpq, "")
    } else {
      print(paste(" - The (sub)period", name, "has no 'scopeNote' (ie, no duration)"))
    }
    # df.name <- df.name.duration[df.name.duration$valuetype == 'scopeNote', "value"]
  }
  d[[field]] <- df
  return(d)
}

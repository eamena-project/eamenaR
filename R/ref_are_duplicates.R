#' Help to identify if two HPs (pairwise) are real duplicates or not
#'
#' @name ref_are_duplicates
#'
#' @description values coming from different records will be put one against another to facilitate the comparisons between possible duplicates. A fuzzy matching between these values is computed and stored in the column 'dist' to resume the information. The distance calculation uses the `stringdist()` function from the `stringdist` package. The lower is this value, the closer are the compared values (probable duplicates).
#'
#' @param db.con the parameters for the DB, in a RPostgres::dbConnect() format. If NA (by default), will read a GeoJSON file.
#' @param d a hash() object (a Python-like dictionary).
#' @param field the field name that will be created in the a hash() object.
#' @param hp.list a list with the HP IDs to compare. By default: `c("EAMENA-0207209", "EAMENA-0182057")`
#' @param selected.fields the list of fields that will be selected to compare their values between different potential duplicates. By default the GeoJSON geometry is also selected.
#' @param dist.method `stringdist` method to calculate the pairwise distance (see the function documentation), by default `"jw"`
#' @param round.dist an integer for the number of digit to preserve in the distance computing. By default: 2.
#' @param export.table if TRUE will export the table of duplicates (FALSE by default).
#' @param dirOut the folder where the outputs will be saved. By default: '/results'. If it doesn't exist, it will be created. Only useful is export.plot.g is TRUE.
#' @param fileOut the output file name. It could be an XLSX or a CSV file. Only useful is export.plot.g is TRUE. By default "duplicates.xlsx".
#' @param verbose if TRUE (by default), print messages.
#'
#' @return a matrix stored in hash() object. This matrix has the ResourceID of the compared HP in column.  If export.table is set to TRUE it will also create an CSV or XLSX table with the potential duplicates, and the fuzzy matching value (column 'dist')
#'
#' @examples
#'
#' export as CSV in the default folder
#' d <- hash::hash()
#' d <- ref_are_duplicates(d = d,
#'                         export.table = T,
#'                         fileOut = "duplicates.csv")
#'
#' export as XLSX in another folder
#' d <- ref_are_duplicates(d = d,
#'                         export.table = T,
#'                         fileOut = "test_duplicates.xlsx",
#'                         dirOut = "C:/Rprojects/eamenaR/inst/extdata/")
#'
#' @export
ref_are_duplicates <- function(db.con = NA,
                               d = NA,
                               field = "are_duplicates",
                               hp.list = c("EAMENA-0207209",
                                           "EAMENA-0182057"),
                               selected.fields = c("Assessment Investigator - Actor",
                                                   "Assessment Activity Date",
                                                   "Resource Name"),
                               geojson.path = paste0(system.file(package = "eamenaR"),
                                                     "/extdata/caravanserail.geojson"),
                               dist.method = "jw",
                               round.dist = 2,
                               export.table = FALSE,
                               dirOut = paste0(system.file(package = "eamenaR"), "/results/"),
                               fileOut = "duplicates.xlsx",
                               verbose = TRUE){
  if(verbose){print("*check values of potential duplicates")}
  if(!is.na(geojson.path)){
    # GeoJSON
    if(verbose){print(paste0("  - read the GeoJSON file: ",
                             DescTools::SplitPath(geojson.path)$fullfilename))}
    # collect also the DB ids and resourceid
    ids <- eamenaR::ref_ids(in.value = "id")
    selected.fields <- c(ids, selected.fields, "resourceid")
    ea.geojson <- geojsonsf::geojson_sf(geojson.path)
    ea.geojson <- ea.geojson[ea.geojson[[ids]] %in% hp.list, selected.fields]
    resourceid.list <- ea.geojson$resourceid
    ea.geojson$resourceid <- NULL
    # ea.geojson$id <- hp.list
    myrownames <- colnames(ea.geojson)
    ea.geojson <- as.data.frame(t(ea.geojson))
    colnames(ea.geojson) <- resourceid.list
    # TODO: for more than 2 duplicates
    # https://stackoverflow.com/a/26408600/2854081
    df <- expand.grid(ea.geojson[, 1], ea.geojson[, 2]) # Distance matrix in long form
    names(df) <- c("a_name", "b_name")
    df$dist <- stringdist::stringdist(df$a_name,
                                      df$b_name,
                                      method = dist.method) # String edit distance (use your favorite function here)
    # Greedy assignment heuristic (Your favorite heuristic here)
    greedyAssign <- function(a, b, d){
      x <- numeric(length(a)) # assgn variable: 0 for unassigned but assignable,
      # 1 for already assigned, -1 for unassigned and unassignable
      while(any(x==0)){
        min_d <- min(d[x == 0]) # identify closest pair, arbitrarily selecting 1st if multiple pairs
        a_sel <- a[d == min_d & x == 0][1]
        b_sel <- b[d == min_d & a == a_sel & x == 0][1]
        x[a==a_sel & b == b_sel] <- 1
        x[x == 0 & (a == a_sel | b == b_sel)] <- -1
      }
      cbind(a = a[x == 1],
            b = b[x == 1],
            d = d[x == 1])
    }
    df <- data.frame(greedyAssign(as.character(df$a_name),
                                  as.character(df$b_name),
                                  round(df$dist, round.dist))
    )
    colnames(df) <- c(resourceid.list[1], resourceid.list[2], "dist")
    df$field <- myrownames
    # remove the distance between DB labels
    df[1 , "dist"] <- "-"
    # reorder columns
    df <- df[ , c(4, 1, 2, 3)]
    d[[field]] <- df
    if(export.table){
      if(DescTools::SplitPath(fileOut)$extension == "xlsx"){
        openxlsx::write.xlsx(df, paste0(dirOut, fileOut))
      }
      if(DescTools::SplitPath(fileOut)$extension == "csv"){
        write.table(df, paste0(dirOut, fileOut), row.names = F, sep = ",")
      }
      if(verbose){print(paste0("  - the '", fileOut, "' dataframe has been exorted in '", dirOut, "'"))
      }

    }
    if(verbose){print("*end of check duplicates")}
    return(d)
  }
  if(is.na(geojson.path)){
    # TODO: DB
    resourceid.list <- c()
    d <- hash::hash()
    for(hp in hp.list){
      d <- uuid_eamenaid(db.con = my_con,
                         d = d,
                         id = hp,
                         disconn = F)
      resourceid.list <- c(resourceid.list, d$uuid)
    }
    if(verbose){print(paste0("  - connect the DB"))}
    sqll <- ""
    d[[field]] <- DBI::dbGetQuery(db.con, sqll)
  }
}

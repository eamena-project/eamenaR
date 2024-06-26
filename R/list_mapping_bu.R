#' Fill an empty BU template with data from an unformatted XLSX
#'
#' @name list_mapping_bu
#'
#' @description Use a mapping file to recast the values of a source file into a format adapted to the bulk upload process (BU). This function can use, for example, \link[eamenaR]{geom_within_gs()} to test if the coordinates are within a grid cell.
#'
#' @param bu.path the path to the BU folder. The BU folder (`bu/`) is the root of job folders (ex: 'mk/', see the 'job' option description). Each job contains one to several BU worksheets. The output subfolder `out/` will be created by the function to store the output files. BU files could be either XLSX or CSV.
#' @param bu.template.path the path to the BU template. The output will be written into this structure
#' @param bu.mapping.file the path to the XLSX or Google Sheet file providing the equivalences (mapping) between the source file (unformatted) and the target file (formatted as a BU).
#' @param bu.mapping.file.ggsheet is the mapping file a Google Sheet (for example: 'https://docs.google.com/spreadsheets/d/1nXgz98mGOySgc0Q2zIeT1RvHGNl4WRq1Fp9m5qB8g8k/edit#gid=1083097625'), by default: FALSE.
#' @param job the job folder is a subfolder of `bu/`. It contains the unformatted XLSX datasets. `job` is also the name of the source fields in the mapping file. By default 'mk'. To clean the mapping file, the function will keep the `target` column, and all columns having a name starting with `mk` and followed by and underscore `mk_` and will remove all the other ones (`mk2`, `mk3`, etc.)
#' @param job.type the name of the field in the `mapping.file` XLSX giving the name of the mapping function to do:
#'   - 'field': one-to-one correspondences, the source values will be copied as it into the target file;
#'   - 'value': constant values (ie, always the same value) that will be copied into the target file;
#'   - 'expression': logical functions (mainly if statements). These functions are written directly in the appropriate cell of the mapping file;
#'   - 'escape': values depending from another column evaluated by 'expression'. This field is
#'       not read
#'   - 'other': when a column (ex: 'Seen') as a value (ex: 'Yes') that refers to several values scattered on different target columns.
#'   - 'supplem': to add supplementary rows like pipes '|' for already existing rows (ex: the alternative name of a place, two different actors)
#' @param eamena.field the name of the field in the `mapping.file` XLSX with the name of the EAMENA fields in a R format ('UNIQUEID', 'Assessment.Investigator.-.Actor', 'Investigator.Role.Type', etc.)
#' @param eamena.id the unique key identifier for a single resource, by default 'UNIQUEID'
#' @param verbose if TRUE (by default): verbose
#'
#' @return One or various XLSX files (almost) ready for an bulk upload process in the EAMENA DB. These files are names in the same way as the input file, except a `_out` suffix is added.
#'
#' @examples
#'
#' list_mapping_bu()
#'
#' list_mapping_bu(mapping.file = 'https://docs.google.com/spreadsheets/d/1nXgz98mGOySgc0Q2zIeT1RvHGNl4WRq1Fp9m5qB8g8k/edit#gid=1083097625',
#'                 mapping.file.ggsheet = T)
#'
#' @export
list_mapping_bu <- function(bu.path = paste0(system.file(package = "eamenaR"),
                                             "/extdata/bu/"),
                            bu.template.path = paste0(system.file(package = "eamenaR"),
                                                      "/extdata/bu_template.xlsx"),
                            bu.mapping.file = paste0(system.file(package = "eamenaR"),
                                                  "/extdata/bu_mapping.xlsx"),
                            bu.mapping.file.ggsheet = FALSE,
                            job = "mk",
                            job.type = "mk_type",
                            eamena.field = "target",
                            eamena.id = "UNIQUEID",
                            verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  dirOut <- paste0(bu.path, job, "/out/")
  dir.create(dirOut, showWarnings = FALSE)
  # data source
  data.path.folder <- paste0(bu.path, job)
  l.bus <- setdiff(list.files(data.path.folder),
                   list.dirs(data.path.folder,
                             recursive = FALSE, full.names = FALSE))
  l.bus.ext <- DescTools::SplitPath(l.bus)$extension
  l.bus <- l.bus[l.bus.ext[] == "xlsx"]
  if(verbose){print(paste0("* work with the job/folder: ", data.path.folder))}
  # mapping file
  if(bu.mapping.file.ggsheet){
    mapping.file <- googlesheets4::read_sheet(bu.mapping.file)
  } else {
    mapping.file <-  openxlsx::read.xlsx(bu.mapping.file)
  }
  # rm unnecessary columns
  target.column <- grep("^target$", colnames(mapping.file))
  job.column <- grep(paste0("^", job, "$"), colnames(mapping.file))
  job.type.column <- grep(paste0(job, "_type"), colnames(mapping.file))
  mapping.file <- mapping.file[ , c(target.column, job.column, job.type.column)]
  # # trim/clean strings (leading/trailing spaces)
  # mapping.file[ , job] <- trimws(mapping.file[ , job])
  cpt <- 0
  for(bu.name in l.bus){
    # bu.name <- "EAMENA_2022-09-26_03-23-29_test.csv"
    # bu.name <- "37.xlsx"
    # bu.name <- "AAA_f18_text_temp.xlsx"
    cpt <- cpt + 1
    if(verbose){print(paste0(cpt, " - read: ", bu.name))}
    data.path <- paste0(bu.path, job, "/", bu.name)
    f.extension <- DescTools::SplitPath(data.path)$extension
    if(f.extension == "xlsx"){
      data <- xlsx::read.xlsx(data.path,
                              sheetIndex = 1)
    }
    if(f.extension == "csv"){
      data <- read.csv(data.path, encoding  = "UTF-8")
      # head(data$NAME.E41)
    }
    data <- data[rowSums(is.na(data)) != ncol(data), ]
    if(verbose){print(paste0(" - nb of rows: ", nrow(data)))}
    data[is.na(data)] <- "" # rm NA value for logical tests
    # BU template for BU structure only
    bu <- openxlsx::read.xlsx(bu.template.path, startRow = 3)
    bu <- bu[0, ]
    for(i in seq(1, nrow(data))){
      bu[nrow(bu) + 1, ] <- NA
    }
    #
    #     # BU structure only
    #     mapping.file.header <- na.omit(bu.mapping.file[ , eamena.field])
    #     bu <- data.frame(matrix(ncol = length(mapping.file.header),
    #                             nrow = 0))
    #     colnames(bu) <- mapping.file.header
    #     for(i in seq(1, nrow(data))){
    #       bu[nrow(bu) + 1, ] <- NA
    #     }

    # - - - - - - - - - - - - - -
    # loops


    # 'field'
    if('field' %in% unique(mapping.file[[job.type]])){
      if(verbose){print(paste0("     works on 'field' field type ('", job.type,"' field)"))}
      mapping.file.fields <- mapping.file[mapping.file[ , job.type] == "field", ]
      mapping.file.fields <- mapping.file.fields[!is.na(mapping.file.fields[ , job.type]), ]
      if(verbose){cat("\n")}
      if(verbose){print(mapping.file.fields)}
      if(verbose){cat("\n")}
      for(i in seq(1, nrow(mapping.file.fields))){
        ea <- as.character(mapping.file.fields[i, eamena.field])
        if(verbose){print(paste0("           ... and read '", ea,"'"))}
        x <- as.character(mapping.file.fields[i, job])
        if(verbose){if(!x %in% colnames(data)){
          stop(paste0("The field name '", x, "' is not present in the file '", bu.name, "'"))}}
        bu[ , ea] <- data[ , x]
      }
    }

    # 'value'
    if('value' %in% unique(mapping.file[[job.type]])){
      if(verbose){print(paste0("     works on 'value' field type ('", job.type,"' field)"))}
      mapping.file.value <- mapping.file[mapping.file[ , job.type] == "value", ]
      mapping.file.value <- mapping.file.value[!is.na(mapping.file.value[ , job.type]), ]
      if(verbose){cat("\n")}
      if(verbose){print(mapping.file.value)}
      if(verbose){cat("\n")}
      for(i in seq(1, nrow(mapping.file.value))){
        ea <- as.character(mapping.file.value[i, eamena.field])
        if(verbose){print(paste0("           ... and read '", ea,"'"))}
        x <- as.character(mapping.file.value[i, job])
        bu[ , ea] <- x
      }
    }

    # # 'value'
    # if(verbose){print(paste0("     works on 'value' field type"))}
    # mapping.file.value <- mapping.file[mapping.file[ , job.type] == "value", ]
    # for(i in seq(1, nrow(mapping.file.value))){
    #   ea <- as.character(mapping.file.value[i, eamena.field])
    #   if(verbose){print(paste0("           ... and read '", ea,"'"))}
    #   x <- as.character(mapping.file.value[i, job])
    #   bu[ , ea] <- x
    # }

    # 'expression'
    if('expression' %in% unique(mapping.file[[job.type]])){
      if(verbose){print(paste0("     works on 'expression' field type"))}
      mapping.file.expres <- mapping.file[mapping.file[ , job.type] == "expression", ]
      mapping.file.expres <- mapping.file.expres[!is.na(mapping.file.expres[ , job.type]), ]
      if(verbose){cat("\n")}
      if(verbose){print(mapping.file.expres)}
      if(verbose){cat("\n")}
      for(i in seq(1, nrow(mapping.file.expres))){
        ea <- as.character(mapping.file.expres[i, eamena.field])
        if(verbose){print(paste0("           ... and read '", ea,"'"))}
        x.text <- as.character(mapping.file.expres[i, job])
        x.text <- gsub("[\r\n]", "\n", x.text)
        eval(parse(text = x.text)) # the XLSX cell text is executed
      }
    }
    # eamenaR::geom_within_gs(data[j, "Point"],
    #                         "https://raw.githubusercontent.com/eamena-project/eamenaR/main/inst/extdata/bu/mk/mbr.geojson",
    #                         verbose = FALSE)
    # 'other' column
    if('other' %in% unique(mapping.file[[job.type]])){
      if(verbose){print(paste0("     works on 'other' field values"))}
      mapping.file.other <- mapping.file[mapping.file[ , job.type] == "other", ]
      for(i in seq(1, nrow(mapping.file.other))){
        x.text <- as.character(mapping.file.other[i, job])
        x.text <- gsub("[\r\n]", "\n", x.text)
        # # print(colnames(data))
        # if(verbose){if(!x %in% colnames(data)){
        #   stop(paste0("The field name '", x, "' is not present in the file '", bu.name, "'"))}}
        eval(parse(text = x.text)) # the XLSX cell text is executed
      }
    }

    # 'supplem' column for supplementary rows
    if('supplem' %in% unique(mapping.file[[job.type]])){
      if(verbose){print(paste0("     works on 'supplem' field values"))}
      mapping.file.supplem <- mapping.file[mapping.file[ , job.type] == "supplem", ]
      for(i in seq(1, nrow(mapping.file.supplem))){
        x.text <- as.character(mapping.file.supplem[i, job])
        x.text <- gsub("[\r\n]", "\n", x.text)
        eval(parse(text = x.text)) # the XLSX cell text is executed
      }
    }

    # delete sur-numerary rows, ie NA
    bu <- bu[!is.na(bu[ , eamena.id]), ]
    bu <- bu[order(bu[ , eamena.id]),]
    row.names(bu) <- seq(1, nrow(bu))

    out.bu <- paste0(dirOut, DescTools::SplitPath(bu.name)$filename)
    out.bu.xlsx <- paste0(out.bu, "_out.xlsx")
    openxlsx::write.xlsx(bu, out.bu.xlsx, rowNames = F, showNA = FALSE)
    if(verbose){print(paste0(" - '", bu.name, "' exported to: ",
                             DescTools::SplitPath(out.bu.xlsx)$drive,
                             DescTools::SplitPath(out.bu.xlsx)$dirname))
      print("")
    }
  }
}

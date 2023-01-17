#' Convert rd files to HTML files using pandoc
#'
pathMan <- "C:/Rprojects/eamenaR/man/"
pathDoc <- "C:/Rprojects/eamenaR/doc/"
for(a.file in list.files(pathMan)){
  # a.file <- "geojson_analysis.Rd"
  print(paste0("*Read: ", a.file))
  file.name <- DescTools::SplitPath(a.file)$filename
  file.name.path <- paste0(pathMan, file.name)
  rdIn <- paste0(paste0(pathMan, file.name), ".rd")
  htmlOut <-  paste0(paste0(pathDoc, file.name), ".html")
  htmlRenderedOut <- paste0("https://eamena-oxford.github.io/eamenaR/doc/", file.name)
  tools::Rd2HTML(tools::parse_Rd(rdIn), out = htmlOut)
  cmd <- paste("pandoc -s -r html ", htmlOut, " -o ", rdIn, ".text", sep = "")
  system(cmd)
  # unlink(htmlOut)
  cat(paste0("      - HTML doc created!",
             "\n",
             "        + raw path: ", htmlOut,
             "\n",
             "        + rendered path: ", htmlRenderedOut,
             "\n\n"))
}



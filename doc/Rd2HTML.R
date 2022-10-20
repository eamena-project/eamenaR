#' Convert rd file to markdown using pandoc
pathMan <- "C:/Rprojects/eamenaR/man/"
pathDoc <- "C:/Rprojects/eamenaR/doc/"
for(a.file in list.files(path.Rd)){
  # a.file <- "geojson_analysis.Rd"
  print(paste0("*Read: ",a.file))
  file.name <- DescTools::SplitPath(a.file)$filename
  file.name.path <- paste0(pathMan, file.name)
  rdIn <- paste0(paste0(pathMan, file.name), ".rd")
  htmlOut <-  paste0(paste0(pathDoc, file.name), ".html")
  tools::Rd2HTML(tools::parse_Rd(rdIn), out = htmlOut)
  cmd <- paste("pandoc -s -r html ", htmlOut, " -o ", rd, ".text", sep="")
  system(cmd)
  # unlink(htmlOut)
  cat(paste0("- HTML doc created for the function '", file.name, "()'",
             "\n",
             "  + raw path: ", htmlOut,
             "\n",
             "  + rendered path: ", "XXX",
             "\n"))
}
# rd_to_markdown(a.file)

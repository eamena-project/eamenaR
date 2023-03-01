# creates an HTML table from the 'symbology.xlsx' file

library(dplyr)
library(DT)
symbology <- openxlsx::read.xlsx(paste0(system.file(package = "eamenaR"),
                                        "/extdata/symbology.xlsx"))
symbology <- symbology[ , c("list", "values", "colors")]
dt <- datatable(symbology,
                caption = "symbology.xlsx",
                rownames = FALSE,
                width = '90%',
                options = list(searching = FALSE,
                               lengthChange = FALSE,
                               paging = FALSE,
                               scrollX = F,
                               initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '20px'});}"),
                               info = FALSE,
                               filter = "none")) %>%
  formatStyle("colors",
              columns = c(2, 3),
              #fontSize = '20px',
              backgroundColor = styleEqual(symbology$colors,
                                           symbology$colors)
  )
htmlwidgets::saveWidget(dt, "C:/Rprojects/eamenaR/results/symbology.html")

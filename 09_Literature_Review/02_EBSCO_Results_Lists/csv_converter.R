library(data.table)
library(writexl)

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading Data 
delivery <- fread("./delivery.csv")

#writing it as Excel
sheets <- list("tri_comp_means" = delivery)
write_xlsx(sheets, "./delivery.xlsx")

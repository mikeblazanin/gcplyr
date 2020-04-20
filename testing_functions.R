source("./functions.R")

#Make test blockcurves data
for (i in 1:100) {
  write.csv(as.data.frame(matrix(i*(1:96), nrow = 8, byrow = T)),
            paste("./test_blockcurves_data_csv/", 
                  formatC(i, width = 3, flag = "0"), ".csv", sep = ""))
}
for (i in 1:100) {
  library(xlsx)
  write.xlsx(as.data.frame(matrix(i*(1:96), nrow = 8, byrow = T)), 
             file = paste("./test_blockcurves_data_xlsx/", 
                          formatC(i, width = 3, flag = "0"), 
                          ".xlsx", sep = ""),
             sheetName = "Sheet1", 
             col.names = TRUE, row.names = TRUE, append = FALSE)
}


my_blockcurves1 <- read_blockcurves(
  files = paste("./test_blockcurves_data_csv/",
                list.files("./test_blockcurves_data_csv/"), sep = ""),
  startrow = 2, startcol = 2, endrow = 9, endcol = 13)
my_blockcurves2 <- read_blockcurves(
  files = paste("./test_blockcurves_data_xlsx/",
                list.files("./test_blockcurves_data_xlsx/"), sep = ""),
  startrow = 2, startcol = 2, endrow = 9, endcol = 13)
my_blockcurves3 <- read_blockcurves(
  files = paste("./test_blockcurves_data_csv/",
                list.files("./test_blockcurves_data_csv/"), sep = ""))

sep_blockcurves1 <- uninterleave(my_blockcurves1, 4)
sep_blockcurves2 <- uninterleave(my_blockcurves2, 10)

wide_0 <- widen_blockcurves(blockcurves = my_blockcurves1)
wide_1 <- widen_blockcurves(blockcurves = sep_blockcurves1[[1]])

wide_2 <- import_blockcurves(paste("./test_blockcurves_data_xlsx/",
                                   list.files("./test_blockcurves_data_xlsx/"), 
                                   sep = ""),
                             startrow = 2, startcol = 2, endrow = 9, endcol = 13,
                             num_plates = 2,
                             infer_colnames = FALSE, infer_rownames = FALSE)
wide_3 <- import_blockcurves(paste("./test_blockcurves_data_xlsx/",
                                   list.files("./test_blockcurves_data_xlsx/"), 
                                   sep = ""),
                             startrow = 2, startcol = 2, endrow = 9, endcol = 13,
                             num_plates = 10,
                             infer_colnames = FALSE, infer_rownames = FALSE)

for (i in 1:length(wide_3)) {
  write.csv(wide_3[[i]], paste("./test_widecurve_csv/",
                               formatC(i, width = 2, flag = "0"),
                               ".csv", sep = ""))
}
for (i in 1:length(wide_3)) {
  write.xlsx(wide_3[[i]], paste("./test_widecurve_xlsx/",
                               formatC(i, width = 2, flag = "0"),
                               ".xlsx", sep = ""),
             sheetName = "Sheet1", 
             col.names = TRUE, row.names = TRUE, append = FALSE)
}
  
import_widecurves(paste("./test_widecurve_csv/",
                        list.files("./test_widecurve_csv/"), 
                        sep = "")
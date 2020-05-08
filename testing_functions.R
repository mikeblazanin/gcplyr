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
  startrow = 2, startcol = 2, endrow = 9, endcol = 13,
  metadata = list("type1" = c(9, 9), "type2" = c(5, 8)))
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

home <- getwd()
setwd("C:/Users/mikeb/Google Drive/Research Projects/12 Growth Curves/1_2019-07-03_MOIDensityGrowthCurve")
wide_4 <- import_blockcurves(files = list.files(), extension = "xlsx",
                             startrow = 25, startcol = 2, endrow = 32, endcol = 13,
                             infer_colnames = FALSE, infer_rownames = FALSE,
                             metadata = list("starttime" = c(21, 2),
                                             "endtime" = c(36, 2),
                                             "temp" = c(23, 2)))
setwd(home)

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
                        sep = ""))


setwd("C:/users/mikeb/Documents/code/growth-curves/test_blockdesign")
des <- read_blockmeasures(list.files())
setwd(home)


tidydes <- make_tidydesign(nrows = 8, ncols = 12,
                           block_row_names = LETTERS[1:8],
                           block_col_names = 1:12,
                Treatment = list(values = c("L", "G", "C"),
                                 2:7, 2:11, "122333"))



#Testing make_layout
dot_args <- list("Isolate" = list(c("A", "B"), 2:7, 2:11, 
                                  pattern = "1,1,2,2,0,0", TRUE))
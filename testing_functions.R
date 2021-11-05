source("./functions.R")



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

tidydes <- make_tidydesign(block_row_names = LETTERS[1:8],
                           block_col_names = 1:12,
                           Treatment = make_designpattern(values = c("A", "B"),
                                                          rows = 1:7,
                                                          cols = 3:12,
                                                          pattern = "11122220",
                                                          byrow = FALSE))

tidydes <- make_tidydesign(block_row_names = LETTERS[1:8],
                           block_col_names = 1:12,
                           Treatment = make_designpattern(values = c("A", "B"),
                                                          rows = 1:8,
                                                          cols = 1:12,
                                                          pattern = "12",
                                                          byrow = TRUE),
                           Treatment = make_designpattern(values = c("C"),
                                                          rows = 4:5,
                                                          cols = 6:7,
                                                          pattern = "1"),
                           Isol = make_designpattern(values = LETTERS[1:4],
                                                     rows = 2:7, cols = 2:11,
                                                     pattern = "111222333444"))


#Testing make_layout
dot_args <- list("Isolate" = list(c("A", "B"), 2:7, 2:11, 
                                  pattern = "1,1,2,2,0,0", TRUE))
context("Import functions")
library(testthat)
library(growth.curves.pkg)

test_that("blockcurves reads data correctly", {
  example_dfs_list <- rep(list(NA), 100)

  #Make test blockcurves data
  library(xlsx)
  dir.create("./test_blockcurves_data_csv/", showWarnings = F)
  dir.create("./test_blockcurves_data_xlsx/", showWarnings = F)
  for (i in 1:length(example_dfs_list)) {
    example_dfs_list[[i]] <- as.data.frame(matrix(i*(1:96), nrow = 8, byrow = T))
    
    write.csv(example_dfs_list[[i]],
              paste("./test_blockcurves_data_csv/", 
                    formatC(i, width = 3, flag = "0"), ".csv", sep = ""))
    write.xlsx(example_dfs_list[[i]], 
               file = paste("./test_blockcurves_data_xlsx/", 
                            formatC(i, width = 3, flag = "0"), 
                            ".xlsx", sep = ""),
               sheetName = "Sheet1", 
               col.names = TRUE, row.names = TRUE, append = FALSE)
  }
  
  my_blockcurves1 <- read_blocks(
    files = paste("./test_blockcurves_data_csv/",
                  list.files("./test_blockcurves_data_csv/"), sep = ""),
    startrow = 2, startcol = 2, endrow = 9, endcol = 13,
    metadata = list("type1" = c(9, 9), "type2" = c(5, 8)))
  my_blockcurves2 <- read_blocks(
    files = paste("./test_blockcurves_data_xlsx/",
                  list.files("./test_blockcurves_data_xlsx/"), sep = ""),
    startrow = 2, startcol = 2, endrow = 9, endcol = 13)
  my_blockcurves3 <- read_blocks(
    files = paste("./test_blockcurves_data_csv/",
                  list.files("./test_blockcurves_data_csv/"), sep = ""))
  
  #TODO:
  #expect_identical(my_blockcurves1[[1]]$data, example_dfs_list[[1]]
  
  
  #Cases to test:
  #infer colnames True & startrow specified
  #                    & startrow not specified
  #infer rownames True & startcol specified
  #                    & startcol not specified
  
  
  
          
})








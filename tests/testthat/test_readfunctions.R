library(testthat)
library(gcplyr)

test_that("infer_names works as expected", {
  #Infer but no open cell
  expect_equal(
    infer_names(df = data.frame(matrix(1:16, nrow = 4)),
                startrow = NA, startcol = NA, endrow = NA, endcol = NA,
                header = NA, sider = NA),
    list(startrow = 1, startcol = 1, endrow = 4, endcol = 4,
         rownames_col = NA, colnames_row = NA))
  
  #Infer open cell
  expect_equal(
    infer_names(df = data.frame(matrix(c("", 2:16), nrow = 4)),
                startrow = NA, startcol = NA, endrow = NA, endcol = NA,
                header = NA, sider = NA),
    list(startrow = 2, startcol = 2, endrow = 4, endcol = 4,
         rownames_col = 1, colnames_row = 1))
  
  #Infer both, only start row
  expect_equal(
    infer_names(df = data.frame(matrix(c(1:16), nrow = 4)),
                startrow = 2, startcol = NA, endrow = NA, endcol = NA,
                header = NA, sider = NA),
    list(startrow = 2, startcol = 1, endrow = 4, endcol = 4,
         rownames_col = NA, colnames_row = NA))
  expect_equal(
    infer_names(df = data.frame(matrix(c("", 2:16), nrow = 4)),
                startrow = 1, startcol = NA, endrow = NA, endcol = NA,
                header = NA, sider = NA),
    list(startrow = 2, startcol = 2, endrow = 4, endcol = 4,
         rownames_col = 1, colnames_row = 1))
  
  #Infer both, only start col
  expect_equal(
    infer_names(df = data.frame(matrix(c(1:16), nrow = 4)),
                startrow = NA, startcol = 2, endrow = NA, endcol = NA,
                header = NA, sider = NA),
    list(startrow = 1, startcol = 2, endrow = 4, endcol = 4,
         rownames_col = NA, colnames_row = NA))
  
  #No infer, no start info
  expect_equal(
    infer_names(df = data.frame(matrix(c("", 2:16), nrow = 4)),
                startrow = NA, startcol = NA, endrow = NA, endcol = NA,
                header = FALSE, sider = FALSE),
    list(startrow = 1, startcol = 1, endrow = 4, endcol = 4,
         rownames_col = NA, colnames_row = NA))
  
  #No infer, only start col
  expect_equal(
    infer_names(df = data.frame(matrix(c("", 2:16), nrow = 4)),
                startrow = NA, startcol = 2, endrow = NA, endcol = NA,
                header = FALSE, sider = FALSE),
    list(startrow = 1, startcol = 2, endrow = 4, endcol = 4,
         rownames_col = NA, colnames_row = NA))
  
  #No infer, only start row
  expect_equal(
    infer_names(df = data.frame(matrix(c("", 2:16), nrow = 4)),
                startrow = 2, startcol = NA, endrow = NA, endcol = NA,
                header = FALSE, sider = FALSE),
    list(startrow = 2, startcol = 1, endrow = 4, endcol = 4,
         rownames_col = NA, colnames_row = NA))
})

test_that("parse_filestrings works correctly", {
  expect_equal(
    parse_filestrings("./test/test2/test3.csv",
                      keep_dot = TRUE, keep_path = TRUE, keep_ext = TRUE),
    "./test/test2/test3.csv")
  expect_equal(
    parse_filestrings("./test/test2/test3.csv",
                      keep_dot = FALSE, keep_path = TRUE, keep_ext = TRUE),
    "test/test2/test3.csv")
  expect_equal(
    parse_filestrings("./test/test2/test3.csv",
                      keep_dot = TRUE, keep_path = FALSE, keep_ext = TRUE),
    "./test3.csv")
  expect_equal(
    parse_filestrings("./test/test2/test3.csv",
                      keep_dot = TRUE, keep_path = TRUE, keep_ext = FALSE),
    "./test/test2/test3")
  expect_equal(
    parse_filestrings("./test/test2/test3.csv",
                      keep_dot = FALSE, keep_path = FALSE, keep_ext = TRUE),
    "test3.csv")
  expect_equal(
    parse_filestrings("./test/test2/test3.csv",
                      keep_dot = FALSE, keep_path = TRUE, keep_ext = FALSE),
    "test/test2/test3")
  expect_equal(
    parse_filestrings("./test/test2/test3.csv",
                      keep_dot = TRUE, keep_path = FALSE, keep_ext = FALSE),
    "./test3")
  expect_equal(
    parse_filestrings("./test/test2/test3.csv",
                      keep_dot = FALSE, keep_path = FALSE, keep_ext = FALSE),
    "test3")
})

test_that("read_gcfile reads xlsx files correctly", {
  setwd(tempdir())
  #Only run the Excel tests if xlsx loads successfully
  run_xlsx <- tryCatch(
    {library(xlsx)
      TRUE},
    error = function(e) {return(FALSE)},
    warning = function(w) {return(TRUE)}
  )
  if(run_xlsx) {
    testdf <- data.frame(matrix(LETTERS[1:25], nrow = 5, ncol = 5))
    testdf[1, ] <- as.character(NA)
    testdf[, 1] <- as.character(NA)
    testdf[1:4, 5] <- as.character(NA)
    testdf[5, 1:4] <- as.character(NA)
    colnames(testdf) <- paste0("...", 1:5)
    row.names(testdf) <- 1:5
    xlsx::write.xlsx(testdf,
                     file = "testdf.xlsx",
                     sheetName = "Sheet1",
                     showNA = FALSE, append = FALSE,
                     col.names = FALSE, row.names = FALSE)
    
    expect_equal(read_gcfile(file = "testdf.xlsx", filetype = "xlsx",
                             na.strings = c("NA", "")), 
                 testdf)
  }
})


test_that("read_blocks reads data correctly", {
  #Make test blockcurves data
  
  #Only run the Excel tests if xlsx loads successfully
  run_xlsx <- tryCatch(
    {library(xlsx)
     TRUE},
    error = function(e) {return(FALSE)},
    warning = function(w) {return(TRUE)}
  )
  
  setwd(tempdir())
  dir.create("./test_blockcurves_data_csv/", showWarnings = F)
  if(run_xlsx) {dir.create("./test_blockcurves_data_xlsx/", showWarnings = F)}
  
  example_dfs_list <- rep(list(NA), 5)
  for (i in 1:length(example_dfs_list)) {
    example_dfs_list[[i]] <- as.data.frame(matrix(as.character(i*(1:96)), 
                                                  nrow = 8, byrow = T))
    
    write.csv(example_dfs_list[[i]],
              paste("./test_blockcurves_data_csv/",
                    formatC(i, width = 3, flag = "0"), ".csv", sep = ""),
              row.names = TRUE)
    if(run_xlsx) {
      xlsx::write.xlsx(example_dfs_list[[i]],
                 file = paste("./test_blockcurves_data_xlsx/",
                              formatC(i, width = 3, flag = "0"),
                              ".xlsx", sep = ""),
                 sheetName = "Sheet1",
                 col.names = TRUE, row.names = TRUE, append = FALSE)
    }
  }
  
  #Read csv with all rows/cols specified, metadata included
  my_blockcurves1 <- read_blocks(
    files = paste("./test_blockcurves_data_csv/",
                  list.files("./test_blockcurves_data_csv/"), sep = ""),
    startrow = 1, startcol = 1, endrow = 9, endcol = "M",
    metadata = list("type1" = c(9, 9), "type2" = c(5, 8)))
  
  my_blockcurves1_expected <- rep(list(NA), length(example_dfs_list))
  for (i in 1:length(my_blockcurves1_expected)) {
    my_blockcurves1_expected[[i]] <- 
      list(data = example_dfs_list[[i]],
           metadata = c(block_name = paste("test_blockcurves_data_csv/",
                                             formatC(i, width = 3, flag = "0"),
                                             sep = ""),
                        type1 = example_dfs_list[[i]][8, 8],
                        type2 = example_dfs_list[[i]][4, 7]))
    row.names(my_blockcurves1_expected[[i]]$data) <-
      as.character(row.names(my_blockcurves1_expected[[i]]$data))
  }
  expect_equal(my_blockcurves1, my_blockcurves1_expected)
  
  my_blockcurves1_excelnumbers <- read_blocks(
    files = paste("./test_blockcurves_data_csv/",
                  list.files("./test_blockcurves_data_csv/"), sep = ""),
    startrow = 1, startcol = 1, endrow = 9, endcol = "M",
    metadata = list("type1" = c(9, "I"), "type2" = c("E", 8)))
  expect_equal(my_blockcurves1_excelnumbers, my_blockcurves1_expected)
  
  #Reading only one column of data
  my_blockcurves1_narrow <- read_blocks(
    files = paste("./test_blockcurves_data_csv/",
                  list.files("./test_blockcurves_data_csv/"), sep = ""),
    startrow = 1, startcol = 1, endrow = 9, endcol = 2)
  
  my_blockcurves1_narrow_expected <- rep(list(NA), length(example_dfs_list))
  for (i in 1:length(my_blockcurves1_narrow_expected)) {
    my_blockcurves1_narrow_expected[[i]] <- 
      list(data = example_dfs_list[[i]][1:8, 1, drop = FALSE],
           metadata = c(block_name = paste("test_blockcurves_data_csv/",
                                           formatC(i, width = 3, flag = "0"),
                                           sep = "")))
    row.names(my_blockcurves1_narrow_expected[[i]]$data) <-
      as.character(row.names(my_blockcurves1_narrow_expected[[i]]$data))
  }
  expect_equal(my_blockcurves1_narrow, my_blockcurves1_narrow_expected)
  
  #Read xlsx with all rows/cols specified, metadata included
  if(run_xlsx) {
    my_blockcurves2 <- read_blocks(
      files = paste("./test_blockcurves_data_xlsx/",
                    list.files("./test_blockcurves_data_xlsx/"), sep = ""),
      startrow = 1, startcol = 1, endrow = 9, endcol = 13,
      metadata = list("type1" = c(9, 9), "type2" = c(5, 8)))
    my_blockcurves2_expected <- rep(list(NA), length(example_dfs_list))
    for (i in 1:length(my_blockcurves2_expected)) {
      my_blockcurves2_expected[[i]] <- 
        list(data = example_dfs_list[[i]],
             metadata = c(block_name = paste("test_blockcurves_data_xlsx/",
                                             formatC(i, width = 3, flag = "0"),
                                             sep = ""),
                          type1 = example_dfs_list[[i]][8, 8],
                          type2 = example_dfs_list[[i]][4, 7]))
      row.names(my_blockcurves2_expected[[i]]$data) <-
        as.character(row.names(my_blockcurves2_expected[[i]]$data))
    }
    expect_equal(my_blockcurves2, my_blockcurves2_expected)
    
    my_blockcurves2_excel <- read_blocks(
      files = paste("./test_blockcurves_data_xlsx/",
                    list.files("./test_blockcurves_data_xlsx/"), sep = ""),
      startrow = 1, startcol = 1, endrow = 9, endcol = 13,
      metadata = list("type1" = c("I", 9), "type2" = c(5, "H")))
    expect_equal(my_blockcurves2_excel, my_blockcurves2_expected)
    
    #Reading only one column of data
    my_blockcurves2_narrow <- read_blocks(
      files = paste("./test_blockcurves_data_xlsx/",
                    list.files("./test_blockcurves_data_xlsx/"), sep = ""),
      startrow = 1, startcol = 1, endrow = 9, endcol = 2)
    my_blockcurves2_narrow_expected <- rep(list(NA), length(example_dfs_list))
    for (i in 1:length(my_blockcurves2_narrow_expected)) {
      my_blockcurves2_narrow_expected[[i]] <- 
        list(data = example_dfs_list[[i]][1:8, 1, drop = FALSE],
             metadata = c(block_name = paste("test_blockcurves_data_xlsx/",
                                             formatC(i, width = 3, flag = "0"),
                                             sep = "")))
      row.names(my_blockcurves2_narrow_expected[[i]]$data) <-
        as.character(row.names(my_blockcurves2_narrow_expected[[i]]$data))
    }
    expect_equal(my_blockcurves2_narrow, my_blockcurves2_narrow_expected)
    
    unlink("./test_blockcurves_data_xlsx", recursive = TRUE)
  }
  
  unlink("./test_blockcurves_data_csv", recursive = TRUE)
})

test_that("read_blocks reads data correctly for multiple blocks in one file", {
  #Make test blockcurves data
  setwd(tempdir())
  dir.create("./test_blockcurves_data_csv/", showWarnings = F)

  example_dfs_list <- rep(list(NA), 3)
  for (i in 1:length(example_dfs_list)) {
    example_dfs_list[[i]] <- as.data.frame(matrix(as.character(i*(1:96)), 
                                                  nrow = 8, byrow = T))
  }
  
  output <- 
    rbind(c(NA, colnames(example_dfs_list[[1]])),
          cbind(row.names(example_dfs_list[[1]]),
                example_dfs_list[[1]]),
          rep(NA, 13))

  for (i in 2:length(example_dfs_list)) {
    temp <- rbind(c(NA, colnames(example_dfs_list[[i]])),
                        cbind(row.names(example_dfs_list[[i]]),
                              example_dfs_list[[i]]),
                  rep(NA, 13))
    colnames(temp) <- colnames(output)
    
    output <- rbind(output, temp)
  }
  write.table(output, "./test_multblocks_onefile.csv", sep = ",",
              row.names = FALSE, col.names = FALSE, na = "")
                  
  #Read csv with all rows/cols specified, metadata included
  my_blockcurves1 <- read_blocks(
    files = "./test_multblocks_onefile.csv",
    startrow = c(1, 11, 21), startcol = 1, endrow = c(9, 19, 29), endcol = "M",
    metadata = list("type1" = list(c(3, 13, 23), c("B", "B", "B")),
                    "type2" = c(5, 8)))
  
  my_blockcurves1_expected <- rep(list(NA), length(example_dfs_list))
  for (i in 1:length(my_blockcurves1_expected)) {
    my_blockcurves1_expected[[i]] <- 
      list(data = example_dfs_list[[i]],
           metadata = c(block_name = "test_multblocks_onefile",
                        type1 = example_dfs_list[[i]][2, 1],
                        type2 = example_dfs_list[[1]][4, 7]))
    row.names(my_blockcurves1_expected[[i]]$data) <-
      as.character(row.names(my_blockcurves1_expected[[i]]$data))
  }
  expect_equal(my_blockcurves1, my_blockcurves1_expected)
  
  unlink("./test_blockcurves_data_csv", recursive = TRUE)
})

test_that("read_wides works correctly", {
  #Make test data
  
  #Only run the Excel tests if xlsx loads successfully
  run_xlsx <- tryCatch(
    {library(xlsx)
      TRUE},
    error = function(e) {return(FALSE)},
    warning = function(w) {return(TRUE)}
  )
  
  setwd(tempdir())
  dir.create("./test_widecurves_data/", showWarnings = F)

  set.seed(1)
  data <- data.frame("time" = as.character(1:100),
                     "dens1" = as.character(1/(1+exp(-.1*((1:100) - 50))) + 
                       rnorm(100, sd = 0.5)),
                     "dens2" = as.character(2/(1+exp(-.1*((1:100) - 50))) + 
                       rnorm(100, sd = 0.5)),
                     "dens3" = as.character(3/(1+exp(-.1*((1:100) - 50))) + 
                       rnorm(100, sd = 0.5)),
                     "dens4" = as.character(4/(1+exp(-.1*((1:100) - 50))) + 
                       rnorm(100, sd = 0.5)))
  row.names(data) <- 2:101
  write.csv(data, "./test_widecurves_data/test.csv", row.names = FALSE)
  if(run_xlsx) {
    xlsx::write.xlsx(data, "./test_widecurves_data/test.xlsx",
               sheetName = "Sheet1", col.names = TRUE, row.names = FALSE, 
               append = FALSE)
  }
  
  #No names to col, no other settings specified
  if(!run_xlsx) {
    data_in <- read_wides(
      files = c("./test_widecurves_data/test.csv"),
      names_to_col = NULL)
    expect_equal(data_in,
                 data)
  } else {
    data_in <- read_wides(
      files = c("./test_widecurves_data/test.csv",
                "./test_widecurves_data/test.xlsx"),
      names_to_col = NULL)
    expect_equal(data_in,
                 list("test_widecurves_data/test" = data,
                      "test_widecurves_data/test" = data))
  }
  
  #Only reading one column of data
  if(!run_xlsx) {
    data_in_narrow <- read_wides(
      files = c("./test_widecurves_data/test.csv"),
      endcol = 1, run_names_header = NULL)
    expect_equal(data_in_narrow,
                 data[, 1, drop = FALSE])
  } else {
    data_in_narrow <- read_wides(
      files = c("./test_widecurves_data/test.csv",
                "./test_widecurves_data/test.xlsx"),
      endcol = 1, run_names_header = NULL)
    expect_equal(data_in_narrow,
                 list("test_widecurves_data/test" = data[, 1, drop = FALSE],
                      "test_widecurves_data/test" = data[, 1, drop = FALSE]))
  }
  
  data2 <- cbind(data.frame(file = rep("test_widecurves_data/test", nrow(data))),
                 data)
  if(!run_xlsx) {
    data_in2 <- read_wides(
      files = c("./test_widecurves_data/test.csv"),
      names_to_col = "file")
    expect_equal(data_in2,
                 data2)
  } else {
    data_in2 <- read_wides(
      files = c("./test_widecurves_data/test.csv",
                "./test_widecurves_data/test.xlsx"),
      names_to_col = "file")
    expect_equal(data_in2,
                 list("test_widecurves_data/test" = data2,
                      "test_widecurves_data/test" = data2))
  }
  
  data3 <- cbind(file = data2[, 1],
                 #The row-col numbers are off bc of headers 
                 data.frame("row5col5" = rep(data[4, 5], nrow(data2)),
                            "row12col2" = rep(data[11, 2], nrow(data2))),
                 data2[, -1])
  if(!run_xlsx) {
    data_in3 <- read_wides(
      files = c("./test_widecurves_data/test.csv"),
      metadata = list("row5col5" = c(5, 5), "row12col2" = c(12, 2)))
    expect_equal(data_in3, data3)
    
    data_in3_excelnums <- read_wides(
      files = c("./test_widecurves_data/test.csv"),
      metadata = list("row5col5" = c(5, "E"), "row12col2" = c(12, "B")))
    expect_equal(data_in3_excelnums,
                 data3)
  } else {
    data_in3 <- read_wides(
      files = c("./test_widecurves_data/test.csv",
                "./test_widecurves_data/test.xlsx"),
      metadata = list("row5col5" = c(5, 5), "row12col2" = c(12, 2)))
    expect_equal(data_in3,
                 list("test_widecurves_data/test" = data3,
                      "test_widecurves_data/test" = data3))
    
    data_in3_excelnums <- read_wides(
      files = c("./test_widecurves_data/test.csv",
                "./test_widecurves_data/test.xlsx"),
      metadata = list("row5col5" = c(5, "E"), "row12col2" = c(12, "B")))
    expect_equal(data_in3_excelnums,
                 list("test_widecurves_data/test" = data3,
                      "test_widecurves_data/test" = data3))
  }
  
  unlink("./test_widecurves_data", recursive = TRUE)
})

test_that("read_wides works correctly with multiple from one file", {
  #Make test data

  setwd(tempdir())

  set.seed(1)
  data <- data.frame("time" = as.character(1:100),
                     "dens1" = as.character(1/(1+exp(-.1*((1:100) - 50))) + 
                                              rnorm(100, sd = 0.5)),
                     "dens2" = as.character(2/(1+exp(-.1*((1:100) - 50))) + 
                                              rnorm(100, sd = 0.5)),
                     "dens3" = as.character(3/(1+exp(-.1*((1:100) - 50))) + 
                                              rnorm(100, sd = 0.5)),
                     "dens4" = as.character(4/(1+exp(-.1*((1:100) - 50))) + 
                                              rnorm(100, sd = 0.5)))
  row.names(data) <- 2:101
  
  temp <- rbind(data,
                rep(NA, ncol(data)),
                colnames(data),
                data)
  write.csv(temp, "test_multwideonefile.csv", row.names = FALSE, na = "")
  
  data_in <- read_wides(
    files = "test_multwideonefile.csv",
    startrow = c(1, 103), endrow = c(101, 203),
    metadata = list("row5colE" = c(5, "E"), 
                    "row12col2" = list(c(13, 114), c("B", "B"))))
  
  temp1 <- cbind("file" = "test_multwideonefile",
                 "row5colE" = "-0.152008845676564",
                 "row12col2" = "0.216802889141846",
                 data)
  temp2 <- cbind("file" = "test_multwideonefile",
                 "row5colE" = "-0.152008845676564",
                 "row12col2" = "0.775730889959501",
                 data)
  row.names(temp2) <- 104:203
  expect_equal(data_in,
               list("test_multwideonefile" = temp1,
                 "test_multwideonefile" = temp2))
})

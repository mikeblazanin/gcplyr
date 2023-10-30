library(testthat)
library(gcplyr)

test_that("make_design issues errors when expected for invalid pattern strings", {
  expect_error(
    make_design(nrows = 8, ncols = 12, lookup_tbl_start = "A",
                    "design_elem" = list(c("test1", "test2", "test3"),
                                         2:4,
                                         5:7,
                                         "123")),
    regexp = "Some values in pattern are not in lookup table. Check that you 
             have lookup_tbl_start correct and that you're only using 
             alphanumeric values")
  expect_error(
    make_design(nrows = 8, ncols = 12, pattern_split = ",",
                    "design_elem" = list(c("test1", "test2", "test3"),
                                         2:4,
                                         5:7,
                                         "1,2,aa")),
    regexp = "Pattern values are multi-character after splitting, but not all pattern values are numeric")
  
  expect_error(
    make_design(nrows = 8, ncols = 12, pattern_split = ",",
                "design_elem" = list(c("test1", "test2", "test3"),
                                     7:9,
                                     5:7,
                                     "1,2,3")),
    regexp = "has rows or columns out of range")
  
  expect_error(
    make_design(nrows = 8, ncols = 12, pattern_split = ",",
                "design_elem" = list(c("test1", "test2", "test3"),
                                     2:4,
                                     11:13,
                                     "1,2,3")),
    regexp = "has rows or columns out of range")
})

test_that("make_design works as expected for numerical pattern strings", {
  expect_equal(
    make_design(wellnames_sep = "",
                nrows = 4, ncols = 4, colnames_first = TRUE,
                    "name" = list(c(1, "a", "hello"),
                                  2:3, 2:4,
                                  pattern = "122333")),
    expected = data.frame(
      Well = paste(rep(1:4, each = 4),
                   rep(LETTERS[1:4], 4), sep = ""),
      name = c(NA, NA, NA, NA,
               NA, "1", "hello", NA,
               NA, "a", "hello", NA,
               NA, "a", "hello", NA))
  )
})

test_that("make_design works as expected for patterns that are vectors", {
  expect_equal(
    make_design(wellnames_sep = "",
                nrows = 4, ncols = 4, colnames_first = TRUE,
                "name" = list(c(1, "a", "hello"),
                              2:3, 2:4,
                              pattern = c(1, 2, 2, 3, 3, 3))),
    expected = data.frame(
      Well = paste(rep(1:4, each = 4),
                   rep(LETTERS[1:4], 4), sep = ""),
      name = c(NA, NA, NA, NA,
               NA, "1", "hello", NA,
               NA, "a", "hello", NA,
               NA, "a", "hello", NA))
  )
})

test_that("make_design works with 0's in numerical pattern string", {
  expect_equal(
    make_design(wellnames_sep = "",
                nrows = 4, ncols = 4, colnames_first = TRUE,
                  "name" = list(c(1, "a", "hello"),
                                2:3, 2:4,
                                pattern = "122033")),
    expected = data.frame(
      Well = paste(rep(1:4, each = 4),
                   rep(LETTERS[1:4], 4), sep = ""),
      name = c(NA, NA, NA, NA,
               NA, "1", NA, NA,
               NA, "a", "hello", NA,
               NA, "a", "hello", NA))
    )
})

test_that("make_design works for multiple design elements", {
  expect_equal(
    make_design(wellnames_sep = "", nrows = 4, ncols = 4, colnames_first = TRUE,
                    "name" = list(c(1, "a", "hello"), 
                                  2:3, 2:4,
                                  pattern = "122033"),
                    "name2" = list(c(1, "a", "hello"),
                                  2:3, 2:4,
                                  pattern = "233011")),
    expected = data.frame(Well = paste(rep(1:4, each = 4),
                                       rep(LETTERS[1:4], 4), sep = ""),
                          name = c(NA, NA, NA, NA,
                                   NA, "1", NA, NA,
                                   NA, "a", "hello", NA,
                                   NA, "a", "hello", NA),
                          name2 = c(NA, NA, NA, NA, 
                                    NA, "a", NA, NA,
                                    NA, "hello", 1, NA,
                                    NA, "hello", 1, NA))
  )
})

test_that("make_design works as expected for alpha pattern strings", {
  expect_equal(
    make_design(wellnames_sep = "",
                nrows = 4, ncols = 4, colnames_first = TRUE,
                    lookup_tbl_start = "a",
                    "name" = list(c(1, "a", "hello"),
                                  2:3, 2:4,
                                  pattern = "abbccc")),
    expected = data.frame(
      Well = paste(rep(1:4, each = 4),
                   rep(LETTERS[1:4], 4), sep = ""),
      name = c(NA, NA, NA, NA,
               NA, "1", "hello", NA,
               NA, "a", "hello", NA,
               NA, "a", "hello", NA))
  )
})

test_that("write_blocks works for output_format = 'single'", {
  example_dfs_list <- rep(list(NA), 4)
  for (i in 1:length(example_dfs_list)) {
    example_dfs_list[[i]] <- 
      as.data.frame(matrix(as.character(i*(1:96)), nrow = 8, byrow = T))
  }
  
  my_blockcurves_allmetad <- rep(list(NA), length(example_dfs_list))
  for (i in 1:length(my_blockcurves_allmetad)) {
    my_blockcurves_allmetad[[i]] <- 
      list(data = example_dfs_list[[i]],
           metadata = c(block_name = paste(formatC(i, width = 3, flag = "0"),
                                           sep = ""),
                        type1 = example_dfs_list[[i]][8, 8],
                        type2 = example_dfs_list[[i]][4, 7]))
  }
  
  my_blockcurves_namesonly <- rep(list(NA), length(example_dfs_list))
  for (i in 1:length(my_blockcurves_namesonly)) {
    my_blockcurves_namesonly[[i]] <- 
      list(data = example_dfs_list[[i]],
           metadata = c(block_name = paste(formatC(i, width = 3, flag = "0"),
                                           sep = "")))
  }
  
  #For block_name_location = "file"
  fil <- tempfile(fileext = ".csv")
  write_blocks(blocks = my_blockcurves_allmetad, file = fil,
               output_format = "single",
               block_name_location = "file")
  
  fil <- tempfile(fileext = ".csv")
  write_blocks(blocks = my_blockcurves_namesonly, file = fil,
               output_format = "single",
               block_name_location = "file")
  
  #For block_name_location = "filename"
  fil <- tempfile(fileext = ".csv")
  expect_warning(
    write_blocks(blocks = my_blockcurves_allmetad, file = fil,
               output_format = "single",
               block_name_location = "filename"))
  
  fil <- tempfile(fileext = ".csv")
  expect_warning(
    write_blocks(blocks = my_blockcurves_namesonly, file = fil,
               output_format = "single",
               block_name_location = "filename"))
})

test_that("write_blocks works for output_format = 'pasted'", {
  example_dfs_list <- rep(list(NA), 4)
  for (i in 1:length(example_dfs_list)) {
    example_dfs_list[[i]] <- 
      as.data.frame(matrix(as.character(i*(1:96)), nrow = 8, byrow = T))
  }
  
  my_blockcurves_allmetad <- rep(list(NA), length(example_dfs_list))
  for (i in 1:length(my_blockcurves_allmetad)) {
    my_blockcurves_allmetad[[i]] <- 
      list(data = example_dfs_list[[i]],
           metadata = c(block_name = paste(formatC(i, width = 3, flag = "0"),
                                           sep = ""),
                        type1 = example_dfs_list[[i]][8, 8],
                        type2 = example_dfs_list[[i]][4, 7]))
  }
  
  my_blockcurves_namesonly <- rep(list(NA), length(example_dfs_list))
  for (i in 1:length(my_blockcurves_namesonly)) {
    my_blockcurves_namesonly[[i]] <- 
      list(data = example_dfs_list[[i]],
           metadata = c(block_name = paste(formatC(i, width = 3, flag = "0"),
                                           sep = "")))
  }
  
  #For block_name_location = "file"
  fil <- tempfile(fileext = ".csv")
  expect_no_error(write_blocks(blocks = my_blockcurves_allmetad, file = fil,
               output_format = "pasted",
               block_name_location = "file"))
  
  fil <- tempfile(fileext = ".csv")
  expect_no_error(write_blocks(blocks = my_blockcurves_namesonly, file = fil,
               output_format = "pasted",
               block_name_location = "file"))
  
  #For block_name_location = "filename"
  fil <- tempfile(fileext = ".csv")
  expect_warning(write_blocks(blocks = my_blockcurves_allmetad, file = fil,
                 output_format = "pasted",
                 block_name_location = "filename"))
  
  fil <- tempfile(fileext = ".csv")
  expect_no_error(write_blocks(blocks = my_blockcurves_namesonly, file = fil,
                 output_format = "pasted",
                 block_name_location = "filename"))
})

test_that("write_blocks works for output_format = 'multiple'", {
  example_dfs_list <- rep(list(NA), 4)
  for (i in 1:length(example_dfs_list)) {
    example_dfs_list[[i]] <- 
      as.data.frame(matrix(as.character(i*(1:96)), nrow = 8, byrow = T))
  }
  
  my_blockcurves_allmetad <- rep(list(NA), length(example_dfs_list))
  for (i in 1:length(my_blockcurves_allmetad)) {
    my_blockcurves_allmetad[[i]] <- 
      list(data = example_dfs_list[[i]],
           metadata = c(block_name = paste(formatC(i, width = 3, flag = "0"),
                                           sep = ""),
                        type1 = example_dfs_list[[i]][8, 8],
                        type2 = example_dfs_list[[i]][4, 7]))
  }
  
  my_blockcurves_namesonly <- rep(list(NA), length(example_dfs_list))
  for (i in 1:length(my_blockcurves_namesonly)) {
    my_blockcurves_namesonly[[i]] <- 
      list(data = example_dfs_list[[i]],
           metadata = c(block_name = paste(formatC(i, width = 3, flag = "0"),
                                           sep = "")))
  }
  
  #For block_name_location = "file"
  fil <- tempfile(fileext = ".csv")
  expect_warning(write_blocks(blocks = my_blockcurves_allmetad, file = fil,
               output_format = "multiple",
               block_name_location = "file"))
  
  fil <- tempfile(fileext = ".csv")
  expect_warning(write_blocks(blocks = my_blockcurves_namesonly, file = fil,
               output_format = "multiple",
               block_name_location = "file"))
  
  #For block_name_location = "filename"
  fil <- tempfile(fileext = ".csv")
  expect_message(write_blocks(blocks = my_blockcurves_allmetad, file = fil,
               output_format = "multiple",
               block_name_location = "filename"))
  
  fil <- tempfile(fileext = ".csv")
  expect_no_error(write_blocks(blocks = my_blockcurves_namesonly, file = fil,
               output_format = "multiple",
               block_name_location = "filename"))
})
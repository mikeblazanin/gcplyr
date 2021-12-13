context("Reshaping functions")
library(testthat)
library(growth.curves.pkg)

test_that("widen_blocks works on a single dataframe", {
  example_dfs_list <- rep(list(NA), 100)
  for (i in 1:length(example_dfs_list)) {
    example_dfs_list[[i]] <- as.data.frame(matrix(as.character(i*(1:96)), 
                                                  nrow = 8, byrow = T))
  }
  
  #Test colnames_first = FALSE
  expected_df <- 
    data.frame(matrix(as.character(1:96), nrow = 1,
                      dimnames = list(NULL,
                                      paste(as.character(rep(1:8, each = 12)),
                                            rep(paste("V", 1:12, sep = ""), 8),
                                            sep = "_"))))
  for (i in 1:ncol(expected_df)) {
    colnames(expected_df)[i] <- 
      as.character(substr(colnames(expected_df)[i], 
                          2, nchar(colnames(expected_df)[i])))
  }
  expect_equal(
    widen_blocks(example_dfs_list[[1]], colnames_first = FALSE),
    expected_df)
  
  #Test colnames_first = TRUE
  expected_df2 <- 
    data.frame(matrix(as.character(rep(1:12, each = 8) + (12 * 0:7)), nrow = 1,
                      dimnames = list(NULL,
                                      paste(rep(paste("V", 1:12, sep = ""), each = 8),
                                            as.character(rep(1:8, 12)),
                                            sep = "_"))))
  expect_equal(
    widen_blocks(example_dfs_list[[1]], colnames_first = TRUE),
    expected_df2)
})

##TODO fix this test too!
test_that("widen_blocks works on a list of dataframes", {
  #With no metadata
  example_dfs_list <- rep(list(NA), 100)
  for (i in 1:length(example_dfs_list)) {
    example_dfs_list[[i]] <- as.data.frame(matrix(as.character(i*(1:96)), 
                                                  nrow = 8, byrow = T))
  }
  wide2_expected <- data.frame(matrix(NA, nrow = 100, ncol = 96,
                                      dimnames = list(NULL,
                                                      paste(rep(paste("V", 1:12, sep = ""), 8),
                                                            rep(1:8, each = 12), 
                                                            sep = "_"))))
  for (i in 1:nrow(wide2_expected)) {
    for (j in 1:ncol(wide2_expected)) {
      wide2_expected[i, j] <- as.character(as.numeric(i)*as.numeric(j))
    }
  }
  expect_equal(
    widen_blocks(example_dfs_list, colnames_first = TRUE),
    wide2_expected)
  
  #With metadata
  example_dfs_list2 <- rep(list(NA), 100)
  for (i in 1:length(example_dfs_list2)) {
    example_dfs_list2[[i]] <- 
      list("data" = as.data.frame(matrix(as.character(i*(1:96)), 
                                         nrow = 8, byrow = T)),
           "metadata" = c("temp1" = i*15, "temp2" = i*30))
  }
  #read blocks with metadata in row2 col 3, row 3 col 6
})


test_that("Pivot_longer works on single dataframe", {
  data <- data.frame("time" = 1:100,
                     "Pop1" = 10/(1+exp(-.1*((1:100) - 50))) + 
                       rnorm(100, sd = 0.5),
                     "Pop2" = 20/(1+exp(-.1*((1:100) - 50))) + 
                       rnorm(100, sd = 0.5))
  data_lng <- pivot_wide_longer(data,
                    data_cols = c("Pop1", "Pop2"))
  data_lng <- data_lng[order(data_lng$Well, data_lng$time), ]
  row.names(data_lng) <- 1:200
  expect_equal(data_lng,
               expected = data.frame(time = c(1:100, 1:100),
                                     Well = c(rep("Pop1", 100), rep("Pop2", 100)),
                                     Measurements = c(data$Pop1, data$Pop2)))
})

test_that("Pivot_longer works on list of dataframes", {
  data_lst <- list("df1" = data.frame("time" = 1:100,
                     "Pop1" = 10/(1+exp(-.1*((1:100) - 50))) +
                       rnorm(100, sd = 0.5),
                     "Pop2" = 20/(1+exp(-.1*((1:100) - 50))) +
                       rnorm(100, sd = 0.5)),
                   "df2" = data.frame("time" = 1:100,
                              "Pop3" = 10/(1+exp(-.1*((1:100) - 50))) +
                                rnorm(100, sd = 0.5),
                              "Pop4" = 20/(1+exp(-.1*((1:100) - 50))) +
                                rnorm(100, sd = 0.5)))
  data_lst_lng <- pivot_wide_longer(data_lst,
                                id_cols = "time")
  for (i in 1:length(data_lst_lng)) {
    data_lst_lng[[i]] <- data_lst_lng[[i]][order(data_lst_lng[[i]]$Well, 
                                                 data_lst_lng[[i]]$time), ]
    row.names(data_lst_lng[[i]]) <- 1:nrow(data_lst_lng[[i]])
  }
  expect_equal(data_lst_lng,
               expected = 
                 list(
                   "df1" = data.frame(time = c(1:100, 1:100),
                                     Well = c(rep("Pop1", 100), rep("Pop2", 100)),
                                     Measurements = c(data_lst[[1]]$Pop1, 
                                                      data_lst[[1]]$Pop2)),
                   "df2" = data.frame(time = c(1:100, 1:100),
                              Well = c(rep("Pop3", 100), rep("Pop4", 100)),
                              Measurements = c(data_lst[[2]]$Pop3, 
                                               data_lst[[2]]$Pop4))))
})

test_that("Merge function collapses lists of dfs correctly", {
  data_lst <- list("df1" = data.frame("time" = 1:100,
                                      "Pop1" = 10/(1+exp(-.1*((1:100) - 50))) +
                                        rnorm(100, sd = 0.5)),
                   "df2" = data.frame("time" = 1:100,
                                      "Pop1" = 20/(1+exp(-.1*((1:100) - 50))) +
                                        rnorm(100, sd = 0.5)))
  expect_equal(merge_dfs(data_lst, collapse = TRUE, names_to = "run"),
               data.frame(time = c(1:100, 1:100),
                          Pop1 = c(data_lst[[1]]$Pop1, data_lst[[2]]$Pop1),
                          run = rep(c("df1", "df2"), each = 100)))
  
  data_lst2 <- list("df1" = data.frame("time" = 1:100,
                                      "fake_treat" = LETTERS[1:100]),
                   "df2" = data.frame("time" = 1:100,
                                      "fake_treat" = letters[1:100]))
  expect_equal(merge_dfs(data_lst, data_lst2, collapse = TRUE, names_to = "run"),
               data.frame(time = c(1:100, 1:100),
                          Pop1 = c(data_lst[[1]]$Pop1, data_lst[[2]]$Pop1),
                          run = rep(c("df1", "df2"), each = 100),
                          fake_treat = c(LETTERS, rep(NA, 74),
                                         letters, rep(NA, 74))))
})

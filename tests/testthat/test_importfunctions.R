library(testthat)
library(gcplyr)

test_that("import_blockdesigns works correctly", {
  setwd(tempdir())
  
  #Single file, single design
  dat1 <- matrix(paste0("str", 1:8), nrow = 8, ncol = 12)
  colnames(dat1) <- 1:12
  row.names(dat1) <- LETTERS[1:8]
  write.csv(dat1, "strain.csv")
  expect_equal(
    import_blockdesigns("strain.csv"),
    data.frame(Well = paste0(rep(LETTERS[1:8], each = 12), 1:12),
               strain = rep(paste0("str", 1:8), each = 12)))
  
  #Single file, pasted designs
  dat2 <- matrix(paste0("str", 1:8), nrow = 8, ncol = 12)
  dat2 <- matrix(paste0(dat2, "_", LETTERS[1:4]), nrow = 8, ncol = 12)
  colnames(dat2) <- 1:12
  row.names(dat2) <- LETTERS[1:8]
  write.csv(dat2, "strain_rep.csv")
  expect_equal(
    import_blockdesigns("strain_rep.csv"),
    data.frame(Well = paste0(rep(LETTERS[1:8], each = 12), 1:12),
               strain_rep = rep(paste0("str", 1:8, "_", LETTERS[1:4]), each = 12)))
  expect_equal(
    import_blockdesigns("strain_rep.csv", sep = "_"),
    data.frame(Well = paste0(rep(LETTERS[1:8], each = 12), 1:12),
               strain = rep(paste0("str", 1:8), each = 12),
               rep = rep(LETTERS[1:4], each = 12)))
  
  #Multiple files, ea w/ single design
  dat3 <- matrix(paste0("str", 1:8), nrow = 8, ncol = 12)
  dat4 <- matrix(LETTERS[1:4], nrow = 8, ncol = 12)
  colnames(dat3) <- 1:12
  row.names(dat3) <- LETTERS[1:8]
  colnames(dat4) <- 1:12
  row.names(dat4) <- LETTERS[1:8]
  write.csv(dat3, "strain.csv")
  write.csv(dat4, "rep.csv")
  #If you want them as one plate (columns)
  expect_equal(
    import_blockdesigns(c("strain.csv", "rep.csv")),
    data.frame(Well = paste0(rep(LETTERS[1:8], each = 12), 1:12),
               strain = rep(paste0("str", 1:8), each = 12),
               rep = rep(LETTERS[1:4], each = 12)))
  #If you want them as separate plates (rows)
  expect_equal(
    import_blockdesigns(c("strain.csv", "rep.csv"), join_designs = FALSE),
    data.frame(block_name = rep(c("strain", "rep"), each = 96),
               Well = rep(paste0(rep(LETTERS[1:8], each = 12), 1:12), 2),
               Designs = c(rep(paste0("str", 1:8), each = 12),
                           rep(LETTERS[1:4], each = 12, times = 2))))
  
  #Single file, multiple designs in it but they're not pasted
  dat3 <- cbind(c("", LETTERS[1:8]), rbind(c(1:12), dat3))
  dat4 <- cbind(c("", LETTERS[1:8]), rbind(c(1:12), dat4))
  dat_3_4 <- rbind(dat3, matrix("", nrow = 1, ncol = 13), dat4)
  write.table(dat_3_4, "strain_and_rep.csv", row.names = FALSE,
            col.names = FALSE, sep = ",")
  #If you want them together as one plate (columns)
  expect_equal(
    import_blockdesigns("strain_and_rep.csv",
                        startrow = c(1, 11), endrow = c(9, 19),
                        block_names = c("strain", "rep")),
    data.frame(Well = paste0(rep(LETTERS[1:8], each = 12), 1:12),
               strain = rep(paste0("str", 1:8), each = 12),
               rep = rep(LETTERS[1:4], each = 12)))
  #If you want them as separate plates (rows)
  expect_equal(
    import_blockdesigns("strain_and_rep.csv",
                        startrow = c(1, 11), endrow = c(9, 19),
                        join_designs = FALSE),
    data.frame(block_name = "strain_and_rep",
               Well = rep(paste0(rep(LETTERS[1:8], each = 12), 1:12), 2),
               Designs = c(rep(paste0("str", 1:8), each = 12),
                           rep(LETTERS[1:4], each = 12, times = 2))))
  
  #Multiple files, ea w/ pasted designs
  dat5 <- matrix(paste0("str", 1:8), nrow = 8, ncol = 12)
  dat5 <- matrix(paste0(dat5, "_", LETTERS[1:4]), nrow = 8, ncol = 12)
  colnames(dat5) <- 1:12
  row.names(dat5) <- LETTERS[1:8]
  write.csv(dat5, "strain_rep.csv")
  dat6 <- matrix(paste0("bact", 1:8), nrow = 8, ncol = 12)
  dat6 <- matrix(paste0(dat6, "_", LETTERS[5:8]), nrow = 8, ncol = 12)
  colnames(dat6) <- 1:12
  row.names(dat6) <- LETTERS[1:8]
  write.csv(dat6, "bact_rep2.csv")
  
  #If you want them as one plate (columns), but don't specify sep
  expect_equal(
    import_blockdesigns(c("strain_rep.csv", "bact_rep2.csv")),
    data.frame(Well = paste0(rep(LETTERS[1:8], each = 12), 1:12),
               strain_rep = rep(paste0("str", 1:8, "_", LETTERS[1:4]), each = 12),
               bact_rep2 = rep(paste0("bact", 1:8, "_", LETTERS[5:8]), each = 12)))
  #If you want them as one plate (columns) and specify sep
  expect_equal(
    import_blockdesigns(c("strain_rep.csv", "bact_rep2.csv"), sep = "_"),
    data.frame(Well = paste0(rep(LETTERS[1:8], each = 12), 1:12),
               strain = rep(paste0("str", 1:8), each = 12),
               rep = rep(LETTERS[1:4], each = 12),
               bact = rep(paste0("bact", 1:8), each = 12),
               rep2 = rep(LETTERS[5:8], each = 12)))
  #If you don't want them as one plate (rows), but don't specify sep
  expect_equal(
    import_blockdesigns(c("strain_rep.csv", "bact_rep2.csv"), 
                        join_designs = FALSE),
    data.frame(block_name = rep(c("strain_rep", "bact_rep2"), each = 96),
               Well = rep(paste0(rep(LETTERS[1:8], each = 12), 1:12), 2),
               Designs = c(t(dat5), t(dat6))))
  #If you don't want them as one plate (rows), but specify sep
  expect_equal(
    import_blockdesigns(c("strain_rep.csv", "bact_rep2.csv"),
                      join_as_cols = FALSE, sep = "_", into = c("A", "B")),
    data.frame(block_name = rep(c("strain_rep", "bact_rep2"), each = 96),
               Well = rep(paste0(rep(LETTERS[1:8], each = 12), 1:12), 2),
               A = c(rep(paste0("str", 1:8), each = 12),
                     rep(paste0("bact", 1:8), each = 12)),
               B = c(rep(LETTERS[1:4], each = 12, times = 2),
                     rep(LETTERS[5:8], each = 12, times = 2))))
})

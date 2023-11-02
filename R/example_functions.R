#' Create R objects or files as seen in vignette examples
#' 
#' This function makes it easy to generate R objects or files that are
#' created in the vignette examples. Note that this function should not
#' be counted on to produce the same output across different versions of
#' \code{gcplyr}, as it will be frequently changed to match the examples
#' in the vignettes.
#' 
#' @param vignette Number of the vignette the example object or file is created in.
#' @param example Number of the example the object or file is created in.
#' @param dir The directory files should be saved into.
#' 
#' @return An R object, or the names of the files if files have been written
#' 
#' @export
make_example <- function(vignette, example, dir = ".") {
  #Make sure dir ends in /
  if(substr(dir, nchar(dir), nchar(dir)) != "/") {dir <- paste0(dir, "/")}
  if(!dir.exists(dir)) {dir.create(dir)}
  
  if(vignette == 1) { 
    # Vignette 1 ----
    
    if(example == 1) { 
      ## Example 1 ----
      write.csv(example_widedata_noiseless, file = paste0(dir, "widedata.csv"), 
                row.names = FALSE)
      message("Files have been written")
      return(paste0(dir, "widedata.csv"))
    
    } else if (example == 2) { 
      ## Example 2 ----
      #Simple design written to file
      example_design <- make_design(
        output_format = "blocks",
        pattern_split = ",", nrows = 8, ncols = 12,
        "Bacteria_strain" = make_designpattern(
          values = paste("Strain", 1:48),
          rows = 1:8, cols = 1:6,
          pattern = 1:48,
          byrow = TRUE),
        "Bacteria_strain" = make_designpattern(
          values = paste("Strain", 1:48),
          rows = 1:8, cols = 7:12,
          pattern = 1:48,
          byrow = TRUE),
        "Phage" = make_designpattern(
          values = c("No Phage"),
          rows = 1:8, cols = 1:6,
          pattern = "1"),
        "Phage" = make_designpattern(
          values = c("Phage Added"),
          rows = 1:8, cols = 7:12,
          pattern = "1"))
      write_blocks(example_design, file = NULL, dir = dir)
      message("Files have been written")
      return(paste0(dir, c("Bacteria_strain.csv", "Phage.csv")))
    }
    
  
  } else if (vignette == 2) {
    # Vignette 2 ----
    
    if(example == 1) {
      ## Example 1 ----
      temp_filenames <- 
        paste0(dir, "Plate1-", 
              paste(example_widedata_noiseless$Time %/% 3600,
                    formatC((example_widedata_noiseless$Time %% 3600) %/% 60, 
                            width = 2, flag = 0),
                    formatC((example_widedata_noiseless$Time %% 3600) %% 60,
                            width = 2, flag = 0),
                    sep = "_"), ".csv")
      for (i in 1:length(temp_filenames)) {
        write.table(
          cbind(
            matrix(c("", "", "", "", "A", "B", "C", "D", "E", "F", "G", "H"), 
                   nrow = 12),
            rbind(rep("", 12),
                  matrix(c("Time", example_widedata_noiseless$Time[i], rep("", 10)), 
                         ncol = 12),
                  rep("", 12),
                  matrix(1:12, ncol = 12),
                  matrix(
                    example_widedata_noiseless[i, 2:ncol(example_widedata_noiseless)],
                    ncol = 12))
          ), 
          file = temp_filenames[i], quote = FALSE, row.names = FALSE, sep = ",",
          col.names = FALSE)
      }
      message("Files have been written")
      return(temp_filenames)
    
    } else if (example == 2) {
      ## Example 2 ----
      #Re-run example 1 basically, bc example 2 need to use that
      blocknames <- 
        paste0("Plate1-", 
               paste(example_widedata_noiseless$Time %/% 3600,
                     formatC((example_widedata_noiseless$Time %% 3600) %/% 60, 
                             width = 2, flag = 0),
                     formatC((example_widedata_noiseless$Time %% 3600) %% 60,
                             width = 2, flag = 0),
                     sep = "_"))
      blocks <- rep(list(list(data = NA, metadata = NA)), length(temp_filenames))
      for (i in 1:length(blocknames)) {
        blocks[[i]]$data <- 
          as.data.frame(matrix(
            as.vector(mode = "character",
                      example_widedata_noiseless[i, 2:ncol(example_widedata_noiseless)]),
            ncol = 12, dimnames = list(LETTERS[1:8], as.character(1:12))))
        blocks[[i]]$metadata <-
          c(block_name = blocknames[i], 
            time = as.character(example_widedata_noiseless$Time[i]))
      }
      # This code just creates an example file with multiple blocks
      write_blocks(blocks,
                   file = "blocks_single.csv", dir = dir,
                   output_format = "single",
                   block_name_location = "file")
      message("Files have been written")
      return(paste0(dir, "blocks_single.csv"))
      
    } else if (example == 3) {
      ## Example 3 ----
      # This code just creates a wide-shaped example file where the data doesn't
      # start on the first row.
      temp_example_widedata <- example_widedata_noiseless
      colnames(temp_example_widedata) <- paste("V", 1:ncol(temp_example_widedata),
                                               sep = "")
      modified_example_widedata <-
        rbind(
          as.data.frame(matrix("", nrow = 4, ncol = ncol(example_widedata_noiseless))),
          colnames(example_widedata_noiseless),
          temp_example_widedata)
      modified_example_widedata[1:2, 1:2] <- 
        c("Experiment name", "Start date", "Experiment_1", as.character(Sys.Date()))
      
      write.table(modified_example_widedata, file = paste0(dir, "widedata.csv"), 
                  row.names = FALSE, col.names = FALSE, sep = ",")
      message("Files have been written")
      return(paste0(dir, "widedata.csv"))
    }
  
    
  } else if (vignette == 3) {
    # Vignette 3 ----
    
    if(example == 1) {
      ## Example 1 ----
      write.csv(
        file = paste0(dir, "mydesign.csv"),
        x = matrix(rep(c("Tr1", "Tr2"), each = 48),
                   nrow = 8, ncol = 12, dimnames = list(LETTERS[1:8], 1:12)))
      message("Files have been written")
      return(paste0(dir, "mydesign.csv"))
      
    } else if (example == 2) {
      ## Example 2 ----
      write.csv(
        file = paste0(dir, "mydesign2.csv"),
        x = matrix(rep(c("StrA", "StrB", "StrC", "StrD"), each = 24),
                   nrow = 8, ncol = 12, dimnames = list(LETTERS[1:8], 1:12),
                   byrow = TRUE))
      message("Files have been written")
      return(paste0(dir, "mydesign2.csv"))
    }
  
  } else if (vignette == 4) {
    # Vignette 4 ----
    
    if (example == 1) {
      ## Example 1 ----
      example_tidydata <- trans_wide_to_tidy(example_widedata_noiseless,
                                             id_cols = "Time")
      example_design <- make_design(
        pattern_split = ",", nrows = 8, ncols = 12,
        "Bacteria_strain" = make_designpattern(
          values = paste("Strain", 1:48),
          rows = 1:8, cols = 1:6, pattern = 1:48, byrow = TRUE),
        "Bacteria_strain" = make_designpattern(
          values = paste("Strain", 1:48),
          rows = 1:8, cols = 7:12, pattern = 1:48, byrow = TRUE),
        "Phage" = make_designpattern(
          values = c("No Phage"), rows = 1:8, cols = 1:6, pattern = "1"),
        "Phage" = make_designpattern(
          values = c("Phage Added"), rows = 1:8, cols = 7:12, pattern = "1"))
      ex_dat_mrg <- merge_dfs(example_tidydata, example_design)
      
      ex_dat_mrg$Time <-
        paste(ex_dat_mrg$Time %/% 3600,
              formatC((ex_dat_mrg$Time %% 3600) %/% 60, 
                      width = 2, flag = 0),
              formatC((ex_dat_mrg$Time %% 3600) %% 60,
                      width = 2, flag = 0),
              sep = ":")
      
      return(ex_dat_mrg)
    }
  
  } else if (vignette == 7) {
    # Vignette 7 ----
    
    if(example == 1) {
      ## Example 1 ----
      # This is the data we've been working with previously
      noiseless_data <- 
        trans_wide_to_tidy(example_widedata_noiseless, id_cols = "Time")
      # This is the same data but with simulated noise added
      noisy_data <- trans_wide_to_tidy(example_widedata, id_cols = "Time")
      # We'll add some identifiers and then merge them together
      noiseless_data <- mutate(noiseless_data, noise = "No")
      noisy_data <- mutate(noisy_data, noise = "Yes")
      ex_dat_mrg <- merge_dfs(noisy_data, noiseless_data)
      ex_dat_mrg <- merge_dfs(ex_dat_mrg, example_design)
      
      ex_dat_mrg$Well <- 
        factor(ex_dat_mrg$Well,
               levels = paste(rep(LETTERS[1:8], each = 12), 1:12, sep = ""))
      ex_dat_mrg$Time <- ex_dat_mrg$Time/3600 #Convert time to hours
      
      # For computational speed, let's just keep the wells we'll be focusing on
      #  (for your own analyses, you should skip this step and continue using
      #  all of your data)
      ex_dat_mrg <- dplyr::filter(ex_dat_mrg, Well %in% sample_wells)
      
      return(ex_dat_mrg)
    }
    
  
    
  } else if (vignette == 9) {
    # Vignette 9 ----
    
    if(example == 1) {
      ## Example 1 ----
      #block-shaped files for multiple plates easily separable
      temp_filenames1 <- 
        paste0(dir, "Plate1-", 
              paste(example_widedata_noiseless$Time %/% 3600,
                    formatC((example_widedata_noiseless$Time %% 3600) %/% 60, 
                            width = 2, flag = 0),
                    formatC((example_widedata_noiseless$Time %% 3600) %% 60,
                            width = 2, flag = 0),
                    sep = "_"), ".csv")
      temp_filenames2 <- 
        paste0(dir, "Plate2-", 
              paste(example_widedata_noiseless$Time %/% 3600,
                    formatC((example_widedata_noiseless$Time %% 3600) %/% 60, 
                            width = 2, flag = 0),
                    formatC((example_widedata_noiseless$Time %% 3600) %% 60,
                            width = 2, flag = 0),
                    sep = "_"), ".csv")
      for (i in 1:length(temp_filenames1)) {
        write.table(
          cbind(
            matrix(c("", "", "", "", "A", "B", "C", "D", "E", "F", "G", "H"), 
                   nrow = 12),
            rbind(rep("", 12),
                  matrix(c("Time", example_widedata_noiseless$Time[i], rep("", 10)), 
                         ncol = 12),
                  rep("", 12),
                  matrix(1:12, ncol = 12),
                  matrix(
                    example_widedata_noiseless[i, 2:ncol(example_widedata_noiseless)],
                    ncol = 12))
          ), 
          file = temp_filenames1[i], quote = FALSE, row.names = FALSE, sep = ",",
          col.names = FALSE)
      }
      for (i in 1:length(temp_filenames2)) {
        write.table(
          cbind(
            matrix(c("", "", "", "", "A", "B", "C", "D", "E", "F", "G", "H"), 
                   nrow = 12),
            rbind(rep("", 12),
                  matrix(c("Time", example_widedata_noiseless$Time[i], rep("", 10)), 
                         ncol = 12),
                  rep("", 12),
                  matrix(1:12, ncol = 12),
                  matrix(
                    example_widedata_noiseless[i, 2:ncol(example_widedata_noiseless)],
                    ncol = 12))
          ), 
          file = temp_filenames2[i], quote = FALSE, row.names = FALSE, sep = ",",
          col.names = FALSE)
      }
      message("Files have been written")
      return(invisible(NULL))
      
    } else if (example == 2) {
      ## Example 2 ----
      #Interleaved block-shaped files
      times <- c(example_widedata_noiseless$Time, example_widedata_noiseless$Time + 1)
      times <- times[order(times)]
      
      temp_filenames <-
        paste(dir,
          paste(formatC(times %/% 3600, width = 2, flag = 0),
                formatC((times %% 3600) %/% 60, width = 2, flag = 0),
                formatC((times %% 3600) %% 60, width = 2, flag = 0),
                sep = "_"), ".csv", sep = "")
      
      for (i in 1:length(temp_filenames)) {
        write.table(
          cbind(
            matrix(c("", "", "", "", "A", "B", "C", "D", "E", "F", "G", "H"), 
                   nrow = 12),
            rbind(rep("", 12),
                  matrix(c("Time", times[i], rep("", 10)), 
                         ncol = 12),
                  rep("", 12),
                  matrix(1:12, ncol = 12),
                  matrix(
                    example_widedata_noiseless[i, 2:ncol(example_widedata_noiseless)],
                    ncol = 12))
          ), 
          file = temp_filenames[i], quote = FALSE, row.names = FALSE, sep = ",",
          col.names = FALSE)
      }
      message("Files have been written")
      return(invisible(NULL))
    }
  }
  stop("Your vignette-example combination is not a valid selection")
}

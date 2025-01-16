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
#' @importFrom rlang .data
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
      utils::write.csv(gcplyr::example_widedata_noiseless, file = paste0(dir, "widedata.csv"), 
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
          rows = 1:8, cols = 1:6, pattern = 1:48, byrow = TRUE),
        "Bacteria_strain" = make_designpattern(
          values = paste("Strain", 1:48),
          rows = 1:8, cols = 7:12, pattern = 1:48, byrow = TRUE),
        "Phage" = make_designpattern(
          values = c("No Phage"), rows = 1:8, cols = 1:6, pattern = "1"),
        "Phage" = make_designpattern(
          values = c("Phage Added"), rows = 1:8, cols = 7:12, pattern = "1"))
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
              paste(gcplyr::example_widedata_noiseless$Time %/% 3600,
                    formatC((gcplyr::example_widedata_noiseless$Time %% 3600) %/% 60, 
                            width = 2, flag = 0),
                    formatC((gcplyr::example_widedata_noiseless$Time %% 3600) %% 60,
                            width = 2, flag = 0),
                    sep = "_"), ".csv")
      for (i in 1:length(temp_filenames)) {
        utils::write.table(
          cbind(
            matrix(c("", "", "", "", "A", "B", "C", "D", "E", "F", "G", "H"), 
                   nrow = 12),
            rbind(rep("", 12),
                  matrix(c("Time", gcplyr::example_widedata_noiseless$Time[i], rep("", 10)), 
                         ncol = 12),
                  rep("", 12),
                  matrix(1:12, ncol = 12),
                  matrix(
                    gcplyr::example_widedata_noiseless[i, 2:ncol(gcplyr::example_widedata_noiseless)],
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
               paste(gcplyr::example_widedata_noiseless$Time %/% 3600,
                     formatC((gcplyr::example_widedata_noiseless$Time %% 3600) %/% 60, 
                             width = 2, flag = 0),
                     formatC((gcplyr::example_widedata_noiseless$Time %% 3600) %% 60,
                             width = 2, flag = 0),
                     sep = "_"))
      blocks <- rep(list(list(data = NA, metadata = NA)), length(blocknames))
      for (i in 1:length(blocknames)) {
        blocks[[i]]$data <- 
          as.data.frame(matrix(
            as.vector(mode = "character",
                      gcplyr::example_widedata_noiseless[i, 2:ncol(gcplyr::example_widedata_noiseless)]),
            ncol = 12, dimnames = list(LETTERS[1:8], as.character(1:12))))
        blocks[[i]]$metadata <-
          c(block_name = blocknames[i], 
            time = as.character(gcplyr::example_widedata_noiseless$Time[i]))
      }
      # This code just creates an example file with multiple blocks
      write_blocks(blocks,
                   file = paste0(dir, "blocks_single.csv"),
                   output_format = "single",
                   block_name_location = "file")
      message("Files have been written")
      return(paste0(dir, "blocks_single.csv"))
      
    } else if (example == 3) {
      ## Example 3 ----
      # This code just creates a wide-shaped example file where the data doesn't
      # start on the first row.
      temp_example_widedata <- gcplyr::example_widedata_noiseless
      colnames(temp_example_widedata) <- paste("V", 1:ncol(temp_example_widedata),
                                               sep = "")
      modified_example_widedata <-
        rbind(
          as.data.frame(matrix("", nrow = 4, ncol = ncol(gcplyr::example_widedata_noiseless))),
          colnames(gcplyr::example_widedata_noiseless),
          temp_example_widedata)
      modified_example_widedata[1:2, 1:2] <- 
        c("Experiment name", "Start date", "Experiment_1", as.character(Sys.Date()))
      
      utils::write.table(modified_example_widedata, file = paste0(dir, "widedata.csv"), 
                  row.names = FALSE, col.names = FALSE, sep = ",")
      message("Files have been written")
      return(paste0(dir, "widedata.csv"))
    }
  
    
  } else if (vignette == 3) {
    # Vignette 3 ----
    
    if(example == 1) {
      ## Example 1 ----
      utils::write.csv(
        file = paste0(dir, "mydesign.csv"),
        x = matrix(rep(c("Tr1", "Tr2"), each = 48),
                   nrow = 8, ncol = 12, dimnames = list(LETTERS[1:8], 1:12)))
      message("Files have been written")
      return(paste0(dir, "mydesign.csv"))
      
    } else if (example == 2) {
      ## Example 2 ----
      utils::write.csv(
        file = paste0(dir, "mydesign2.csv"),
        x = matrix(rep(c("StrA", "StrB", "StrC", "StrD"), each = 24),
                   nrow = 8, ncol = 12, dimnames = list(LETTERS[1:8], 1:12),
                   byrow = TRUE))
      message("Files have been written")
      return(paste0(dir, "mydesign2.csv"))
    } else if (example == 3) {
      ## Example 3 ----
      utils::write.csv(
        file = paste0(dir, "mydesign_sep.csv"),
        x = rbind(matrix(rep(c("Tr1", "Tr2"), each = 48),
                         nrow = 8, ncol = 12, dimnames = list(LETTERS[1:8], 1:12)),
                  matrix(data = "", nrow = 1, ncol = 12,
                         dimnames = list("", rep("", 12))),
                  matrix(data = 1:12, nrow = 1, ncol = 12,
                         dimnames = list("", rep("", 12))),
                  matrix(rep(c("StrA", "StrB", "StrC", "StrD"), each = 24),
                         nrow = 8, ncol = 12, dimnames = list(LETTERS[1:8], 1:12),
                         byrow = TRUE)))
      message("Files have been written")
      return(paste0(dir, "mydesign_sep.csv"))
    } else if (example == 4) {
      ## Example 3 ----
      pt1 <- matrix(rep(c("Tr1", "Tr2"), each = 48),
                    nrow = 8, ncol = 12, dimnames = list(LETTERS[1:8], 1:12))
      pt2 <- matrix(rep(c("StrA", "StrB", "StrC", "StrD"), each = 24),
                    nrow = 8, ncol = 12, dimnames = list(LETTERS[1:8], 1:12),
                    byrow = TRUE)
      utils::write.csv(
        file = paste0(dir, "mydesign_pasted.csv"),
        x = matrix(paste(pt1, pt2, sep = "_"),
                   nrow = 8, ncol = 12, dimnames = list(LETTERS[1:8], 1:12)))
      message("Files have been written")
      return(paste0(dir, "mydesign_pasted.csv"))
    }
  
  } else if (vignette == 4) {
    # Vignette 4 ----
    
    if (example == 1) {
      ## Example 1 ----
      example_tidydata <- trans_wide_to_tidy(gcplyr::example_widedata_noiseless,
                                             id_cols = "Time")
      ex_dat_mrg <- merge_dfs(example_tidydata,
                              gcplyr::example_design_tidy)
      
      ex_dat_mrg$Time <-
        paste(ex_dat_mrg$Time %/% 3600,
              formatC((ex_dat_mrg$Time %% 3600) %/% 60, 
                      width = 2, flag = 0),
              formatC((ex_dat_mrg$Time %% 3600) %% 60,
                      width = 2, flag = 0),
              sep = ":")
      
      return(ex_dat_mrg)
    } else if (example == 2) {
      ## Example 2 ----
      example_tidydata <- trans_wide_to_tidy(gcplyr::example_widedata_noiseless,
                                             id_cols = "Time")
      set.seed(1)
      ex_dat_mrg <- mutate(
        example_tidydata,
        Measurements = ifelse(Well == "A1",
                              round(runif(length(Measurements), 0.198, 0.202), 3), 
                              Measurements + 0.2),
        Well_type = ifelse(Well == "A1", "Blank", "Non-blank"))
      ex_dat_mrg <- ex_dat_mrg[order(ex_dat_mrg$Well, decreasing = TRUE), ]
      return(ex_dat_mrg)
    } else if (example == 3) {
      ## Example 3 ----
      example_tidydata <- trans_wide_to_tidy(gcplyr::example_widedata_noiseless,
                                             id_cols = "Time")
      ex_dat_mrg <- merge_dfs(example_tidydata,
                              make_design(nrows = 8, ncols = 12,
                                          Media = mdp("Media_1", 1:2, 1:12),
                                          Media = mdp("Media_2", 3:4, 1:12),
                                          Media = mdp("Media_3", 5:6, 1:12),
                                          Media = mdp("Media_4", 7:8, 1:12)))
      set.seed(1)
      ex_dat_mrg <- mutate(
        ex_dat_mrg,
        Measurements = ifelse(Well %in% c("A1", "C1", "E1", "G1"), 
                              round(runif(length(Measurements), -0.002, 0.002), 3),
                              Measurements),
        Measurements = case_when(Media == "Media_1" ~ Measurements + 0.2,
                                 Media == "Media_2" ~ Measurements + 0.25,
                                 Media == "Media_3" ~ Measurements + 0.1,
                                 Media == "Media_4" ~ Measurements + 0.15,
                                 .default = Measurements),
        Well_type = ifelse(Well %in% c("A1", "C1", "E1", "G1"), 
                           "Blank", "Non-blank"))
      ex_dat_mrg <- ex_dat_mrg[order(ex_dat_mrg$Well, decreasing = TRUE), ]
      return(ex_dat_mrg)
    }
    
  } else if (vignette == 7) {
    # Vignette 7 ----
    
    if(example == 1) {
      ## Example 1 ----
      # This is the data we've been working with previously
      noiseless_data <- 
        trans_wide_to_tidy(gcplyr::example_widedata_noiseless, id_cols = "Time")
      # This is the same data but with simulated noise added
      noisy_data <- trans_wide_to_tidy(gcplyr::example_widedata, id_cols = "Time")
      # We'll add some identifiers and then merge them together
      noiseless_data <- dplyr::mutate(noiseless_data, noise = "No")
      noisy_data <- dplyr::mutate(noisy_data, noise = "Yes")
      ex_dat_mrg <- merge_dfs(noisy_data, noiseless_data,
                              warn_morerows = FALSE)
      ex_dat_mrg <- merge_dfs(ex_dat_mrg, gcplyr::example_design_tidy)
      
      ex_dat_mrg$Well <- 
        factor(ex_dat_mrg$Well,
               levels = paste(rep(LETTERS[1:8], each = 12), 1:12, sep = ""))
      ex_dat_mrg$Time <- ex_dat_mrg$Time/3600 #Convert time to hours
      
      # For computational speed, let's just keep the wells we'll be focusing on
      #  (for your own analyses, you should skip this step and continue using
      #  all of your data)
      sample_wells <- c("A1", "F1", "F10", "E11")
      ex_dat_mrg <- ex_dat_mrg[ex_dat_mrg$Well %in% sample_wells, ]
      
      return(ex_dat_mrg)
    } else if (example == 2) {
      ## Example 2 ----
      noisy_data <- trans_wide_to_tidy(gcplyr::example_widedata, id_cols = "Time")
      ex_dat_mrg <- merge_dfs(noisy_data, gcplyr::example_design_tidy)
      
      ex_dat_mrg$Well <- 
        factor(ex_dat_mrg$Well,
               levels = paste(rep(LETTERS[1:8], each = 12), 1:12, sep = ""))
      ex_dat_mrg$Time <- ex_dat_mrg$Time/3600 #Convert time to hours
      
      return(ex_dat_mrg)
    }
    
  } else if (vignette == 8) {
    # Vignette 8 ----
    
    if(example == 1) {
      ## Example 1 ----
      # Define the function that calculates density with a discrete lag time
      lag_then_gr <- function(r, k, lag, init_dens, times) {
        lagged_times <- times - lag
        lagged_times <- ifelse(lagged_times < 0, 0, lagged_times)
        # Density function
        return(k/(1-(1-(k/init_dens))*exp(-r*lagged_times)))
      }
      
      # Set up our wide-shaped data frame
      times <- seq(from = 0, to = 24*60, by = 15)
      sim_dat <- as.data.frame(matrix(NA, nrow = length(times), ncol = 98))
      sim_dat[, 1] <- times
      colnames(sim_dat) <- c("time", "averaged", paste("Well", 1:96, sep = ""))
      
      # Simulate growth
      for (i in 3:ncol(sim_dat)) {
        sim_dat[, i] <- lag_then_gr(times = sim_dat$time, 
                                   r = 0.02, k = 1, 
                                   lag = stats::runif(1, min = 0, max = 500),
                                   init_dens = 0.001)
      }
      
      # Calculate the "average well"
      sim_dat[, "averaged"] <- rowMeans(sim_dat[, 3:ncol(sim_dat)])
      
      # Transform to tidy and calculate per-capita growth rate                  
      sim_dat_tdy <- trans_wide_to_tidy(sim_dat, id_cols = "time")
      
      return(sim_dat_tdy)
    } else if (example == 2) {
      ## Example 2 ----
      example_tidydata <- trans_wide_to_tidy(gcplyr::example_widedata_noiseless,
                                             id_cols = "Time")
      ex_dat_mrg <- merge_dfs(example_tidydata, gcplyr::example_design_tidy)
      ex_dat_mrg_sum <-
        dplyr::summarize(
          dplyr::group_by(
            dplyr::filter(ex_dat_mrg, .data$Phage == "No Phage"),
            .data$Well, .data$Bacteria_strain, .data$Phage),
          auc = auc(x = .data$Time, y = .data$Measurements))
      
      set.seed(123)
      antibiotic_dat <- 
        data.frame(Bacteria_strain = paste("Strain", 1:48),
                   Antibiotic_resis = 
                     ex_dat_mrg_sum$auc[
                       match(paste("Strain", 1:48), 
                             ex_dat_mrg_sum$Bacteria_strain)] * 
                     stats::runif(48, 0.5, 1.5) < mean(ex_dat_mrg_sum$auc))
      return(antibiotic_dat)
    }
    
  } else if (vignette == 9) {
    # Vignette 9 ----
    
    if(example == 1) {
      ## Example 1 ----
      #block-shaped files for multiple plates easily separable
      temp_filenames1 <- 
        paste0(dir, "Plate1-", 
              paste(gcplyr::example_widedata_noiseless$Time %/% 3600,
                    formatC((gcplyr::example_widedata_noiseless$Time %% 3600) %/% 60, 
                            width = 2, flag = 0),
                    formatC((gcplyr::example_widedata_noiseless$Time %% 3600) %% 60,
                            width = 2, flag = 0),
                    sep = "_"), ".csv")
      temp_filenames2 <- 
        paste0(dir, "Plate2-", 
              paste(gcplyr::example_widedata_noiseless$Time %/% 3600,
                    formatC((gcplyr::example_widedata_noiseless$Time %% 3600) %/% 60, 
                            width = 2, flag = 0),
                    formatC((gcplyr::example_widedata_noiseless$Time %% 3600) %% 60,
                            width = 2, flag = 0),
                    sep = "_"), ".csv")
      for (i in 1:length(temp_filenames1)) {
        utils::write.table(
          cbind(
            matrix(c("", "", "", "", "A", "B", "C", "D", "E", "F", "G", "H"), 
                   nrow = 12),
            rbind(rep("", 12),
                  matrix(c("Time", gcplyr::example_widedata_noiseless$Time[i], rep("", 10)), 
                         ncol = 12),
                  rep("", 12),
                  matrix(1:12, ncol = 12),
                  matrix(
                    gcplyr::example_widedata_noiseless[i, 2:ncol(gcplyr::example_widedata_noiseless)],
                    ncol = 12))
          ), 
          file = temp_filenames1[i], quote = FALSE, row.names = FALSE, sep = ",",
          col.names = FALSE)
      }
      for (i in 1:length(temp_filenames2)) {
        utils::write.table(
          cbind(
            matrix(c("", "", "", "", "A", "B", "C", "D", "E", "F", "G", "H"), 
                   nrow = 12),
            rbind(rep("", 12),
                  matrix(c("Time", gcplyr::example_widedata_noiseless$Time[i], rep("", 10)), 
                         ncol = 12),
                  rep("", 12),
                  matrix(1:12, ncol = 12),
                  matrix(
                    gcplyr::example_widedata_noiseless[i, 2:ncol(gcplyr::example_widedata_noiseless)],
                    ncol = 12))
          ), 
          file = temp_filenames2[i], quote = FALSE, row.names = FALSE, sep = ",",
          col.names = FALSE)
      }
      message("Files have been written")
      return(c(temp_filenames1, temp_filenames2))
      
    } else if (example == 2) {
      ## Example 2 ----
      #Interleaved block-shaped files
      times <- c(gcplyr::example_widedata_noiseless$Time, gcplyr::example_widedata_noiseless$Time + 1)
      times <- times[order(times)]
      
      temp_filenames <-
        paste(dir,
          paste(formatC(times %/% 3600, width = 2, flag = 0),
                formatC((times %% 3600) %/% 60, width = 2, flag = 0),
                formatC((times %% 3600) %% 60, width = 2, flag = 0),
                sep = "_"), ".csv", sep = "")
      
      for (i in 1:length(temp_filenames)) {
        utils::write.table(
          cbind(
            matrix(c("", "", "", "", "A", "B", "C", "D", "E", "F", "G", "H"), 
                   nrow = 12),
            rbind(rep("", 12),
                  matrix(c("Time", times[i], rep("", 10)), 
                         ncol = 12),
                  rep("", 12),
                  matrix(1:12, ncol = 12),
                  matrix(
                    gcplyr::example_widedata_noiseless[i, 2:ncol(gcplyr::example_widedata_noiseless)],
                    ncol = 12))
          ), 
          file = temp_filenames[i], quote = FALSE, row.names = FALSE, sep = ",",
          col.names = FALSE)
      }
      message("Files have been written")
      return(temp_filenames)
      
    } else if (example == 3) {
      ## Example 3 ----
      # This code just creates a wide-shaped example file where the data doesn't
      # start on the first row.
      temp_example_widedata <- gcplyr::example_widedata_noiseless
      colnames(temp_example_widedata) <- paste("V", 1:ncol(temp_example_widedata),
                                               sep = "")
      modified_example_widedata <-
        rbind(
          as.data.frame(matrix("", nrow = 4, ncol = ncol(gcplyr::example_widedata_noiseless))),
          colnames(gcplyr::example_widedata_noiseless),
          temp_example_widedata)
      modified_example_widedata[1:2, 1:2] <- 
        c("Experiment name", "Start date", "Experiment_1", as.character(Sys.Date()))
      
      utils::write.table(modified_example_widedata, file = paste0(dir, "widedata.csv"), 
                  row.names = FALSE, col.names = FALSE, sep = ",")
      utils::write.table(modified_example_widedata, file = paste0(dir, "widedata2.csv"), 
                  row.names = FALSE, col.names = FALSE, sep = ",")
      message("Files have been written")
      return(paste0(dir, c("widedata.csv", "widedata2.csv")))
    }
  }
  stop("Your vignette-example combination is not a valid selection")
}

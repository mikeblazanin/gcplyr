#' Create R objects or files as seen in vignette examples
#' 
#' This function makes it easy to generate R objects or files that are
#' created in the vignette examples.
#' 
#' @param vignette Number of the vignette the example object or file is created in
#' @param example Number of the example the object or file is created in
#' @param dir The directory files should be saved into. Only required when
#'            the specified example writes to file(s)
#' 
#' @return An R object, or NULL if files have been written
#' 
#' @export
make_example <- function(vignette, example, dir = NULL) {
  browser()
  #Make sure dir ends in /
  if(substr(dir, nchar(dir), nchar(dir)) != "/") {dir <- paste0(dir, "/")}
  if(!dir.exists(dir)) {dir.create(dir)}
  
  #Vignette 1 ----
  if(vignette == 1) {
    
    ##Example 1 ----
    if(example == 1) {
      write.csv(example_widedata_noiseless, file = paste0(dir, "widedata.csv"), 
                row.names = FALSE)
      message("Files have been written")
      return(invisible(NULL))
    
    ##Example 2 ----  
    } else if (example == 2) {
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
      return(invisible(NULL))
    }
    
  #Vignette 9 ----
  } else if (vignette == 9) {
    
    ##Example 1 ----
    if(example == 1) {
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
      
    ##Example 2 ----
    } else if (example == 2) {
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

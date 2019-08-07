#wide-curves: dataframe with each column corresponding to a single well
#block-curves: dataframe where rows and columns match literally to a plate

library(readxl)

make_widecurves <- function(files, startRow, endRow, startCol, endCol, sheet=1) {
  #requires all files for a given growth curve to be the same file format
  my_curves <- rep(NA, length(files))
  require(tools)
  if (file_ext(files)[1] == "csv") {
    my_curves <- lapply(files, function(x) read.csv(x)[startRow:endRow, startCol:endCol])
  } else if (file_ext(files)[1] == "xls" | file_ext(files)[1] == "xlsx") {
    ##Define a function to convert Excel column name letters to numbers
    # Input: A string of letters s
    # Output: Corresponding column number
    LettersToNumbers <- function(s){
      # Uppercase
      s_upper <- toupper(s)
      # Convert string to a vector of single letters
      s_split <- unlist(strsplit(s_upper, split=""))
      # Convert each letter to the corresponding number
      s_number <- sapply(s_split, function(x) {which(LETTERS == x)})
      # Derive the numeric value associated with each letter
      numbers <- 26^((length(s_number)-1):0)
      # Calculate the column number
      column_number <- sum(s_number * numbers)
      column_number
    }
    ##Define function to convert Excel column numbers to letters
    # Input: a column number
    # Output: Corresponding column name (string of letters)
    NumbersToLetters <- function(x) {
      output <- ""
      MY_LETTERS <- LETTERS[c(26, 1:25)]
      while (x > 0) {
        output <- paste(MY_LETTERS[(x)%%26+1], output, sep = "")
        x <- floor(x/26)
      }
      return(output)
    }
    
    require(readxl)
    my_range <- paste(LETTERS[startCol], 
    my_curves <- lapply(files, function(x) read_excel(x, sheet = sheet,
                                                      range = 
  
  
}

layout_cleanup <- function(layout) {
  #This function takes a layout dataframe with ...'s where info needs to be 
  #iterated in and does so, while adding _A, _B etc for replicate wells
  #with the same contents
  #Wells labeled NA are not filled in (they are empty)
  
  #Define a matrix to track well contents we've seen before
  #So that when they come up again the replicate number can resume
  #where it left off
  vals_and_cntr <- matrix(nrow = 0, ncol = 2)
  for (i in 1:nrow(layout)) {
    for (j in 2:ncol(layout)) {
      if(!is.na(layout[i, j])) { #Non-empty well
        if (nchar(layout[i, j]) > 1) { #Non ... well
          current_val <- layout[i, j]
          if (!(current_val %in% vals_and_cntr[, 1])) {
            #This is the first time we've seen these well contents
            #So we should start numbering replicates at the beginning
            vals_and_cntr <- rbind(vals_and_cntr, c(current_val, 1))
            row <- nrow(vals_and_cntr)
          } else { 
            #this isn't the first time we've seen these contents
            #resume replicate numbering where we left off
            row <- which(vals_and_cntr[, 1] == current_val)
          }
        }
        layout[i, j] <- paste(current_val, 
                              LETTERS[as.numeric(vals_and_cntr[row, 2])], sep = "_")
        vals_and_cntr[row, 2] <- as.numeric(vals_and_cntr[row, 2]) + 1
      }
    }
  }
  return(layout)
}

#Merge layout & data
merge_lay_data <- function(layout, data) {
  #Tidies (melts) the layout dataframe
  #Then merges the now-tidy layout & data dataframes
  layout_mlt <- reshape2::melt(layout, id.vars = 1, value.name = "Contents", 
                               variable.name = "column")
  layout_mlt$Well <- paste(layout_mlt[, 1], substr(layout_mlt$column, 2, 
                                                nchar(as.character(layout_mlt$column))), 
                           sep = "")
  
  data_mlt <- reshape2::melt(data, id.vars = c("Time", "Temperature"), 
                             variable.name = "Well", value.name = "OD600")
  data_mlt$Contents <- layout_mlt$Contents[match(as.character(data_mlt$Well), 
                                                 as.character(layout_mlt$Well))]
  data_mlt$format <- paste(colnames(layout)[1], "_Rep", sep = "")
  return(data_mlt)
}

smooth_data <- function(my_data, smooth_over, subset_by) {
  #data must be sorted sequentially before fed into function
  #my_data is a vector of the data to be smoothed
  #smooth over is how many sequential entries to average
  #the unique values of subset_by will be what is iterated over
  out_list <- rep(NA, length(my_data))
  cntr = 1
  for (my_uniq in unique(subset_by)) {
    my_sub <- subset(my_data, subset_by == my_uniq)
    out_list[cntr:(cntr+length(my_sub)-smooth_over)] <- 0
    for (i in 1:smooth_over) {
      out_list[(cntr):(cntr+length(my_sub)-smooth_over)] <-
        out_list[(cntr):(cntr+length(my_sub)-smooth_over)] + 
        my_sub[i:(length(my_sub)-smooth_over+i)]
    }  
    cntr <- cntr+length(my_sub)
  }
  out_list <- out_list/smooth_over
  return(out_list)
}


#extraction function: to get OD peak height and time
analyze_curves <- function(od_data, time_data, bandwidth = 10, return) {
  #Takes vectors of the od_data and time_data
  #Bandwidth is how wide the window should be to look for a peak
  #Narrower bandwidth will get you an earlier local maxima
  #Wider bandwidth will get you a later more-global maxima
  #Designed to be run w/ group_by
  prev_max_pos <- 0
  cnt_max_pos <- bandwidth
  while (cnt_max_pos != prev_max_pos) {
    prev_max_pos <- cnt_max_pos
    search_start <- pmax(cnt_max_pos-bandwidth, 1) #start of the search window
    search_end <- pmin(search_start + 2*bandwidth, length(od_data)) #end of the search window
    cnt_max_pos <- which.max(od_data[search_start:search_end]) + search_start - 1
  }
  if (return == "max") {return(od_data[cnt_max_pos])
  } else if (return == "maxtime") {return(time_data[cnt_max_pos])}
}

#Group data by indiv growth curves
grp_data1 <- dplyr::group_by(spl_data1[!is.na(spl_data1$sm_od), ], 
                     bacteria, phage, phageshock, bactshock, mediashock, Rep, Contents)

#Get OD peak height & time for each growth curve
out_data1 <- dplyr::summarize(grp_data1, 
                      max = analyze_curves(sm_od, Time, 
                                           bandwidth = 20, return = "max"),
                      maxtime = analyze_curves(sm_od, Time, 
                                               bandwidth = 20, return = "maxtime"))


#Plots to visually inspect peak designation accuracy
view_peaks <- function(data_mlt, data_out, plt_point = TRUE,
                       numplots = 9, lwd = 1) {
  for (start_group in seq(from = 1, to = length(unique(data_mlt$Contents)), by = numplots)) {
    my_groups <- unique(data_mlt$Contents)[start_group:(start_group+numplots-1)]
    myplot <- ggplot(data = data_mlt[data_mlt$Contents %in% my_groups, ],
                 aes(x = Time, y = sm_od)) + geom_line(lwd = lwd) +
            facet_wrap(~Contents) + ylab("Smoothed OD600")
    if (plt_point) {myplot <- myplot + 
                              geom_point(data = data_out[data_out$Contents %in% my_groups, ],
                                                  aes(x = maxtime, y = max),
                                                  size = 3, pch = 13)
    }
    print(myplot)
    # ggsave(filename = paste(start_group, "_growcurves.pdf", sep = ""),
    #        device = "pdf", width = 8, height = 8, units = "in")
  }
}


#### Kenichi's script:
#### # The script reads optical density data from  the microplate reader in the xls spreadsheets generated by evoware

require(xlsx)

# cd into the directory with the excel spreadsheets
plate_reader_spreads <- list.files()
# kv <- matrix(ncol=96,nrow=length(plate_reader_spreads))



microplate_output <- list()

for (i in 1:length(plate_reader_spreads))
	{
	# By evoware default, the spreadsheet format begins the microplate reader on row #27, col #B:M (note there are 12 columns)
	# with shaking by incubator:
	z <- read.xlsx(plate_reader_spreads[i],sheetIndex=1, startRow=27, endRow=35, colIndex=2:14)
	# without shaking by incubator:
	#z <- read.xlsx(plate_reader_spreads[i],sheetIndex=1, startRow=24, endRow=32, colIndex=2:14)
	microplate_output[[i]] <- z
	}

# To visualize dynamics in a given well:
retrieve_well_timestep <- function(step,x,y)
	{
	return(microplate_output[[step]][x,y])
	}

# TODO: fix this to use mapply
visualize_dynamics <- function(x,y)
	{
	retrieve_this_well <- function(step)
		{
		return(retrieve_well_timestep(step,x,y))
		}
		
	od_measures <- sapply(1:length(plate_reader_spreads), retrieve_this_well)
	#od_measures <- sapply(1:281, retrieve_this_well)
	plot(od_measures,xlab="measurement point", ylab="OD",t="l",ylim=range(microplate_output))
	}

dev.new(height=8,width=10)
par(mfrow=c(8,12),mar=c(1,2,1,1))
for (i in 1:8)
	for (j in 1:12)
		visualize_dynamics(i,j)

matrixAll <- matrix(nrow=length(plate_reader_spreads), ncol=96)
for (i in 1:length(plate_reader_spreads))
 {
 matrixAll[i,] <- c(as.matrix(microplate_output[[i]]))
 }



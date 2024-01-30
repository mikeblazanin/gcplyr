#' Example noisy growth curve data in wide format
#' 
#' A dataset containing example growth of 96 wells of simulated bacteria
#'  or bacteria and phages
#'  
#' Wells A1...A8 through F1...F8 contain 48 different
#' simulated bacterial strains growing alone. Wells G1...G8 through L1...L8
#' contain the same 48 bacterial strains in an identical layout, but this
#' time growing in the presence of a phage
#' 
#' Bacterial populations exhibit diauxic growth as they approach their
#' carrying capacity, and they also evolve resistance in the face of 
#' selection from the phage population.
#' 
#' This data includes some simulated noise to approximate the noise generated
#' during data collection by plate readers
#'
#' @format A dataframe with 97 rows and 97 variables:
#' \describe{
#'      \item{time}{time, in seconds, since growth curve began}
#'      \item{A1, A2...H11, H12}{bacterial density in the given well}
#'  }
"example_widedata"

#' Example growth curve data in wide format
#' 
#' A dataset containing example growth of 96 wells of simulated bacteria
#'  or bacteria and phages
#'  
#' Wells A1...A8 through F1...F8 contain 48 different
#' simulated bacterial strains growing alone. Wells G1...G8 through L1...L8
#' contain the same 48 bacterial strains in an identical layout, but this
#' time growing in the presence of a phage
#' 
#' Bacterial populations exhibit diauxic growth as they approach their
#' carrying capacity, and they also evolve resistance in the face of 
#' selection from the phage population.
#' 
#' This data does not include any simulated noise
#'
#' @format A dataframe with 97 rows and 97 variables:
#' \describe{
#'      \item{time}{time, in seconds, since growth curve began}
#'      \item{A1, A2...H11, H12}{bacterial density in the given well}
#'  }
"example_widedata_noiseless"

#' Design for example growth curve data
#'  
#' A tidy-shaped dataset with the experimental design (i.e. plate layout)
#' for the example data included with \code{gcplyr}.
#' 
#' Wells A1...A8 through F1...F8 contain 48 different
#' simulated bacterial strains growing alone. Wells G1...G8 through L1...L8
#' contain the same 48 bacterial strains in an identical layout, but this
#' time growing in the presence of a phage
#'
#' @format A dataframe with 96 rows and 3 variables:
#' \describe{
#'      \item{Well}{The well of the plate}
#'      \item{Bacteria_strain}{The numbered bacterial strain growing in each well}
#'      \item{Phage}{Whether or not the bacteria were simulated growing with phages}
#'  }
"example_design_tidy"
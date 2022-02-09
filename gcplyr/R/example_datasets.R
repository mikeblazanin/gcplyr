#' Example growth curve data in wide format
#' 
#' A dataset containing example growth of 96 wells of simulated bacteria
#'  or bacteria and phages
#'
#' @format A dataframe with ___ rows and 97 variables:
#' \describe{
#'      \item{time}{time, in seconds, since growth curve began}
#'      \item{A1, A2...H11, H12}{bacterial density in the given well}
#'  }
"example_widedata"

#Code to generate example data:
example_widedata <- as.data.frame(matrix(NA, nrow = 24*4+1, ncol = 97))
colnames(example_widedata) <- c("Time", 
                                paste(
                                  rep(to_excel(1:8), 12),
                                  rep(1:12, each = 8), sep = ""))
example_widedata$Time <- seq(from = 0, to = 24*60*60,
                             by = 15*60)
#Generate vectors of bacterial growth parameters
set.seed(123)
u_vector <- runif(48, min = 0.1, 1)/3600
u_vector <- rep(u_vector, 2)
k_vector <- rep(10**9, 96)
dens_init_vector <- rep(10**5, 96)
#Generate vectors of parameters to approximate viral growth
u_vir_vector <- c(rep(0, 48), runif(48, min = 0.1, 1)/3600)
k_vir_vector <- rep(10**10, 96)
dens_init_vir_vector <- c(rep(0, 48), rep(10**4, 48))
#Calculate growth (bacteria alone in first 48 wells, bact + phage in next 48)
for (i in 1:96) {
  example_widedata[, i+1] <- 
    k_vector[i]/
    (1+((k_vector[i]-dens_init_vector[i])/dens_init_vector[i])*
       exp(-u_vector[i]*example_widedata$Time)) -
    k_vir_vector[i]/
    (1+((k_vir_vector[i]-dens_init_vir_vector[i])/dens_init_vir_vector[i])*
       exp(-u_vir_vector[i]*example_widedata$Time))
  example_widedata[(example_widedata[, i+1] < 0), i+1] <- 0
}
#Code to visualize example data
ex_lng <- tidyr::pivot_longer(example_widedata, cols = -Time)
ggplot2::ggplot(ex_lng, ggplot2::aes(x = Time, y = value, color = name)) +
  ggplot2::geom_line() +
  ggplot2::guides(color = FALSE) +
  ggplot2::scale_y_continuous(trans = "log10")
#Save
save(example_widedata, file="data/example_widedata.RData")

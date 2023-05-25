library(gcplyr)
library(ggplot2)
library(dplyr)

# Load experimental data ----
dat <-
  read_wides(
    files = "./manuscript/2021-10-15_Emma_Growth_Curve.csv",
    startrow = 29, startcol = "B")

dat <- trans_wide_to_tidy(dat, id_cols = c("file", "Time", "T 600"))
dat <- mutate(dat,
              Time = lubridate::time_length(lubridate::hms(Time), unit = "hour"),
              type = "nondiaux")

design_diftconcs <- 
  make_design(
    nrows = 8, ncols = 12,
    output_format = "tidy",
    init_bact = make_designpattern(c(10**5, 5*10**4, 10**4, 0),
                                   rows = 2:7, cols = 2:4,
                                   pattern = "444111222333222222",
                                   byrow = TRUE),
    init_bact = make_designpattern(c(10**5, 10**4),
                                   rows = 2:6, cols = 5:7,
                                   pattern = "111111111222222",
                                   byrow = TRUE),
    init_moi = make_designpattern(c(0, 0.1, 0.01),
                                  rows = 2:7, cols = 2:4,
                                  pattern = "111111111111222333",
                                  byrow = TRUE),
    init_moi = make_designpattern(c(0.1, 0.01, 0.001),
                                  rows = 2:6, cols = 5:7,
                                  pattern = "111222333111222",
                                  byrow = TRUE),
    #Row 7 cols 5:7 is actually empty but df rows will be dropped anyway
    bacteria = make_designpattern("PF",
                                  rows = 2:7, cols = 2:7,
                                  pattern = "1")
  )

dat <- merge_dfs(dat, design_diftconcs)
dat <- dplyr::filter(dat, init_bact != "NA", init_moi != "NA", bacteria != "NA")

#subtract blank
dat$Measurements <- dat$Measurements - 
  min(dplyr::filter(dat, init_bact == 0)$Measurements, na.rm = TRUE)

dat <- dplyr::filter(dat, init_bact == 10**5, init_moi %in% c(0, 0.01))

dat <- dplyr::filter(dat, Well %in% c("C2", "C7"))
dat <- select(dat, Time, Well, Measurements, init_moi, type)

# Load diauxie experimental data ----
dat_diaux <- read.csv("./manuscript/Isolate_growth_curves.csv")
dat_diaux$Well <- "Z1"
dat_diaux$init_moi <- "0"
dat_diaux$type <- "diauxic"
dat_diaux <- mutate(dat_diaux, Time = Time_s/3600)
#Subtract blank
dat_diaux$Measurements <- 
  dat_diaux$OD600 - lm(OD600 ~ cfu_ml, dat_diaux)$coefficients[1]
dat_diaux <- select(dat_diaux, Time, Well, Measurements, init_moi, type)

# Merge ----
dat <- dplyr::full_join(dat, dat_diaux)

#Make 4 example datasets (all running from 0 to 18 hours):
#1 - C2 but only after 2 hours (so no lag) - 'nolag'
#2 - C2 entirely (with lag) - 'lag'
#3 - Z1 but rescaled so y values are ~same as C2 - 'diauxie'
#3 - C7 entirely - 'phage'

dat <- mutate(dat,
  ex_case = NA,
  ex_case = ifelse(Well == "C2", 'lag', ex_case),
  ex_case = ifelse(Well == "Z1", 'diauxie', ex_case),
  ex_case = ifelse(Well == "C7", 'phage', ex_case))
dat$Measurements[dat$Well == "Z1"] <-
  scales::rescale(x = dat$Measurements[dat$Well == "Z1"],
          to = c(min(dat$Measurements[dat$Well == "C2"]) + 0.001,
                 max(dat$Measurements[dat$Well == "C2"])))
dat <- filter(dat, Time <= 18.1)
temp <- filter(dat, ex_case == "lag", Time > 2)
temp <- mutate(temp, ex_case = "nolag", 
               Time = Time - 2,
               Time = scales::rescale(Time, to = c(0, 18)))
dat <- rbind(dat, temp)


ggplot(dat, aes(x = Time, y = Measurements, color = ex_case)) +
  geom_point() +
  scale_y_log10()






png("./talk_materials/example_data1.png", width = 5, height = 4,
    units = "in", res = 150)
ggplot(filter(dat, Well == "C2", Time > 2),
       aes(x = Time-2, y = Measurements)) +
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  labs(x = "Time (hr)") +
  theme(axis.title = element_text(size = 20)) +
  scale_y_log10()
dev.off()

png("./talk_materials/example_data2.png", width = 5, height = 4,
    units = "in", res = 150)
ggplot(filter(dat, Well == "C2"),
       aes(x = Time, y = Measurements)) +
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  labs(x = "Time (hr)") +
  theme(axis.title = element_text(size = 20)) +
  scale_y_log10()
dev.off()

# fitting funcs ----
logis_func <- function(r, k, d0, t_vals) {
  if (anyNA(c(r, k, d0, t_vals))) {return(NA)}
  t_vals_hrs <- t_vals/3600
  d <- k/(1+(((k-d0)/d0)*exp(-r*(t_vals_hrs))))
  return(d)
}

logis_fit_err <- function(params, t_vals, dens_vals) {
  #params <- c("logk" = ..., "d0" = ..., "r" = ..., "delta" = ...)
  t_vals_hrs <- t_vals/3600
  k <- 10**params["logk"]
  pred_vals <- logis_func(r = params["r"], k = params["k"], d0 = params["d0"],
                          t_vals = t_vals)
  pred_vals[pred_vals < 0] <- 0
  err <- sum((log10(pred_vals) - log10(dens_vals))**2)
  if (is.infinite(err) | is.na(err)) {return(2*10**300)} else {return(err)}
}

baranyi_func <- function(r, k, v, d0, t_vals) {
  #Modified from Ram et al 2019 with a(t) = 1
  # (equivalent to logistic with a deceleration param)
  if (anyNA(c(r, k, v, d0, t_vals))) {return(NA)}
  t_vals_hrs <- t_vals/3600
  d <- k/((1-(1-((k/d0)**v))*exp(-r*v*t_vals_hrs))**(1/v))
  return(d)
}

baranyi_fit_err <- function(params, t_vals, dens_vals) {
  #params <- c("logk" = ..., "logd0" = ..., "r" = ..., "v" = ...)
  pred_vals <- baranyi_func(r = params["r"],
                            k = 10**params["logk"],
                            v = params["v"],
                            d0 = 10**params["logd0"],
                            t_vals = t_vals)
  pred_vals[pred_vals < 0] <- 0
  err <- sum((log10(pred_vals) - log10(dens_vals))**2)
  if (is.infinite(err) | is.na(err)) {return(2*10**300)} else {return(err)}
}

get_baranyi_fit <- function(x, y) {
  optim(par = c("logk" = log10(1),
                "logd0" = log10(init_d0),
                "r" = init_r,
                "v" = init_v),
        fn = baranyi_fit_err,
        dens_vals = gc_data$cfu_ml[gc_rows],
        t_vals = gc_data$Time_s[gc_rows],
        method = "L-BFGS-B",
        #logk, logd0, r, v
        lower = c(5, 4, 0, 0),
        upper = c(11, 10, 10, 50))
  
  
}

# fit ----

#Do fitting

gc_summarize <- summarize(


gc_summarized <- cbind(gc_summarized,
                       data.frame("fit_r" = as.numeric(NA), 
                                  "fit_k" = as.numeric(NA),
                                  "fit_v" = as.numeric(NA), 
                                  "fit_d0" = as.numeric(NA),
                                  "fit_err" = as.numeric(NA)))
for (sum_row in 1:nrow(gc_summarized)) {
  my_well <- gc_summarized$uniq_well[sum_row]
  
  start_time <- gc_summarized$threshold_percap_gr_time[sum_row]
  
  if(is.na(gc_summarized$diauxie_time[sum_row])) {
    end_time <- max(gc_data$Time_s[gc_data$uniq_well == my_well])
  } else {end_time <- gc_summarized$diauxie_time[sum_row]}
  
  gc_rows <- which(gc_data$uniq_well == my_well &
                     gc_data$Time_s <= end_time &
                     gc_data$Time_s >= start_time)
  
  #Set initial values
  init_d0 <- min(gc_data$cfu_ml[gc_rows])
  init_K <- max(gc_data$cfu_ml[gc_rows])
  init_r <- 1
  init_v <- 1
  
  #Fit
  temp <- optim(par = c("logk" = log10(init_K),
                        "logd0" = log10(init_d0),
                        "r" = init_r,
                        "v" = init_v),
                fn = baranyi_fit_err,
                dens_vals = gc_data$cfu_ml[gc_rows],
                t_vals = gc_data$Time_s[gc_rows],
                method = "L-BFGS-B",
                #logk, logd0, r, v
                lower = c(5, 4, 0, 0),
                upper = c(11, 10, 10, 50))
  
  #Save fit vals
  gc_summarized[sum_row, 
                c("fit_r", "fit_k", "fit_v", "fit_d0", "fit_err")] <-
    data.frame("fit_r" = temp$par["r"], 
               "fit_k" = 10**temp$par["logk"],
               "fit_v" = temp$par["v"], 
               "fit_d0" = 10**temp$par["logd0"],
               "fit_err" = temp$value)
}
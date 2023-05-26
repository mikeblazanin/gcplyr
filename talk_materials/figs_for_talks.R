library(gcplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cranlogs)

# Load example fitting data ----
## no-diaux data
dat <-
  read_wides(
    files = "./manuscript/2021-10-15_Emma_Growth_Curve.csv",
    startrow = 29, startcol = "B")

dat <- trans_wide_to_tidy(dat, id_cols = c("file", "Time", "T 600"))
dat <- mutate(dat,
              Time = lubridate::time_length(lubridate::hms(Time), unit = "hour"))
dat <- dplyr::filter(dat, Well %in% c("C2", "C7"))

## diaux data
dat_diaux <- read.csv("./manuscript/Isolate_growth_curves.csv")
dat_diaux$Well <- "Z1"
dat_diaux <- mutate(dat_diaux, 
                    Time = Time_s/3600,
                    Measurements = OD600)

## merge
dat <- dplyr::full_join(dat, dat_diaux)
dat <- select(dat, Time, Well, Measurements)

#Make 4 example datasets for fitting ----
# (all running from 0 to 18 hours):
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
dat_cut <- filter(dat, Time <= 18.1)
temp <- filter(dat_cut, ex_case == "lag", Time > 2)
temp <- mutate(temp, ex_case = "nolag", 
               Time = Time - 2,
               Time = scales::rescale(Time, to = c(0, 18)))
dat_cut <- rbind(dat_cut, temp)

ggplot(dat_cut, aes(x = Time, y = Measurements)) +
  geom_point(aes(color = ex_case)) +
  scale_y_log10()

#Functions for fitting ----
super_func <- function(r, k, d0, 
                       v = 1, q0 = Inf, m = Inf, 
                       t_vals) {
  #Specify q0 = Inf and m = Inf to have no acclimation
  if(anyNA(c(r, k, d0, t_vals))) {return(NA)}
  if(q0 < 0) {q0 <- 0}
  if(q0 == Inf && m == Inf) {a <- t_vals
  } else {a <- t_vals + 1/m*log((exp(-m * t_vals) + q0)/(1+q0))}
  d <- k/(1-(1-((k/d0)**v))*exp(-r*v*a))**(1/v)
  return(d)
}

super_fit_err <- function(params, t_vals, dens_vals,
                          v = NULL, q0 = NULL, m = NULL) {
  #params = c("r" = , "logk" = , "logd0" = , v = , q0 = , m = )
  # except that v, q0, and m could be specified outside of params
  # in which case they're held fixed and not optimized
  #Use q0 = Inf and m = Inf to have no acclimation
  if(!is.null(v)) {params["v"] <- v}
  if(!is.null(q0)) {params["q0"] <- q0}
  if(!is.null(m)) {params["m"] <- m}
  if(any(!c("r", "logk", "logd0", "v", "q0", "m") %in% names(params))) {
    stop("All params must be specified or fixed")}
  
  pred_vals <- super_func(t_vals = t_vals,
                          r = params["r"], k = 10**params["logk"],
                          d0 = 10**params["logd0"], v = params["v"],
                          q0 = params["q0"], m = params["m"])
  err <- sum((log10(pred_vals) - log10(dens_vals))**2)
  if (is.infinite(err) | is.na(err)) {return(2*10**300)} else {return(err)}
}

get_super_fit <- function(x, y,
                          r = 0.2, logk = log10(1), logd0 = log10(0.1),
                          v = 1, q0 = Inf, m = Inf,
                          v_fixed = TRUE, q0_fixed = TRUE, m_fixed = TRUE,
                          prefix = "") {
  params <- c("r" = r, "logk" = logk, "logd0" = logd0)
  
  if(!v_fixed) {params <- c(params, "v" = v); v <- NULL}
  if(!q0_fixed) {params <- c(params, "q0" = q0); q0 <- NULL}
  if(!m_fixed) {params <- c(params, "m" = m); m <- NULL}
  
  temp <- optim(par = params,
                fn = super_fit_err,
                dens_vals = y, t_vals = x,
                v = v, q0 = q0, m = m)
  temp <- 
    as.data.frame(
      bind_rows(
        temp$par
      )
    )
  colnames(temp) <- paste0(prefix, colnames(temp))
  return(temp)
}

#Fit and plot ----
dat_cut_sum <- summarize(
  group_by(dat_cut, ex_case),
  get_super_fit(x = Time, y = Measurements, prefix = "logis_"),
  get_super_fit(x = Time, y = Measurements, v_fixed = FALSE, prefix = "logisv_"),
  get_super_fit(x = Time, y = Measurements, v_fixed = FALSE, 
                q0_fixed = FALSE, m_fixed = FALSE,
                q0 = 0.5, m = 0.2, prefix = "baranyi_"))

dat_cut <- left_join(dat_cut, dat_cut_sum)
dat_cut <- mutate(group_by(dat_cut, ex_case),
              pred_logis = 
                super_func(r = logis_r[1], k = 10**logis_logk[1], 
                           d0 = 10**logis_logd0[1], t_vals = Time),
              pred_logisv = super_func(r = logisv_r[1], k = 10**logisv_logk[1], 
                                       d0 = 10**logisv_logd0[1], 
                                       v = logisv_v[1], t_vals = Time),
              pred_baranyi = super_func(r = baranyi_r[1], k = 10**baranyi_logk[1], 
                                        d0 = 10**baranyi_logd0[1], 
                                        v = baranyi_v[1], 
                                        q0 = baranyi_q0[1], m = baranyi_m[1],
                                        t_vals = Time))

dat_cut_lng <- pivot_longer(data = dat_cut,
                    cols = starts_with("pred_"),
                    names_to = "pred_func", values_to = "pred_val")

#Plot with fitted curves
ggplot(dat_cut_lng, aes(x = Time, y = Measurements, color = ex_case)) +
  geom_point(alpha = 0.1) +
  scale_y_log10() +
  geom_line(aes(y = pred_val, lty = pred_func)) +
  facet_wrap(~ ex_case)

p1 <- ggplot(filter(dat_cut, ex_case == "nolag"),
             aes(x = Time, y = Measurements)) +
  geom_point() +
  scale_y_log10() +
  guides(lty = "none") +
  theme_bw() +
  labs(x = "Time (hr)") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16))

png("./talk_materials/nolag.png", width = 5, height = 4,
    units = "in", res = 150)
p1
dev.off()

png("./talk_materials/nolag_logis.png", width = 5, height = 4,
    units = "in", res = 150)
p1 + geom_line(aes(y = pred_logis), lty = 2, lwd = 2, color = "red")
dev.off()

png("./talk_materials/nolag_logisv.png", width = 5, height = 4,
    units = "in", res = 150)
p1 + geom_line(aes(y = pred_logisv), lty = 2, lwd = 2, color = "red")
dev.off()

p1 <- ggplot(filter(dat_cut, ex_case == "lag"),
             aes(x = Time, y = Measurements)) +
  geom_point() +
  scale_y_log10() +
  guides(lty = "none") +
  theme_bw() +
  labs(x = "Time (hr)") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16))

png("./talk_materials/lag.png", width = 5, height = 4,
    units = "in", res = 150)
p1
dev.off()

png("./talk_materials/lag_logisv.png", width = 5, height = 4,
    units = "in", res = 150)
p1 + geom_line(aes(y = pred_logisv), lty = 2, lwd = 2, color = "red")
dev.off()

png("./talk_materials/lag_baranyi.png", width = 5, height = 4,
    units = "in", res = 150)
p1 + geom_line(aes(y = pred_baranyi), lty = 2, lwd = 2, color = "red")
dev.off()


p1 <- ggplot(filter(dat_cut, ex_case == "diauxie"),
             aes(x = Time, y = Measurements)) +
  geom_point() +
  scale_y_log10() +
  guides(lty = "none") +
  theme_bw() +
  labs(x = "Time (hr)") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16))

png("./talk_materials/diaux.png", width = 5, height = 4,
    units = "in", res = 150)
p1
dev.off()

png("./talk_materials/diaux_baranyi.png", width = 5, height = 4,
    units = "in", res = 150)
p1 + geom_line(aes(y = pred_baranyi), lty = 2, lwd = 2, color = "red")
dev.off()

# Create noisy data ----
datnoisy <- filter(dat, Well == "C2")
set.seed(2)
datnoisy$Measurements <-
  round(datnoisy$Measurements +
          0.015*c(arima.sim(model = list(order = c(0, 0, 0)),
                            n = length(datnoisy$Measurements))) +
          sample(x = c(0, 1), length(datnoisy$Measurements), 
                 replace = TRUE, prob = c(0.8, 0.2)) *
          rexp(n = length(datnoisy$Measurements), rate = 20)  +
          sample(x = c(0, 1), length(datnoisy$Measurements), 
                 replace = TRUE, prob = c(0.85, 0.15)) *
          rexp(n = length(datnoisy$Measurements), rate = 19),
        3)

ggplot(data = datnoisy, aes(x = Time, y = Measurements)) +
  geom_point()

datnoisy <- 
  mutate(group_by(datnoisy, Well),
         deriv = calc_deriv(y = Measurements, x = Time),
         sm_1 = smooth_data(y = Measurements, x = Time, 
                               sm_method = "moving-median",
                               window_width_n = 5),
         sm_2 = smooth_data(y = sm_1, x = Time,
                                    sm_method = "moving-average",
                                    window_width_n = 5))

p1 <- ggplot(data = filter(datnoisy), 
             aes(x = Time, y = Measurements)) +
  geom_point(size = 2) +
  theme_bw() +
  labs(x = "Time (hr)") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16))

png("./talk_materials/noisy.png", width = 5, height = 4,
    units = "in", res = 150)
p1
dev.off()

png("./talk_materials/noisy_smoothed.png", width = 5, height = 4,
    units = "in", res = 150)
p1 + geom_line(aes(y = sm_2), color = "red", lwd = 1.3)
dev.off()

#Downloads data
cran <- cran_downloads(packages = "gcplyr", from = "2023-02-01")
cran <- mutate(cran, cumdownloads = cumsum(count))
png("./talk_materials/cran_downloads.png", width = 5, height = 4,
    units = "in", res = 150)
ggplot(data = cran,
       aes(x = date, y = cumdownloads)) +
  geom_point() +
  labs(x = "Date", y = "Cumulative Downloads") +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16))
dev.off()

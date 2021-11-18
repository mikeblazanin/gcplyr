#For testing smooth_data

# set.seed(1)
# data <- data.frame("time" = 1:100,
#                    "dens" = 10/(1+exp(-.1*((1:100) - 50))) + rnorm(100, sd = 0.5))
# temp <- mgcv::gam(dens ~ s(time), data=data)
# temp <- mgcv::gam(formula = formula, data = data)
# 
# plot(test$time, test$dens)
# lines(test$time, temp$fitted)
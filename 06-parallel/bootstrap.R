library(boot)

data("USArrests")

ans <- lm(Murder ~ Rape, data = USArrests)

system.time({
  ans_boot <- boot(
    USArrests,
    statistic = function(dat, i) {
      coef(lm(Murder ~ Rape, data = dat[i,]))
    },
    R = 4000, ncpus = 4, parallel = "multicore")
})

system.time({
  ans_boot <- boot(
    USArrests,
    statistic = function(dat, i) {
      coef(lm(Murder ~ Rape, data = dat[i,]))
    },
    R = 4000, ncpus = 1)
})

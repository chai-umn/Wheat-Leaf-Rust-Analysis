# Analysis on the economically justified R&D investment
# Date: 2020-06-01
# R codes used for the following paper: 
# Chai, Y., Pardey, P.G., Hurley, T.M., Senay, S.D., and Beddow, J.M. (2020) A Probabilistic Bio-economic Assessment of the Global Consequences of Wheat Leaf Rust" Submitted to Phytopathology
# Contact: chaix026@umn.edu

rm(list = ls())

library(data.table)
library(ggplot2)

# Load simulation data
load(file="./Results/Mid/loss.sim.RData")
load(file="./Results/Mid/loss.sim.H.RData")

# Read in price info
price <- fread("./FAO data/wheat_price_b2016.csv")
price.use <- as.vector(price[yr %in% 1961:2050, .(price=r.P.Ton.b2016)]$price)
price.sim <- matrix(price.use, nrow=length(price.use), ncol=ncol(loss.sim), byrow = F)

# Add in price
# Ordinary loss
loss.value <- loss.sim * price.sim


# High loss
loss.value <- loss.sim.H  * price.sim
# loss.value <- (loss.sim.H - loss.sim) * price.sim

yr.avg <- colMeans(loss.value[1:56, ])
summary(yr.avg/1e+6)
quantile(yr.avg/1e+6, probs = c(0.1, 0.50, 0.8, 0.95))

# Interests rates
rc <- 0.1
rr <- 0.03
rm <- 0.1

# Future value of benefits
rr.yr <- 2000:2050
rr.vec <- (1+rr)^(2050-rr.yr)
FVB <- as.vector( rr.vec %*% tail(loss.value, length(rr.yr) ) )

# Present value of costs
fx.PVC <- function(cost, start.yr=1990, end.yr=2050) {
  PVC <- sum(cost / ((1+rc)^(start.yr:end.yr - start.yr)))
  return(PVC)
}

# MIRR
fx.MIRR <- function(cost, start.yr=1990, end.yr=2050) {
  PVC <- fx.PVC(cost, start.yr, end.yr)
  MIRR <- (FVB/PVC)^(1/(end.yr-start.yr)) - 1
  return(MIRR)
}

# Objective
fx.Obj <- function(cost, start.yr=1990, end.yr=2050, cutoff=0.05) {
  MIRR.cutoff <- as.numeric(quantile(fx.MIRR(cost), cutoff))
  Obj <- abs(MIRR.cutoff-rm)
  return(Obj)
}

Opt.Invest <- optimize(fx.Obj, c(30e+06, 80e+06))
Opt.res <- Opt.Invest$minimum
Opt.res

# Numeric plot
num.Obj <- data.frame(cost=c(seq(Opt.res-10e+06, Opt.res, length.out = 20),
                             seq(Opt.res, Opt.res+10e+06, length.out = 20)), Obj = NA)
for (i in 1:nrow(num.Obj)) {
  num.Obj$Obj[i] <- fx.Obj(num.Obj$cost[i])
}

ggplot(data = num.Obj) +
  geom_line(aes(x=cost, y=Obj))





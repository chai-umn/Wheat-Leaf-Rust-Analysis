# Date: 2020-06-01
# R codes used for the following paper: 
# Chai, Y., Pardey, P.G., Hurley, T.M., Senay, S.D., and Beddow, J.M. (2020) A Probabilistic Bio-economic Assessment of the Global Consequences of Wheat Leaf Rust" Submitted to Phytopathology
# Contact: chaix026@umn.edu

rm(list = ls())

library(data.table)
library(ggplot2)

####################################
# Read in FAO data for prod and area
prod <- fread("./FAO data/wheat_prod.csv")
area <- fread("./FAO data/wheat_area.csv")


# Change data to long format
prod.L <- melt(prod, id.vars="yr", variable.name = "zone", variable.factor =F, value.name = "prod")
area.L <- melt(area, id.vars="yr", variable.name = "zone", variable.factor =F, value.name = "area")
yield.L <- merge(prod.L, area.L, by=c("yr", "zone")) [, yield := prod/area]
rm(prod, prod.L, area, area.L)


####################################
# Read in CLIMEX zone GIEI data
eigi <- fread("./CLIMEX output/leaf(ei-5_gi-5---FAO2012).csv")
eigi[, area.hi.shr := tot.area.hi / tot.area.all]
eigi[, eigi.area.hi.shr := eigi.area.hi / tot.area.hi]

####################################
# Merge data
dat.atrisk <- merge(yield.L, eigi[, .(region, area.hi.shr, eigi.area.hi.shr)], by.x="zone", by.y="region", all.x=TRUE) 
dat.atrisk.ori <- as.data.frame(dat.atrisk)
rm(yield.L, eigi)

####################################


####################################
# Simulation
set.seed(100)
rep.N <- 50000
obs.N <- nrow(dat.atrisk)
yr.N <- length(unique(dat.atrisk$yr))

# Simulation function
fx.loss.sim <- function(beta.a, beta.b) {
  loss.sim <- matrix(NA, nrow = yr.N, ncol = rep.N)
  rownames(loss.sim) <- unique(dat.atrisk$yr)
  colnames(loss.sim) <- paste0("rep", 1:rep.N)
  
  
  for (i in 1:rep.N) {
    loss.rep <- dat.atrisk[, loss.shr := rbeta(obs.N, beta.a, beta.b)
                           ][, loss.prod := yield*(1/(1-area.hi.shr*eigi.area.hi.shr*loss.shr)-1)*area
                             ][, .(loss.tot = sum(loss.prod, na.rm=T)), by=.(yr)]
    loss.sim[, i] <- loss.rep$loss.tot
  }
  
  return(loss.sim)
  
}

# Beta distribution
# Parameters are taken from STATA estimation
beta.a.ori <- 1.427
beta.b.ori <- 86.848

# US High Loss, cutoff=2
beta.a.H <- 6.704
beta.b.H <- 189.647

# US High Loss, other cutoffs
beta.a.H1 <- 3.595
beta.b.H1 <- 144.492

beta.a.H3 <- 9.001
beta.b.H3 <- 200.268

beta.a.H5 <- 11.117
beta.b.H5 <- 147.627

# Base-scenario
qbeta(c(0.5, 0.8), beta.a.ori, beta.b.ori)

# High scenario
qbeta(c(0.5, 0.8), beta.a.H, beta.b.H)

# Simulation results

loss.sim.ori   <- fx.loss.sim(beta.a.ori, beta.b.ori)
save(loss.sim.ori, file="./Results/Mid/loss.sim.ori.RData")


loss.sim.H <- fx.loss.sim(beta.a.H, beta.b.H)
save(loss.sim.H, file="./Results/Mid/loss.sim.H.RData")


loss.sim.H1 <- fx.loss.sim(beta.a.H1, beta.b.H1)
save(loss.sim.H1, file="./Results/Mid/loss.sim.H1.RData")

loss.sim.H3 <- fx.loss.sim(beta.a.H3, beta.b.H3)
save(loss.sim.H3, file="./Results/Mid/loss.sim.H3.RData")

loss.sim.H5 <- fx.loss.sim(beta.a.H5, beta.b.H5)
save(loss.sim.H5, file="./Results/Mid/loss.sim.H5.RData")


# Fixed loss estimate

prod.sum <- yield.L[yr >= 2000, .(prod = sum(prod)), by=yr]
plot(prod.sum$yr, prod.sum$prod)
mean(prod.sum$prod)
ggplot(prod.sum) +
  geom_density(aes(prod/1e+06))

loss.Fix1 <- yield.L[ yr <= 2016, .(loss = sum(prod/(1-0.01))*0.01), by=.(yr)]
plot(loss.Fix1$yr, loss.Fix1$loss)
mean(loss.Fix1$loss)
ggplot(loss.Fix1) +
  geom_density(aes(loss/1e+06))

loss.Fix2 <- yield.L[ yr <= 2016, .(loss = sum(prod/(1-0.02))*0.02), by=.(yr)]
mean(loss.Fix2$loss)
ggplot(loss.Fix2) +
  geom_density(aes(loss/1e+06))

loss.Fix3 <- yield.L[ yr <= 2016, .(loss = sum(prod/(1-0.03))*0.03), by=.(yr)]
mean(loss.Fix3$loss)



######################################
# Resutls summary
library(data.table)
library(ggplot2)

# 1961-2016

load("./Results/Mid/loss.sim.ori.RData")
load("./Results/Mid/loss.sim.H.RData")
load("./Results/Mid/loss.sim.H1.RData")
load("./Results/Mid/loss.sim.H3.RData")


# All data

# 2000-2015
dat.loss.sim.ori <- data.table(melt(data.frame(t(loss.sim.ori[40:90,]/1e+06)), variable.name="xYr"), Scenario = "Baseline") 
dat.loss.sim.H  <- data.table(melt(data.frame(t(loss.sim.H[40:90,])/1e+06), variable.name="xYr"), Scenario = "High Loss") 

dat.loss.sim.H1  <- data.table(melt(data.frame(t(loss.sim.H1[40:90,]/1e+06)), variable.name="xYr"), Scenario = "High Loss 1") 
dat.loss.sim.H3  <- data.table(melt(data.frame(t(loss.sim.H3[40:90,]/1e+06)), variable.name="xYr"), Scenario = "High Loss 3") 

df.loss.sim <- rbind(dat.loss.sim.ori,  dat.loss.sim.H, dat.loss.sim.H1, dat.loss.sim.H3)

# Summary
df.loss.sim[, .(Q10 = quantile(value, 0.1),
                Q50 = quantile(value, 0.5),
                Q80 = quantile(value, 0.8),
                Q95 = quantile(value, 0.95),
                Avg = mean(value)), by=Scenario]

# Plot
ggplot(df.loss.sim[Scenario %in% c("Baseline", "High Loss")]) +
  geom_line(aes(value, color=Scenario), stat="density", size=1) +
  scale_color_manual(values=c("#636363",  "#e31a1c")) +
  xlab("Global Yearly Leaf Rust Losses (million metric ton)") +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8))

### Extra results

# Column means (Average yearly loss)
# Baseline loss distribution

yr.avg.b <- colMeans(loss.sim.ori[1:56, ])
summary(yr.avg.b )
quantile(yr.avg.b, probs = c(0.1, 0.50, 0.8, 0.95))

# High Loss distribution
yr.avg.H <- colMeans(loss.sim.H[1:56, ])
summary(yr.avg.H)
quantile(yr.avg.H, probs = c(0.1, 0.50, 0.8, 0.95))

# Alternative High Loss distribution
yr.avg.H1 <- colMeans(loss.sim.H1[1:56, ])
summary(yr.avg.H1)
quantile(yr.avg.H1, probs = c(0.1, 0.50, 0.8, 0.95))

yr.avg.H3 <- colMeans(loss.sim.H3[1:56, ])
summary(yr.avg.H3)
quantile(yr.avg.H3, probs = c(0.1, 0.50, 0.8, 0.95))

# yr.avg.H5 <- colMeans(loss.sim.H5[1:56, ])
# summary(yr.avg.H5)
# quantile(yr.avg.H5, probs = c(0.1, 0.50, 0.8, 0.95))


# Row means
# Baseline loss distribution
yr.avg.r.b <- rowMeans(loss.sim.ori[1:56, ])
summary(yr.avg.r.b )
quantile(yr.avg.r.b, probs = c(0.1, 0.50, 0.8, 0.95))

# High Loss distribution
yr.avg.r.H <- rowMeans(loss.sim.H[1:56, ])
summary(yr.avg.r.H)
quantile(yr.avg.r.H, probs = c(0.1, 0.50, 0.8, 0.95))

# Alternative High Loss distribution
yr.avg.r.H1 <- rowMeans(loss.sim.H1[1:56, ])
summary(yr.avg.r.H1)
quantile(yr.avg.r.H1, probs = c(0.1, 0.50, 0.8, 0.95))

yr.avg.r.H3 <- rowMeans(loss.sim.H3[1:56, ])
summary(yr.avg.r.H3)
quantile(yr.avg.r.H3, probs = c(0.1, 0.50, 0.8, 0.95))
# Date: 2020-06-01
# R codes used for the following paper: 
# Chai, Y., Pardey, P.G., Hurley, T.M., Senay, S.D., and Beddow, J.M. (2020) A Probabilistic Bio-economic Assessment of the Global Consequences of Wheat Leaf Rust" Submitted to Phytopathology
# Contact: chaix026@umn.edu

# Clear memory
rm(list=ls())

#### libraries ####
library(data.table)
library(ggplot2)
library(foreign)
library(ggthemes)
library(RColorBrewer)
library(maps)
library(geofacet)
library(corrplot)

#### Read data ####
WheatRustCDL.DF <- read.table("./CDL data/WheatRustCDL20180302.csv", header=TRUE, sep=",")
WheatRustCDL.DF[is.na(WheatRustCDL.DF)] <- 0
LeafRustCDL <- as.data.table(WheatRustCDL.DF[, c("Year", "WheatType", "State", "Prod1000Bu", "LeafPer")])
rm(WheatRustCDL.DF)

#### Data analysis ####
# Transform per loss into 1000bushel losses
LeafRustCDL[ , LRfree1000Bu:= Prod1000Bu/(100-LeafPer)*100]  #Leaf rust free production in 1000 bushels
LeafRustCDL[ , Leaf1000Bu:= LRfree1000Bu*LeafPer/100]  #Leaf rust losses in 1000 bushels
u.bu2tonne <- 36.744  # 1 tonne = 36.744 bushels

# **********************************************
# Aggregate by Year, then calculate the % losses
LeafRust.bYr <- LeafRustCDL[, .(Prod1000Bu=sum(LRfree1000Bu), Leaf1000Bu=sum(Leaf1000Bu)),
                            by=.(Year)
                            ] [, LeafPer:=Leaf1000Bu/Prod1000Bu*100] #Aggregate, then calculate % 
LeafRust.bYr[, `:=`(ProdTonne=Prod1000Bu*1000/u.bu2tonne, LeafTonne=Leaf1000Bu*1000/u.bu2tonne)]
setkey(LeafRust.bYr, Year)
plot(LeafRust.bYr$LeafPer, type="h")
hist(LeafRust.bYr$LeafPer)
LeafRust.bYr[LeafPer>=4, ]

# Export to .csv for STATA beta estimation
save(LeafRust.bYr, file="./Results/Mid/LeafRust.bYr.RData")
write.dta(LeafRust.bYr[, .(rustyear=Year, lrprop=LeafPer/100)], file="./Results/Mid/LeafRustbYr_Beta.dta")


# ***********************************************
# Aggregate by state by year, then calculate the % losses
LeafRust.bSt.bYr <- LeafRustCDL[, .(Prod1000Bu=sum(LRfree1000Bu), Leaf1000Bu=sum(Leaf1000Bu)),
                                by=.(Year, State)
                                ] [, LeafPer:=Leaf1000Bu/Prod1000Bu*100] #Aggregate, then calculate % 
LeafRust.bSt.bYr[, `:=`(ProdTonne=Prod1000Bu*1000/u.bu2tonne, LeafTonne=Leaf1000Bu*1000/u.bu2tonne)]
rm(LeafRustCDL)
save(LeafRust.bSt.bYr, file="./Results/Mid/LeafRust.bSt.bYr.RData")

#Number of States having >0 leaf rust losses
LeafRust.NSt <- LeafRust.bSt.bYr[, .(NSt=sum(LeafPer>0)), by=Year]
setkey(LeafRust.NSt, Year)
plot(LeafRust.NSt, type="l")

LeafRust.NSt[, .(AvgNSt=mean(NSt)), by=.(Period=factor(Year<1970, levels=c(TRUE, FALSE), labels=c("Pre1970", "Post1970")))]

#Export data for excel
write.csv(LeafRust.bYr[LeafRust.NSt], file="LeafRust.bYr.NSt.csv", row.names = FALSE)


#### Plots ####
dat.plot <- as.data.table(as.data.frame(LeafRust.bSt.bYr))
dat.plot[LeafPer > 30, LeafPer := 30]

dat.plot.sum <- LeafRust.bSt.bYr[, .(Nyr1 = sum(LeafPer >= 2)/(.N), Nyr5 = sum(LeafPer >=5)/(.N),
                                     maxPer = max(LeafPer), avgPer = mean(LeafPer)), by=.(State)]
dat.plot.sum[, LR.suit := "Low"]
dat.plot.sum[ Nyr1 >= 0.2 , LR.suit := "High"]
dat.plot.sum$LR.suit <- factor(dat.plot.sum$LR.suit, levels=c("High", "Low"))

dat.plot[, LRLossGroup := cut(LeafTonne, breaks = c(0, 25000, +Inf),
                              include.lowest = TRUE,
                              labels = c("<= 25", "> 25"))]
  
# map grid

# grid customization
my_grid <- us_state_grid1
my_grid <- my_grid[(my_grid$name %in% dat.plot.sum$State), ]
# grid_preview(my_grid)

my_grid$col[my_grid$code == "DE"] <- 9
my_grid$row[my_grid$code == "DE"] <- 5
my_grid$row[my_grid$code == "FL"] <- 6
my_grid$col[my_grid$code == "TX"] <- 3
my_grid$row[my_grid$code == "TX"] <- 6

my_grid$col[my_grid$code == "WI"] <- 7
my_grid$row[my_grid$code == "WI"] <- 2
my_grid$col[my_grid$code == "MI"] <- 8

my_grid$row <- my_grid$row - 1

colPal <- c("#1a9641",  "#d7191c")

ggplot(data=dat.plot) +
  geom_bar(aes(x=Year, y=LeafPer), stat="identity", color="#d7191c") +
  ylab("Percent Yield Loss") +
  xlab("") +
  theme_bw() +
  theme(text=element_text(size=14, family = "Arial"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(face="bold"),
        strip.text.x = element_text(colour = "black", face="bold"))+
  facet_geo(~State, grid = my_grid, label = "code")

ggsave("./Figures/Fig2.tiff", device="tiff", width = 7, height = 4, units = "in", scale=1.5)



# #### Correlation analysis ####
# 
# # By state
# dat.cor <- expand.grid(State = unique(LeafRust.bSt.bYr$State), Year = 1918:2016)
# dat.cor <- merge(dat.cor, LeafRust.bYr[, .(Year, Avg.US = LeafPer)])
# dat.cor <- merge(dat.cor, LeafRust.bSt.bYr[, .(Year, State, LeafPer)], by=c("Year", "State"), all.x=TRUE)
# dat.cor[is.na(dat.cor)] <- 0
# dat.cor <- as.data.table(dat.cor)
# # dat.cor[, Avg.Yr := mean(LeafPer, na.rm = T), by=.(State)]
# 
# dat.cor.St.US <- dat.cor[, .(Cor.St.US=cor(LeafPer, Avg.US)), by=State]
# # dat.cor.St.Yr <- dat.cor[, .(Cor.St.Yr=cor(LeafPer, Avg.Yr)), by=Year]
# 
# 
# # In Matrix
# dat.cor <- expand.grid(State = unique(LeafRust.bSt.bYr$State), Year = 1918:2016)
# dat.cor <- merge(dat.cor, LeafRust.bSt.bYr[, .(Year, State, LeafPer)], by=c("Year", "State"), all.x=TRUE)
# dat.cor <- rbind(dat.cor, LeafRust.bYr[, .(Year, State="Total US", LeafPer)])
# 
# dat.cor.wd <- dcast(dat.cor, Year ~ State, value.var = "LeafPer")
# dat.cor.wd[is.na(dat.cor.wd)] <- 0
# 
# # Correlation matrix
# dat.cor.wd <- dat.cor.wd[, !c("Year"), with=FALSE]
# M <- round(cor(dat.cor.wd, use="pairwise.complete.obs"), 2)
# M[is.na(M)] <- 0
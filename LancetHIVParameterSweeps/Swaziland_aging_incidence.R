##################################################################################################
#Swaziland EMOD plotting
##################################################################################################
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(GGally)
library(dplyr)
library(mgcv)
library(data.table)
library(tidyr)
library(matrixStats)
library(stringr)

options(scipen=999)

# baseline sweeps

###########################################
#Load scenarios
###########################################

#Long file
setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\HIV\\EMOD\\Swaziland_05June2018\\Scenarios_calibration_run3_v05Sep2018_newcampaign_FINAL\\ScenariosOct_FINAL")
reporthivbyageandgender.master.final <- read.csv("reporthivbyageandgender.master.final.csv")

###########################################
#Age-specific incidence over time
###########################################

#Calculate Incidence overall
reporthivbyageandgender.master.final$Year2 <- floor((reporthivbyageandgender.master.final$Year-0.5))
reporthivbyageandgender.master.final$Uninfected.Population = reporthivbyageandgender.master.final$Population-reporthivbyageandgender.master.final$Infected
head(reporthivbyageandgender.master.final)
trajectories_IR.1a <- aggregate(Newly.Infected ~ Age+Gender+Year2+sim.id+scenario, subset(reporthivbyageandgender.master.final, Age>10), FUN=sum) #sums number of new infections in each year
trajectories_IR.2 <- aggregate(Uninfected.Population ~ Age+Gender+Year+sim.id+scenario, subset(reporthivbyageandgender.master.final, Age>10), FUN=sum)
trajectories_IR.2$Year2 <- floor(trajectories_IR.2$Year-0.5)
trajectories_IR.2 <- trajectories_IR.2[!duplicated(trajectories_IR.2[c("Age","Gender","Year2","sim.id","scenario")]),] #remove second instance of duplicate rows
trajectories_IR.2 <- trajectories_IR.2[-match("Year",names(trajectories_IR.2))]
trajectories_IRoverall <- merge(trajectories_IR.1a, trajectories_IR.2, by=c("Age","Gender","Year2","sim.id","scenario"))
trajectories_IRoverall$incidence <- trajectories_IRoverall$Newly.Infected / trajectories_IRoverall$Uninfected.Population
summary(trajectories_IRoverall$incidence[trajectories_IRoverall$scenario=="baseline"])

#Calibration plot
year=2050
swaziland.inc <- ggplot(data=subset(trajectories_IRoverall, Year2 < year & scenario=="baseline")) +
  geom_point(data=subset(trajectories_IRoverall,  Year2 < year & scenario=="baseline"), size=1.2, color = "grey", aes(x=Year2, y=incidence*100))+
  geom_line(data=subset(trajectories_IRoverall,  Year2 < year & scenario=="baseline"), color="grey",aes(x=Year2, y=incidence*100, group=sim.id)) +
  geom_smooth(aes(x=Year2, y=incidence*100),method="loess", span=0.1, se = T, size=1, color="blue", linetype=1) +
  geom_point(data = subset(incidence.data, Gender==2 & Year==2011), size=2, color = "black", aes(x=Year, y=Incidence))+
  geom_point(data = subset(incidence.data, Gender==2 & Year==2016), size=2, color = "black", aes(x=Year, y=Incidence))+
  geom_errorbar(data = subset(incidence.data, Gender==2 & Year==2011), aes(x=Year, ymin=lb, ymax=ub), color="black", width=2, size=1) +
  geom_errorbar(data = subset(incidence.data, Gender==2 & Year==2016), aes(x=Year, ymin=lb, ymax=ub), color="black", width=2, size=1) +
  xlab("Year")+
  ylab("Incidence (per 100 py)")+
  theme_bw(base_size=16) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_y_continuous(breaks = seq(0,5,1),limits=c(0,5),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1975,year,5),limits=c(1975,year+1),expand = c(0,0)) +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))
swaziland.inc

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Submission\\Lancet HIV\\Resubmission2\\Figures")
ggsave("swaziland.incidence.5scenarios_FINAL.pdf", height=8, width=8)

##################################################################################################
#Swaziland EMOD plotting
#Plot model output
##################################################################################################
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(GGally)
library(plyr)
library(mgcv)
library(proto)
library(psych)
library(tidyr)

options(scipen=999)

#set working dir
wd <- "C:\\Users\\aakullian\\Documents\\GitHub\\EMOD_eswatini\\Calibration\\Data"
setwd(wd)
###########################################################################################################
#Bring in incidence reference data
incidence.data <- read.csv(paste0('./', "Swaziland_Incidence_Data", '.csv'))

#Bring in prevalence reference data
prevalence <- read.csv(paste0('./', "SWAZILAND_nationalprevalence", '.csv'))
prevalence$Age <- prevalence$start.age
prevalence.bothgenders <- read.csv(paste0('./', "SWAZILAND_nationalprevalence_bothgenders", '.csv'))
head(prevalence.bothgenders)

#Bring in ART coverage data
artdata <- read.csv(paste0('./', "SWAZILAND_calibration_nationalARTprevalence", '.csv'))
artdata$Agecat <- artdata$AgeBin
artdata$ART_coverage <- artdata$NationalARTPrevalence
labs <- c("1" = "Women", "0" = "Men")

###########################################
#Stich together output files for each of 250 sims
###########################################

# #baseline
# files <- list.files(path = '../Output/Baseline/ReportHIVByAgeAndGender', full.names = F) #list all the output files
# f <- paste0('../Output/Baseline/ReportHIVByAgeAndGender/', files)
# names(reporthivbyageandgender)
# 
# for (i in seq(1,250,1)){
#   reporthivbyageandgender <- read.csv(paste0(f[i]))
#   reporthivbyageandgender <- reporthivbyageandgender[,c(1,3,4,7,8,9,10,11,13)]
#   reporthivbyageandgender$scenario <- "baseline"
#   reporthivbyageandgender$sim.id <- paste0(files[i])
#   if (i==1){
#     reporthivbyageandgender.master <- reporthivbyageandgender
#   } else {
#     reporthivbyageandgender.master <- rbind(reporthivbyageandgender.master, reporthivbyageandgender)
#     print(paste0("I'm working on baseline folder",i))
#   }
# }
# reporthivbyageandgender.master.baseline <- reporthivbyageandgender.master
# 
# write.csv(reporthivbyageandgender.master.baseline, "reporthivbyageandgender.master.baseline.csv")
# 

###########################################
#Calculate incidence 
###########################################
reporthivbyageandgender.master.final <- read.csv("reporthivbyageandgender.master.baseline.csv")
#Calculate Incidence overall
reporthivbyageandgender.master.final$Year2 <- floor((reporthivbyageandgender.master.final$Year-0.5))
reporthivbyageandgender.master.final$Uninfected.Population = reporthivbyageandgender.master.final$Population-reporthivbyageandgender.master.final$Infected
trajectories_IR.1a <- aggregate(Newly.Infected ~ Year2+sim.id+scenario, subset(reporthivbyageandgender.master.final, Age>10 & Age<50), FUN=sum) #sums number of new infections in each year
trajectories_IR.2 <- aggregate(Uninfected.Population ~ Year+sim.id+scenario, subset(reporthivbyageandgender.master.final, Age>10 & Age<50), FUN=sum)
trajectories_IR.2$Year2 <- floor(trajectories_IR.2$Year-0.5)
trajectories_IR.2 <- trajectories_IR.2[!duplicated(trajectories_IR.2[c("Year2","sim.id","scenario")]),] #remove second instance of duplicate rows
trajectories_IR.2 <- trajectories_IR.2[-match("Year",names(trajectories_IR.2))]
trajectories_IRoverall <- merge(trajectories_IR.1a, trajectories_IR.2, by=c("Year2","sim.id","scenario"))
trajectories_IRoverall$incidence <- trajectories_IRoverall$Newly.Infected / trajectories_IRoverall$Uninfected.Population
trajectories_IRoverall$Gender = 2
head(trajectories_IRoverall)

#Calculate incidence by gender and append to overall
trajectories_IR.1a <- aggregate(Newly.Infected ~ Year2+Gender+sim.id+scenario, subset(reporthivbyageandgender.master.final, Age>10 & Age<50), FUN=sum) #sums number of new infections in each year
trajectories_IR.2 <- aggregate(Uninfected.Population ~ Year+Gender+sim.id+scenario, subset(reporthivbyageandgender.master.final, Age>10 & Age<50), FUN=sum)
trajectories_IR.2$Year2 <- floor(trajectories_IR.2$Year-0.5)
trajectories_IR.2 <- trajectories_IR.2[!duplicated(trajectories_IR.2[c("Year2","Gender","sim.id","scenario")]),] #remove second instance of duplicate rows
trajectories_IR.2 <- trajectories_IR.2[-match("Year",names(trajectories_IR.2))]
trajectories_IR <- merge(trajectories_IR.1a, trajectories_IR.2, by=c("Year2","Gender","sim.id","scenario"))
trajectories_IR$incidence <- trajectories_IR$Newly.Infected / trajectories_IR$Uninfected.Population
head(trajectories_IR)

trajectories_IR_comb <- rbind(trajectories_IR, trajectories_IRoverall)
table(trajectories_IR_comb$Gender)

#save incidence data
write.csv(trajectories_IR_comb, "EmodIncidence250Sims.csv")

#Bring in data:
trajectories_IR_comb <- read.csv("EmodIncidence250Sims.csv")

#get smoothed values
year=2050
baseline.smooth <- subset(trajectories_IR_comb, Year2 <= year & scenario=="baseline" & Gender==2)
baseline.smooth.m <- subset(trajectories_IR_comb, Year2 <= year & scenario=="baseline" & Gender==0)
baseline.smooth.f <- subset(trajectories_IR_comb, Year2 <= year & scenario=="baseline" & Gender==1)

inc.smooth_vals = data.frame(cbind("incidence"= predict(loess(incidence~Year2,baseline.smooth, span=0.3), data.frame(Year2=seq(1980,2050,1))), "year" = seq(1980,2050,1), "gender"="Combined"))
inc.smooth_vals$incidence <- as.numeric(as.character(inc.smooth_vals$incidence))
inc.smooth_vals$year <- as.numeric(as.character(inc.smooth_vals$year))

inc.smooth_vals.m = data.frame(cbind("incidence"= predict(loess(incidence~Year2,baseline.smooth.m, span=0.3), data.frame(Year2=seq(1980,2050,1))), "year" = seq(1980,2050,1), "gender"="Men"))
inc.smooth_vals.m$incidence <- as.numeric(as.character(inc.smooth_vals.m$incidence))
inc.smooth_vals.m$year <- as.numeric(as.character(inc.smooth_vals.m$year))

inc.smooth_vals.f = data.frame(cbind("incidence"= predict(loess(incidence~Year2,baseline.smooth.f, span=0.3), data.frame(Year2=seq(1980,2050,1))), "year" = seq(1980,2050,1), "gender"="Women"))
inc.smooth_vals.f$incidence <- as.numeric(as.character(inc.smooth_vals.f$incidence))
inc.smooth_vals.f$year <- as.numeric(as.character(inc.smooth_vals.f$year))

inc.smooth_vals.c <- rbind(inc.smooth_vals, inc.smooth_vals.m, inc.smooth_vals.f)
table(inc.smooth_vals.c$gender)

#write data frames
write.csv(inc.smooth_vals.c, "EmodIncidenceSmoothedSims.csv")

#Calibration plot
head(inc.smooth_vals.c)
inc.smooth_vals.c$year <- as.numeric(inc.smooth_vals.c$year)
inc.smooth_vals.c$incidence <- as.numeric(inc.smooth_vals.c$incidence)
ggplot(data=inc.smooth_vals.c,aes(x=year, y=incidence*100, group=gender, color=gender)) +
  geom_line() +
  xlab("Year")+
  ylab("Incidence (per 100 py)")+
  theme_bw(base_size=16) +
  theme(legend.position="bottom") 

#Calibration plot with sims by gender
head(trajectories_IR_comb)
class(trajectories_IR_comb$Gender)
trajectories_IR_comb$Gender <- as.factor(trajectories_IR_comb$Gender)
labs = c("Male","Female","Combined")
p <- ggplot(data=trajectories_IR_comb,aes(x=Year2, y=incidence*100, group=interaction(Gender, sim.id), color=Gender)) +
  geom_line(alpha=0.05) +
  geom_smooth(aes(x=Year2, y=incidence*100, color=Gender, group=Gender),method="loess", span=0.1, se = T, size=1, linetype=1) +
  scale_color_manual(values = c("blue","red","purple"), labels=labs) +
  scale_y_continuous(breaks = seq(0,6,1),limits=c(0,6),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1980,year,5),limits=c(1980,year),expand = c(0,0)) +
  xlab("Year")+
  ylab("Incidence (per 100 py)")+
  theme_bw(base_size=16) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))+
  theme(legend.position="bottom") 
p

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
names(swaziland.inc)

#get smoothed values
year=2050
baseline.smooth <- subset(trajectories_IRoverall, Year2 < year & scenario=="baseline")
smooth_vals = as.data.frame(cbind("incidence"= predict(loess(incidence~Year2,baseline.smooth, span=0.2)), "year" = baseline.smooth$Year2))
smooth_vals <- smooth_vals[!duplicated(smooth_vals[c("year","incidence")]),] #remove second instance of duplicate rows

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland.incidence.calib_FINAL_gatesreview.jpg", height=8, width=8)

year = 2051
span = 0.2
table(trajectories_IRoverall$scenario)
trajectories_IRoverall$scenario_f <- ordered(trajectories_IRoverall$scenario, levels=c("ART100pct","noARTnoVMMC","noART","909090","baseline"))
swaziland.inc <- ggplot() +
  geom_smooth(data=subset(trajectories_IRoverall, Year2 < year), aes(x=Year2, y=incidence*100, color=scenario_f),method="loess", span=span, se = F, size=2) +
  xlab("Year")+
  ylab("Incidence (per 100 py)")+
  theme_bw(base_size=24) +
  scale_color_manual(labels = c("100% ART uptake","No interventions", "VMMC only","Age-targeted 90-90-90", "Baseline"), values = c("green","red", "orange", "purple", "blue")) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  geom_hline(yintercept=0.1, linetype=2) +
  scale_y_continuous(breaks = seq(0,6,0.5),limits=c(0,5), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1975,year,5), limits=c(1975, year),expand = c(0,0)) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  guides(col = guide_legend(nrow=3)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))
swaziland.inc

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland.incidence.5scenarios_FINAL.jpg", height=8, width=8)


#Plot incidence by gender
table(trajectories_IR_comb$Gender)
labs <- c("2"="Both", "1" = "Women", "0" = "Men")
#Calibration plot
swaziland.inc <- ggplot(data=subset(trajectories_IR_comb,  Year2 < year & scenario=="baseline")) +
  geom_line(data=subset(trajectories_IR_comb,  Year2 < year & scenario=="baseline"), color="grey",aes(x=Year2, y=incidence*100, group=sim.id)) +
  facet_grid(~Gender, labeller=labeller(Gender = labs)) +
  stat_smooth(aes(x=Year2, y=incidence*100),method="loess", span=0.2, se = T, size=1, color="blue") +
  geom_point(data = subset(incidence.data, Gender < 2), size=2, color = "black", aes(x=Year, y=Incidence))+
  geom_errorbar(data = subset(incidence.data, Gender < 2), aes(x=Year, ymin=lb, ymax=ub), color="black", width=2, size=1) +
  xlab("Year")+
  ylab("Incidence (per 100 py")+
  theme_bw(base_size=16) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_y_continuous(breaks = seq(0,6,1),expand = c(0,0)) +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))
swaziland.inc

#Plot incidence by gender (all one plot)
table(trajectories_IR_comb$Gender)
labs <- c("2"="Both", "1" = "Women", "0" = "Men")
#Calibration plot
swaziland.inc <- ggplot(data=subset(trajectories_IR_comb,  Year2 < year & scenario=="baseline")) +
  geom_line(data=subset(trajectories_IR_comb,  Year2 < year & scenario=="baseline"),aes(x=Year2, y=incidence*100, group=sim.id, color=Gender)) +
  stat_smooth(aes(x=Year2, y=incidence*100),method="loess", span=0.2, se = T, size=1) +
  geom_point(data = subset(incidence.data, Gender < 2), size=2, color = "black", aes(x=Year, y=Incidence))+
  geom_errorbar(data = subset(incidence.data, Gender < 2), aes(x=Year, ymin=lb, ymax=ub, color=Gender), color="black", width=2, size=1) +
  xlab("Year")+
  ylab("Incidence (per 100 py")+
  theme_bw(base_size=16) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_y_continuous(breaks = seq(0,6,1),expand = c(0,0)) +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))
swaziland.inc

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland.incidence.bygender.calib_FINAL.jpg", height=8, width=12)

###########################################
#Incidence scenarios by gender
###########################################

#Incidenc by (4) scenarios
year = 2051
span = 0.2
table(trajectories_IR_comb$scenario)
trajectories_IR_comb$scenario_f <- ordered(trajectories_IR_comb$scenario, levels=c("ART100pct","noARTnoVMMC","noART","909090","baseline"))
swaziland.inc <- ggplot() +
  geom_smooth(data=subset(trajectories_IR_comb, Year2 < year), aes(x=Year2, y=incidence*100, color=scenario_f),method="loess", span=span, se = F, size=2) +
  xlab("Year")+
  ylab("Incidence (per 100 py)")+
  facet_grid(~Gender, labeller=labeller(Gender = labs)) +
  theme_bw(base_size=24) +
  scale_color_manual(labels = c("100% ART uptake","No interventions", "VMMC only","Age-targeted 90-90-90", "Baseline"), values = c("green","red", "orange", "purple", "blue")) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_y_continuous(breaks = seq(0,6,0.5),limits=c(0,5), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1975,year,5), limits=c(1975, year),expand = c(0,0)) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  guides(col = guide_legend(nrow=3)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))
swaziland.inc

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland.incidence.bygender.4scenarios.jpg", height=8, width=12)

###########################################
#Incidence scenarios by age and gender
###########################################
reporthivbyageandgender.master$Year2 <- floor((reporthivbyageandgender.master$Year-0.5))
reporthivbyageandgender.master$Uninfected.Population <- reporthivbyageandgender.master$Population - reporthivbyageandgender.master$Infected
trajectories_IR.1a <- aggregate(Newly.Infected ~ Year2+Gender+Agecat+sim.id+scenario, subset(reporthivbyageandgender.master), FUN=sum) #sums number of new infections in each year
trajectories_IR.2 <- aggregate(Uninfected.Population ~ Year+Gender+Agecat+sim.id+scenario, subset(reporthivbyageandgender.master), FUN=sum)
trajectories_IR.2$Year2 <- floor(trajectories_IR.2$Year-0.5)
trajectories_IR.2 <- trajectories_IR.2[!duplicated(trajectories_IR.2[c("Year2","Gender","Agecat", "sim.id","scenario")]),] #remove second instance of duplicate rows
trajectories_IR.2 <- trajectories_IR.2[-match("Year",names(trajectories_IR.2))]
trajectories_IR <- merge(trajectories_IR.1a, trajectories_IR.2, by=c("Year2","Gender","Agecat","sim.id","scenario"))
trajectories_IR$incidence <- trajectories_IR$Newly.Infected / trajectories_IR$Uninfected.Population

#Incidence by scenario
year1=2015 
year2=2050
table(trajectories_IR$Agecat)
labs.age <- c("[15,25)" = "15-24", "[25,35)" = "25-34", "[35,50)" = "35-49")
year = 2051
span = 0.2
trajectories_IR$scenario_f <- ordered(trajectories_IR$scenario, levels=c("ART100pct","noARTnoVMMC","noART","909090","baseline"))
swaziland.inc <- ggplot() +
  geom_smooth(data=subset(trajectories_IR, Year2 < year & scenario_f != "noARTnoVMMC" & scenario_f!="noART"), aes(x=Year2, y=incidence*100, color=scenario_f),method="loess", span=span, se = F, size=2) +
  xlab("Year")+
  ylab("Incidence (per 100 py)")+
  facet_grid(Gender~Agecat, labeller=labeller(Gender = labs, Agecat = labs.age)) +
  theme_bw(base_size=24) +
  scale_color_manual(labels = c("100% ART uptake","Age-targeted 90-90-90", "Baseline"), values = c("green", "purple", "blue")) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_y_continuous(breaks = seq(0,2.5,0.5),limits=c(0,2.6), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(year1,year2,5), limits=c(year1, year2+1),expand = c(0,0)) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  guides(col = guide_legend(nrow=1)) +
  #geom_hline(yintercept=0.1, linetype=2) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))
swaziland.inc

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland.incidence.byageandgender.2scenarios.jpg", height=12, width=10)

############################################
#Prevalence overall 15-49
############################################
#Prevalence 15-49 
reporthivbyageandgender.master2 <- subset(reporthivbyageandgender.master.final, scenario=="baseline")
table(reporthivbyageandgender.master2$scenario)
head(reporthivbyageandgender.master2)
trajectories_infections <- aggregate(Infected ~ Year+sim.id, subset(reporthivbyageandgender.master2, Age>=15 & Age<50), FUN=sum) #sums prev infections in each year
trajectories_population <- aggregate(Population ~ Year+sim.id, subset(reporthivbyageandgender.master2, Age>=15 & Age<50), FUN=sum)
trajectories_prev <- merge(trajectories_infections, trajectories_population, by=c("Year","sim.id"))
trajectories_prev$prevalence <- trajectories_prev$Infected / trajectories_prev$Population
head(prevalence.bothgenders)
head(trajectories_prev)

prevalence.bothgenders.c <- subset(prevalence.bothgenders, is.na(prevalence.bothgenders$Gender==T))
year=2050
swaziland.prev <- ggplot(data=subset(trajectories_prev, Year < year)) +
  geom_line(size=1.2, aes(x=Year, y=prevalence*100, group=sim.id), alpha=0.01,color="grey")+ 
  geom_smooth(aes(x=Year, y=prevalence*100),color="blue", method="loess", span=0.1, se = F, size=2) +
  geom_point(data = prevalence.bothgenders.c, size=2, color = "black", aes(x=Year, y=NationalPrevalence*100)) + 
  geom_errorbar(data = prevalence.bothgenders.c, aes(x=Year, ymin=lb*100, ymax=ub*100), color="black", width=1, size=1) +
  xlab("Year")+
  ylab("Prevalence")+
  theme_bw(base_size=16) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_x_continuous(breaks = seq(1980,2050,5)) +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white"))
swaziland.prev

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland.prevalence.calib.gender_baseline.jpg", height=8, width=15)

#############################################
#Prevalence by age and gender
#############################################
#Prevalence 15-49 by gender
reporthivbyageandgender.master2 <- subset(reporthivbyageandgender.master.final, scenario=="baseline")
table(reporthivbyageandgender.master2$scenario)
head(reporthivbyageandgender.master2)
trajectories_infections <- aggregate(Infected ~ Year+Gender+sim.id+scenario, subset(reporthivbyageandgender.master2, Age>10 & Age<50), FUN=sum) #sums prev infections in each year
trajectories_population <- aggregate(Population ~ Year+Gender+sim.id+scenario, subset(reporthivbyageandgender.master2, Age>10 & Age<50), FUN=sum)
trajectories_prev <- merge(trajectories_infections, trajectories_population, by=c("Year","Gender","sim.id","scenario"))
trajectories_prev$prevalence <- trajectories_prev$Infected / trajectories_prev$Population
head(prevalence.bothgenders)
head(trajectories_prev)

trajectories_infections.age <- aggregate(Infected ~ Year+Gender+Age+sim.id+scenario, subset(reporthivbyageandgender.master2, Age>10 & Age<100), FUN=sum) #sums prev infections in each year
trajectories_population.age <- aggregate(Population ~ Year+Gender+Age+sim.id+scenario, subset(reporthivbyageandgender.master2, Age>10 & Age<100), FUN=sum)
trajectories_prev.age <- merge(trajectories_infections.age, trajectories_population.age, by=c("Year","Age","Gender","sim.id","scenario"))
trajectories_prev.age$prevalence <- trajectories_prev.age$Infected / trajectories_prev.age$Population

prevalence.bothgenders.c <- prevalence.bothgenders[c(1:6),]
labs <- c("1" = "Women", "0" = "Men")
year=2050
swaziland.prev <- ggplot(data=subset(trajectories_prev, Year < year)) +
  geom_line(size=1.2, aes(x=Year, y=prevalence*100, group=sim.id), alpha=0.01,color="blue")+ 
  geom_smooth(aes(x=Year, y=prevalence*100),color="blue", method="loess", span=0.1, se = F, size=2) +
  geom_point(data = prevalence.bothgenders.c, size=2, color = "black", aes(x=Year, y=NationalPrevalence*100)) + 
  geom_errorbar(data = prevalence.bothgenders.c, aes(x=Year, ymin=lb*100, ymax=ub*100), color="black", width=1, size=1) +
  geom_vline(xintercept=2004, linetype=2) +
  geom_vline(xintercept=2020, linetype=2) +
  facet_grid(~Gender, labeller=labeller(Gender = labs)) +
  xlab("Year")+
  ylab("Prevalence")+
  theme_bw(base_size=16) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_x_continuous(breaks = seq(1980,year,5)) +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
swaziland.prev

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland.prevalence.calib.gender_baseline.jpg", height=8, width=15)

#HIV prev by age and gender
labs <- c("1" = "Women", "0" = "Men")
labs.age <- c("15"="15-19","20"="20-24","25"="25-29","30"="30-34","35"="35-39",
              "40"="40-44","45"="45-49","50"="50-54","55"="55-59","60"="60-64")
swaziland.prev <- ggplot(data=trajectories_prev.age) +
  geom_point(size=1.2, aes(x=Year, y=prevalence*100), alpha=0.01,color="blue")+ 
  geom_smooth(aes(x=Year, y=prevalence*100),color="blue", method="loess", span=0.03, se = F, size=2) +
  geom_point(data = prevalence, size=2, color = "black", aes(x=Year, y=NationalPrevalence*100)) + 
  geom_errorbar(data = prevalence, aes(x=Year, ymin=lb*100, ymax=ub*100), color="black", width=2, size=1) +
  facet_grid(Gender~Age, labeller=labeller(Gender = labs, Age=labs.age)) +
  xlab("Year")+
  ylab("Prevalence")+
  theme_bw(base_size=16) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  #scale_y_continuous(breaks = seq(0,3,0.5),limits=c(0,3)) +
  scale_x_continuous(breaks = seq(1980,2050,10)) +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white"))
swaziland.prev

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland.prevalence.calib.ageandgender_baseline.jpg", height=8, width=15)

#Swaziland prevalence over time by age and gender
labs <- c("1" = "Women", "0" = "Men")
head(prevalence)
head(trajectories_prev.age_year)
trajectories_prev.age_year <- subset(trajectories_prev.age, Year==2016|Year==2011)
table(trajectories_prev.age_year$Year)
swaziland.prev <- ggplot() +
  geom_point(data=subset(prevalence), size=2, aes(x=Age, y=NationalPrevalence*100, color=factor(Year))) + 
  geom_line(data=subset(prevalence), size=2, aes(x=Age, y=NationalPrevalence*100, color=factor(Year))) + 
  geom_errorbar(data=subset(prevalence), aes(x=Age, ymin=lb*100, ymax=ub*100,  color=factor(Year)), width=2, size=1) +
  geom_line(data=trajectories_prev.age_year,aes(x=Age, y=prevalence*100, color=factor(Year), group=sim.id), size=1, alpha=0.05) +
  scale_color_manual(values = c("blue","purple", "red")) +
  facet_grid(~Gender, labeller=labeller(Gender = labs)) +
  xlab("Year")+
  ylab("Prevalence")+
  theme_bw(base_size=20) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  theme(legend.title=element_blank())+
  scale_y_continuous(breaks = seq(0,60,10),limits=c(0,60),expand=c(0,0)) +
  #scale_x_continuous(breaks = seq(15,60,5),expand=c(0,0)) +
  theme(legend.position="bottom") +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
swaziland.prev

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland.prevalence.calib.ageandgender_baseline_byage.jpg", height=6, width=10)

##############################################
#ART coverage
################################################
names(reporthivbyageandgender.master)
table(reporthivbyageandgender.master$scenario)
table(reporthivbyageandgender.master$Agecat)
trajectory_ART_calib <- aggregate(cbind(On_ART, Infected) ~ Year+Agecat+Gender+sim.id+scenario,
                                  subset(reporthivbyageandgender.master, Year > 2000 & Year < 2050), FUN=sum)
trajectory_ART_calib$ART_coverage <- trajectory_ART_calib$On_ART / trajectory_ART_calib$Infected
trajectory_ART_calib.master<-trajectory_ART_calib
head(trajectory_ART_calib.master)
table(trajectory_ART_calib.master$Agecat)

#ART scale-up to 2016
trajectory_ART_calib.master.plot <- subset(trajectory_ART_calib.master,  (scenario=="baseline" | scenario=="909090" | scenario=="ART100pct" ))
head(trajectory_ART_calib.master.plot)
table(trajectory_ART_calib.master.plot$Agecat)
table(trajectory_ART_calib.master.plot$scenario)
year=2049
swaziland.ART.calib <- ggplot() +
  geom_smooth(data=trajectory_ART_calib.master.plot,
              aes(x=Year, y=ART_coverage*100, group = factor(Agecat), color=factor(Agecat)), method="loess",span=0.1, se=F, size=1.2) +
  geom_smooth(data=trajectory_ART_calib.master.plot,
              aes(x=Year, y=ART_coverage*100), color="black",linetype=2, method="loess",span=0.1, se=F, size=1.2) +
  geom_point(data=subset(artdata), aes(x=Year, y=ART_coverage*100,
                                       color=factor(Agecat)), size=2) +
  geom_errorbar(data = subset(artdata), aes(x=Year, ymin=lb*100, ymax=ub*100, color=factor(Agecat)), width=1, size=1.2) +
  facet_grid(scenario~Gender, labeller=labeller(Gender = labs)) +
  theme(legend.position="bottom") +
  #geom_hline(aes(yintercept=81)) +
  ylab("ART Coverage %")+
  theme_bw(base_size=20) +
  #scale_color_manual(labels = c("15-24", "25-34", "35-49"), values = c("blue", "purple", "red")) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,10), expand=c(0,0)) +
  scale_x_continuous(limits = c(2000,year), breaks = seq(2000,year+1,5), expand=c(0,0)) +
  theme(legend.position="bottom") +
  theme(legend.direction='vertical') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  guides(colour = guide_legend(nrow = 1, title=""))
swaziland.ART.calib

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland_ART_scaleup_by_scenario.jpg", height=12, width=9)

################################################
#ART by risk scenario
################################################
reporthivbyageandgender.master <- reporthivbyageandgender.master.final.risk
names(reporthivbyageandgender.master)
table(reporthivbyageandgender.master$scenario)
reporthivbyageandgender.master$Agecat<-cut(reporthivbyageandgender.master$Age, c(15,25,35,45), right=F)
trajectory_ART_calib <- aggregate(cbind(On_ART, Infected) ~ Year+Agecat+Gender+sim.id+scenario,
                                  subset(reporthivbyageandgender.master, Age>10 & Age<60 & Year > 2000 & Year < 2050), FUN=sum)
trajectory_ART_calib$ART_coverage <- trajectory_ART_calib$On_ART / trajectory_ART_calib$Infected
trajectory_ART_calib.master<-trajectory_ART_calib

#ART scale-up to 2016
year=2049
swaziland.ART.calib <- ggplot() +
  geom_smooth(data=trajectory_ART_calib.master,
              aes(x=Year, y=ART_coverage*100, group = factor(Agecat), color=factor(Agecat)), method="loess",span=0.1, se=F, size=1.2) +
  geom_smooth(data=trajectory_ART_calib.master,
              aes(x=Year, y=ART_coverage*100), color="black",linetype=2, method="loess",span=0.1, se=F, size=1.2) +
  geom_point(data=subset(artdata), aes(x=Year, y=ART_coverage*100,
                                       color=factor(Agecat)), size=2) +
  geom_errorbar(data = subset(artdata), aes(x=Year, ymin=lb*100, ymax=ub*100, color=factor(Agecat)), width=1, size=1.2) +
  facet_grid(scenario~Gender, labeller=labeller(Gender = labs)) +
  theme(legend.position="bottom") +
  #geom_hline(aes(yintercept=81)) +
  ylab("ART Coverage %")+
  theme_bw(base_size=20) +
  scale_color_manual(labels = c("15-24", "25-34", "35-44"), values = c("blue", "purple", "red")) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,10), expand=c(0,0)) +
  scale_x_continuous(limits = c(2000,year), breaks = seq(2000,year+1,5), expand=c(0,0)) +
  theme(legend.position="bottom") +
  theme(legend.direction='vertical') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  guides(colour = guide_legend(nrow = 1, title=""))
swaziland.ART.calib

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland_ART_scaleup_by_risk_scenario.jpg", height=12, width=9)

################################################
#Numbers on ART by scenario
#################################################
head(trajectory_ART_calib.master,200)
trajectory_ART_calib.master$Year2 <- floor((trajectory_ART_calib.master$Year-0.5))
trajectory_numbersonART <- aggregate(On_ART ~ Year2+Agecat+Gender+sim.id+scenario,
                                         subset(trajectory_ART_calib.master), FUN=sum)
trajectory_numbersonART$scenario_f = factor(trajectory_numbersonART$scenario, levels=c('ART100pct','909090',"baseline"))

table(trajectory_numbersonART$scenario_f)
head(trajectory_numbersonART)
year=2048
swaziland.ART.calib <- ggplot() +
  geom_smooth(data=subset(trajectory_numbersonART, Agecat=="[15,25)"),
              aes(x=Year2, y=On_ART*scale.factor, group = factor(scenario_f), color=factor(scenario_f)), method="loess",span=0.2, se=F, size=1.2) +
  geom_smooth(data=subset(trajectory_numbersonART, Agecat=="[25,35)"),
              aes(x=Year2, y=On_ART*scale.factor, group = factor(scenario_f), color=factor(scenario_f)), method="loess",span=0.2, se=F, size=1.2, linetype=2) +
  geom_smooth(data=subset(trajectory_numbersonART, Agecat=="[35,45)"),
              aes(x=Year2, y=On_ART*scale.factor, group = factor(scenario_f), color=factor(scenario_f)), method="loess",span=0.2, se=F, size=1.2, linetype=3) +
  facet_grid(~Gender, labeller=labeller(Gender = labs)) +
  theme(legend.position="bottom") +
  #geom_hline(aes(yintercept=81)) +
  xlab("Year")+
  ylab("Number on ART")+
  theme_bw(base_size=20) +
  scale_color_manual(labels = c("ART100pct", "909090", "baseline"), values = c("green", "purple", "blue")) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  #scale_y_continuous(limits=c(0,100), breaks = seq(0,100,10), expand=c(0,0)) +
  scale_x_continuous(limits = c(2000,year), breaks = seq(2000,year,5), expand=c(0,0)) +
  theme(legend.position="bottom") +
  theme(legend.direction='vertical') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  guides(colour = guide_legend(nrow = 1, title=""))
swaziland.ART.calib

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland_numbersonART.byage.gender2_scaleup_by_scenario.jpg", height=8, width=10)

#by gender
head(trajectory_ART_calib.master,200)
trajectory_ART_calib.master$Year2 <- floor((trajectory_ART_calib.master$Year-0.5))
trajectory_numbersonART <- aggregate(On_ART ~ Year2+Gender+sim.id+scenario,
                                     subset(trajectory_ART_calib.master), FUN=sum)
trajectory_numbersonART$scenario_f = factor(trajectory_numbersonART$scenario, levels=c('baseline','909090',"ART100pct"))
table(trajectory_numbersonART$scenario_f)
year=2048
swaziland.ART.calib <- ggplot() +
  geom_smooth(data=subset(trajectory_numbersonART, scenario_f=="909090"),
              aes(x=Year2, y=On_ART*scale.factor), method="loess",span=0.2, se=F, size=1.2, color="purple") +
  geom_smooth(data=subset(trajectory_numbersonART, scenario_f=="ART100pct"),
              aes(x=Year2, y=On_ART*scale.factor), method="loess",span=0.2, se=F, size=1.2, color="green") +
  facet_grid(~Gender, labeller=labeller(Gender = labs)) +
  geom_smooth(data=subset(trajectory_numbersonART, scenario_f=="baseline"),
              aes(x=Year2, y=On_ART*scale.factor), method="loess",span=0.2, se=F, size=1.2, color="blue") +
  theme(legend.position="bottom") +
  #geom_hline(aes(yintercept=81)) +
  ylab("Number on ART")+
  theme_bw(base_size=20) +
  #scale_color_manual(labels = c("15-24", "25-34", "35-44"), values = c("blue", "purple", "red")) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  #scale_y_continuous(limits=c(0,100), breaks = seq(0,100,10), expand=c(0,0)) +
  scale_x_continuous(limits = c(2000,year), breaks = seq(2000,year,5), expand=c(0,0)) +
  theme(legend.position="bottom") +
  theme(legend.direction='vertical') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  guides(colour = guide_legend(nrow = 1, title=""))
swaziland.ART.calib

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland_numbersonART.bygender2_scaleup_by_scenario.jpg", height=8, width=10)

###########################################
#Calculate mortality
###########################################

#Calculate Incidence and plot with calibration points
reporthivbyageandgender.master$Year2 <- floor((reporthivbyageandgender.master$Year-0.5))
trajectories_MR.1a <- aggregate(Died_from_HIV ~ Year2+sim.id+scenario, subset(reporthivbyageandgender.master), FUN=sum) #sums number of new deaths in each year
trajectories_MR.2 <- aggregate(Population ~ Year+sim.id+scenario, subset(reporthivbyageandgender.master), FUN=sum) #population at midyear
trajectories_MR.2$Year2 <- floor(trajectories_MR.2$Year-0.5)
trajectories_MR.2 <- trajectories_MR.2[!duplicated(trajectories_MR.2[c("Year2","sim.id","scenario")]),] #remove second instance of duplicate rows
trajectories_MR.2 <- trajectories_MR.2[-match("Year",names(trajectories_MR.2))]
trajectories_MRoverall <- merge(trajectories_MR.1a, trajectories_MR.2, by=c("Year2","sim.id","scenario"))
trajectories_MRoverall$mortality <- trajectories_MRoverall$Died_from_HIV / trajectories_MRoverall$Population
head(trajectories_MRoverall)

#Plot baseline mortality with calibration points
year=2050
#Calibration plot
swaziland.mortality <- ggplot(data=subset(trajectories_MRoverall, Year2 < year & scenario=="baseline")) +
  geom_point(data=subset(trajectories_MRoverall,  Year2 < year & scenario=="baseline"), size=1.2, color = "grey", aes(x=Year2, y=mortality*100))+
  geom_line(data=subset(trajectories_MRoverall,  Year2 < year & scenario=="baseline"), color="grey",aes(x=Year2, y=mortality*100, group=sim.id)) +
  geom_smooth(aes(x=Year2, y=mortality*100),method="loess", span=0.1, se = T, size=1, color="blue", linetype=1) +
  xlab("Year")+
  ylab("Mortality (per 100 py)")+
  theme_bw(base_size=16) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_y_continuous(breaks = seq(0,2,0.5),limits=c(0,2),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1975,year,5),limits=c(1975,year+1),expand = c(0,0)) +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))
swaziland.mortality

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland.mortality.baseline.jpg", height=8, width=8)

year1 = 1975
year2 = 2050
head(trajectories_MRoverall)
trajectories_MRoverall$scenario_f <- ordered(trajectories_MRoverall$scenario, levels=c("ART100pct","noARTnoVMMC","noART","909090","baseline"))
swaziland.mortality.scenario <- ggplot() +
  geom_smooth(data=subset(trajectories_MRoverall, Year2 < year), aes(x=Year2, y=mortality*100, color=scenario_f),method="loess", span=span, se = F, size=2) +
  xlab("Year")+
  ylab("Mortality (per 100 py)")+
  #facet_grid(Gender~Agecat, labeller=labeller(Gender = labs, Agecat = labs.age)) +
  theme_bw(base_size=24) +
  scale_color_manual(labels = c("100% ART uptake","No interventions", "VMMC only","Age-targeted 90-90-90", "Baseline"), values = c("green","red", "orange", "purple", "blue")) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_y_continuous(breaks = seq(0,3,0.5),limits=c(0,3), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(year1,year2,5), limits=c(year1, year2+1),expand = c(0,0)) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  guides(col = guide_legend(nrow=1)) +
  #geom_hline(yintercept=0.1, linetype=2) +
  guides(col = guide_legend(nrow=3)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))
swaziland.mortality.scenario

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland.mortality.by.scenario.jpg", height=8, width=8)

#Crude mortality numbers

table(trajectories_MRoverall$scenario)
names(trajectories_MRoverall)
swaziland.mort <- ggplot() +
  geom_smooth(data=subset(trajectories_MRoverall, Year2 < year), aes(x=Year2, y=Died_from_HIV*scale.factor, color=scenario_f),method="loess", span=span, se = F, size=2) +
  xlab("Year")+
  ylab("Annual deaths from HIV/AIDS")+
  theme_bw(base_size=24) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_color_manual(labels = c("100% ART uptake","No interventions", "VMMC only","Age-targeted 90-90-90", "Baseline"), values = c("green","red", "orange", "purple", "blue")) +
  scale_y_continuous(breaks = seq(0,20000,5000),limits=c(0,22000),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1980,year,5), limits=c(1980, year+1),expand = c(0,0)) +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  guides(col = guide_legend(nrow=3)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.title=element_blank(),
        panel.border = element_rect(colour = "black"))
swaziland.mort

setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\Manuscripts\\Ongoing Manuscripts Folder\\BMGF Lancet Paper\\Final_Plots")
ggsave("swaziland.deaths.5scenarios.jpg", height=8, width=8)


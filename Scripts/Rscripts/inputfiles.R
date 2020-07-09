##################################################################################################
#Bring in reportHIVbyageandgender Baseline and 100% ART scenario for Eswatini model with ART stages
##################################################################################################

#Bring in newest downloaded baseline transmission data + reportHIVbyageandgender from same sim folders
input_dir <- "C:\\Users\\aakullian\\Dropbox (IDM)\\GitHub\\EMOD_eswatini\\Output\\Baseline_transmission_ExtraData"
primary_dirs = list.files(input_dir)
transmission.data.frame <- data.frame("Year2"=seq(1980,2050,1))

#reporthivbyageandgender data
i=0
for(dir in primary_dirs) {
  i=i+1
  temp_table <- as.data.table(read.csv(file = paste(input_dir,dir,"ReportHIVByAgeAndGender.csv",sep="/"))) 
  temp_table$Year2 <- floor((temp_table$Year-0.5))
  temp_table$Uninfected.Population = temp_table$Population-temp_table$Infected
  trajectories_IR.1a <- aggregate(cbind(Newly.Infected,Transmitters) ~ Year2+Gender+Age+IsCircumcised+IP_Key.ARTstate, subset(temp_table, Age>10 & Year < 2051), FUN=sum) #sums number of new infections/transmissions in each year
  trajectories_IR.2 <- aggregate(cbind(Uninfected.Population,Infected,Population) ~ Year+Gender+Age+IsCircumcised+IP_Key.ARTstate, subset(temp_table, Age>10 & Year < 2051), FUN=sum) #sums at risk population in each year
  trajectories_IR.2$Year2 <- floor(trajectories_IR.2$Year-0.5)
  trajectories_IR.2 <- trajectories_IR.2[!duplicated(trajectories_IR.2[c("Year2","Gender", "Age", "IsCircumcised", "IP_Key.ARTstate")]),] #remove second instance of duplicate rows
  trajectories_IR.2 <- trajectories_IR.2[-match("Year",names(trajectories_IR.2))]
  trajectories_IRoverall <- merge(trajectories_IR.1a, trajectories_IR.2, by=c("Year2","Gender","Age","IsCircumcised","IP_Key.ARTstate"))
  trajectories_IRoverall$sim.id <- paste(dir)
  if (i == 1){
    transmission.data.frame <- trajectories_IRoverall
  }
  else{
    transmission.data.frame <- rbind(transmission.data.frame, trajectories_IRoverall)
  }
  print(paste("working on folder", i, dir,sep=" "))
}

reporthivbyageandgender.baseline <- transmission.data.frame

#transmission data
i=0
vars <-  c("SRC_ID", "SRC_GENDER", "DEST_ID", "DEST_GENDER", "SRC_STAGE","src_age_int","dest_age_int","Year2","YEAR")
for(dir in primary_dirs) {
  i=i+1
  temp_table <- as.data.table(read.csv(file = paste(input_dir,dir,"TransmissionReport.csv",sep="/"))) 
  temp_table <- subset(temp_table, YEAR>=1980 & YEAR <2050)
  temp_table$Year2 <- floor(temp_table$YEAR)
  temp_table$src_age_int <- floor(temp_table$SRC_AGE / 365.25)
  temp_table$dest_age_int <- floor(temp_table$DEST_AGE / 365.25)
  temp_table <- subset(temp_table, dest_age_int > 14 & src_age_int > 14)
  temp_table <- temp_table[, vars, with = FALSE]
  temp_table$sim.id <- paste(dir)
  if (i == 1){
    transmission.data.frame <- temp_table
  }
  else{
    transmission.data.frame <- rbind(transmission.data.frame, temp_table)
  }
  print(paste("working on folder", i, dir,sep=" "))
}

transmissionreport.master.baseline <- transmission.data.frame

#100% ART

#Bring in newest downloaded baseline transmission data + reportHIVbyageandgender from same sim folders
input_dir <- "C:\\Users\\aakullian\\Dropbox (IDM)\\GitHub\\EMOD_eswatini\\Output\\ART100pct_transmission_ExtraData"
primary_dirs = list.files(input_dir)
transmission.data.frame <- data.frame("Year2"=seq(1980,2050,1))

#reporthivbyageandgender data
i=0
for(dir in primary_dirs) {
  i=i+1
  temp_table <- as.data.table(read.csv(file = paste(input_dir,dir,"ReportHIVByAgeAndGender.csv",sep="/"))) 
  temp_table$Year2 <- floor((temp_table$Year-0.5))
  temp_table$Uninfected.Population = temp_table$Population-temp_table$Infected
  trajectories_IR.1a <- aggregate(cbind(Newly.Infected,Transmitters) ~ Year2+Gender+Age+IsCircumcised+IP_Key.ARTstate, subset(temp_table, Age>10 & Year < 2051), FUN=sum) #sums number of new infections/transmissions in each year
  trajectories_IR.2 <- aggregate(cbind(Uninfected.Population,Infected,Population) ~ Year+Gender+Age+IsCircumcised+IP_Key.ARTstate, subset(temp_table, Age>10 & Year < 2051), FUN=sum) #sums at risk population in each year
  trajectories_IR.2$Year2 <- floor(trajectories_IR.2$Year-0.5)
  trajectories_IR.2 <- trajectories_IR.2[!duplicated(trajectories_IR.2[c("Year2","Gender", "Age", "IsCircumcised", "IP_Key.ARTstate")]),] #remove second instance of duplicate rows
  trajectories_IR.2 <- trajectories_IR.2[-match("Year",names(trajectories_IR.2))]
  trajectories_IRoverall <- merge(trajectories_IR.1a, trajectories_IR.2, by=c("Year2","Gender","Age","IsCircumcised","IP_Key.ARTstate"))
  trajectories_IRoverall$sim.id <- paste(dir)
  if (i == 1){
    transmission.data.frame <- trajectories_IRoverall
  }
  else{
    transmission.data.frame <- rbind(transmission.data.frame, trajectories_IRoverall)
  }
  print(paste("working on folder", i, dir,sep=" "))
}

reporthivbyageandgender.ART100pct <- transmission.data.frame

#transmission data
i=0
vars <-  c("SRC_ID", "SRC_GENDER", "DEST_ID", "DEST_GENDER", "SRC_STAGE","src_age_int","dest_age_int","Year2","YEAR")
for(dir in primary_dirs) {
  i=i+1
  temp_table <- as.data.table(read.csv(file = paste(input_dir,dir,"TransmissionReport.csv",sep="/"))) 
  temp_table <- subset(temp_table, YEAR>=1980 & YEAR <2050)
  temp_table$Year2 <- floor(temp_table$YEAR)
  temp_table$src_age_int <- floor(temp_table$SRC_AGE / 365.25)
  temp_table$dest_age_int <- floor(temp_table$DEST_AGE / 365.25)
  temp_table <- subset(temp_table, dest_age_int > 14 & src_age_int > 14)
  temp_table <- temp_table[, vars, with = FALSE]
  temp_table$sim.id <- paste(dir)
  if (i == 1){
    transmission.data.frame <- temp_table
  }
  else{
    transmission.data.frame <- rbind(transmission.data.frame, temp_table)
  }
  print(paste("working on folder", i, dir,sep=" "))
}

transmissionreport.master.ART100pct <- transmission.data.frame

#combine into one reportHIV and one transmission datafile
reporthivbyageandgender.baseline$scenario = "baseline"
reporthivbyageandgender.ART100pct$scenario = "ART100pct"
transmissionreport.master.baseline$scenario = "baseline"
transmissionreport.master.ART100pct$scenario = "ART100pct"

reporthivbyageandgender <- rbind(reporthivbyageandgender.baseline, reporthivbyageandgender.ART100pct)
transmissionreport <- rbind(transmissionreport.master.baseline, transmissionreport.master.ART100pct)
head(transmissionreport)

#save an Rda data file for inputs: 
setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\GitHub\\EMOD_eswatini\\Output")
save(reporthivbyageandgender, transmissionreport, file = "inputfiles2.rda")
write.csv(reporthivbyageandgender, "reporthivbyageandgender.csv")
write.csv(transmissionreport, "transmissionreport.csv")



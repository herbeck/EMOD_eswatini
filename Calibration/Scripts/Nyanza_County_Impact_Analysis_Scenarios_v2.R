## Purpose: Analyze output from EMOD simulations at county level.
## Inputs:  csv files from mean of 10 replicates of each scenario over 250 best-fitting parameter sets
## Outputs: One dataset, comprising outcomes for each risk group (medium and high) and each county
## Date:    7 December 2018
## Author:  Kathryn Peebles

# Load packages
library(data.table)

# Set working directories
input_dir  <- "C:/Users/kpeebles/NyanzaCalib-2018-10-08/Nyanza--0.0525--rep3--resampling/Analysis_Calibrated_Scenarios_Increased_Replicates/Calibrated_Nyanza_Scenarios"
output_dir <- "C:/Users/kpeebles/Dropbox/ZimPrEP-LSHTM/Kenya/Analysis/UnivariateAnalysis"

# Acronyms and coding
# b = baseline, m = medium risk, h = high risk
# NodeID: 1 = Homa Bay, 2 = Kisii, 3 = Kisumu, 4 = Migori, 5 = Nyamira, 6 = Siaya

# Specify files to be loaded and analyzed. Reorder in numerical order.
files_b <- list.files(path = paste0(input_dir), pattern = "*.csv", full.names = T)

dt_b_list <- lapply(files_b, function(x) as.data.table(read.csv(file = x)))

n_replicates <- length(files_index_list[[1]]) # Number of replicates per grouped subset

constant_columns <- c('Age_Range', 'Gender', 'Risk')

impact_columns <- c('AI',
                   'PY_onPrEP',
                   'PY_onPrEP_per_AI',
                   'PrEP_Cov',
                   'Incidence',
                   'Population',
                   'HIVNegatives',
                   'PrEPL_Cov',
                   'SecondarytoPrimary_Ratio',
                   'Primary_Prevention',
                   'Secondary_Prevention',
                   'Expected_Newly_Infected_if_no_PrEP')

epidemic_columns <- c('Incidence_HighRisk',
                      'Incidence_All',
                      'Prevalence_HighRisk',
                      'Prevalence_All',
                      'ART_Coverage')

misc_columns <- c('FSW_Proportion_WF1549',
                  'VMMC_Coverage1549',
                  'VMMC_Coverage_ALL15492007',
                  'VMMC_Coverage_ALL15492018')

# Complete calculations for each node
node_ids   <- 1:6
node_names <- c("Homa_Bay", "Kisii", "Kisumu", "Migori", "Nyamira", "Siaya")

# Evalute PrEP impact for 3 time periods: 2018-2023, 2018-2028, 2018-2038
years       <- list(c(2018, 2023), c(2018, 2028), c(2018, 2038))
year_suffix <- c("_5yrs", "_10yrs", "_20yrs")

# Specify age and sex groups
age_groups <- list(c(15, 20), c(20, 25), c(15, 50)) # [15, 20), [20, 25), [15, 50)
age_names  <- c("1520", "2025", "1549")

sex_groups <- c(0, 1) # "Male", "Female"
sex_names  <- c("Male", "Female")

for(g in 1:length(files_index_list)) {
  load(paste0(input_dir, "/dt_b_list_", g, ".RDATA"))
  load(paste0(input_dir, "/dt_m_list_", g, ".RDATA"))
  load(paste0(input_dir, "/dt_h_list_", g, ".RDATA"))
  
  for(risk in c("MEDIUM", "HIGH")) {
    for(i in node_ids) {
      
      # Estimate impact of intervention over 5-, 10-, and 20-year intervals
      for(k in 1:length(years)) {
        
        # Create table to store results
        dt <- data.table(sapply(impact_columns, function(x) x = rep(NA_real_, n_replicates)))
        
        for(j in 1:length(dt_b_list)) {
          
          # Extract baseline simulation j
          dt_b <- dt_b_list[[j]]
          
          # Assign appropriate risk results for simulation j
          if(risk == "MEDIUM") { dt_r <- dt_m_list[[j]] } else { dt_r <- dt_h_list[[j]] }
          
          # Subset data tables to county corresponding to i index
          dt_b <- dt_b[NodeId == i]
          dt_r <- dt_r[NodeId == i]
          
          # Subset risk data table into three data tables for ease of calculations below. 1) specified year range 2) specified year range, gender, and risk 3) specified year range, gender, risk, and age
          dt_r_time        <- dt_r[Year > years[[k]][1] & Year <= years[[k]][2]]
          dt_r_timepop     <- dt_r[Year > years[[k]][1] & Year <= years[[k]][2] & Gender == 1 & IP_Key.Risk == risk]
          dt_r_timepop_age <- dt_r[Year > years[[k]][1] & Year <= years[[k]][2] & Gender == 1 & IP_Key.Risk == risk & Age >= 15 & Age <= 49]
          
          # Estimate impact over first yearly periods
          dt[j, AI := dt_b[Year > years[[k]][1] & Year <= years[[k]][2], sum(Newly.Infected)] - dt_r_time[, sum(Newly.Infected)]]
          
          # Estimate person-years on PrEP per infection averted in given time period. Gender is specified in Zin's analysis script, so I've included it here in subsets above, as well; however, targeting strategies are specifically to women, so shouldn't be necessary to include "female" as a subsetting criteria.
          dt[j, PY_onPrEP := (dt_r_timepop[HasIntervention.PrEP_Or_Placebo == 1, sum(Population)] - dt_r_timepop[HasIntervention.PlaceboPrEP == 1, sum(Population)])]
          dt[j, PY_onPrEP_per_AI := dt[j, PY_onPrEP]/dt[j, AI]]
          
          # Estimate PrEP coverage in yearly periods
          dt[j, Population   := dt_r_timepop_age[, sum(Population)]]
          dt[j, HIVNegatives := dt[j, Population] - dt_r_timepop_age[, sum(Infected)]]
          dt[j, PrEP_Cov     := 100 * (dt_r_timepop[HasIntervention.PrEP_Or_Placebo == 1, sum(Population)] - dt_r_timepop[HasIntervention.PlaceboPrEP == 1, sum(Population)])/dt[j, HIVNegatives]]
          dt[j, PrEPL_Cov    := 100 * (dt_r_timepop[HasIntervention.PlaceboPrEP == 1, sum(Population)])/dt[j, HIVNegatives]]
          
          # Estimate HIV incidence
          dt[j, Incidence := dt_r_timepop_age[, sum(Newly.Infected)]/dt[j, HIVNegatives]]
          
          # Calculate the primary-to-secondary ratio.
          # Calculate the expected number of new infections in the absence of PrEP as the number of HIV-negative individuals on PrEP multiplied by cumulative incidence among those on placebo PrEP.
          dt[j, Expected_Newly_Infected_if_no_PrEP := (dt_r_timepop[HasIntervention.PrEP_Or_Placebo == 1, sum(Population) - sum(Infected)] - dt_r_timepop[HasIntervention.PlaceboPrEP == 1, sum(Population) - sum(Infected)]) * dt_r_timepop[HasIntervention.PlaceboPrEP == 1, sum(Newly.Infected)]/dt_r_timepop[HasIntervention.PlaceboPrEP == 1, sum(Population) - sum(Infected)]]
          # Version for Edinah: dt[j, Expected_Newly_Infected_if_no_PrEP := (dt_r_timepop[HasIntervention.EffectivePrEP == 1, sum(Population) - sum(Infected)]) * dt_r_timepop[HasIntervention.PlaceboPrEP == 1, sum(Newly.Infected)]/dt_r_timepop[HasIntervention.PlaceboPrEP == 1, sum(Population) - sum(Infected)]]
          
          # Calculate primary prevention as the expected number of new infections in the absence of PrEP minus the number of new infections that occurred among people on PrEP.
          dt[j, Primary_Prevention := Expected_Newly_Infected_if_no_PrEP - (dt_r_timepop[HasIntervention.PrEP_Or_Placebo == 1, sum(Newly.Infected)] - dt_r_timepop[HasIntervention.PlaceboPrEP == 1, sum(Newly.Infected)])]
          # Version for Edinah: dt[j, Primary_Prevention := Expected_Newly_Infected_if_no_PrEP - (dt_r_timepop[HasIntervention.EffectivePrEP == 1, sum(Newly.Infected)])]
          
          dt[j, Secondary_Prevention := AI - Primary_Prevention]
          
          dt[j, SecondarytoPrimary_Ratio := Secondary_Prevention/Primary_Prevention] #indirect/direct effect
        }
        setnames(dt, old = names(dt), new = sapply(names(dt), function(x) paste0(x, year_suffix[k])))
        
        # Add merging variable
        dt[, sim := 1:.N]
        
        assign(x = paste0("dt", year_suffix[k]), value = dt)
      }
      
      dt_years <- Reduce(f = merge, list(dt_5yrs, dt_10yrs, dt_20yrs))
      
      # Estimate epidemic quantities for males and females of given age groups
      for(sex in sex_groups) {
        for(a in 1:length(age_groups)) {
          
          # Create table to store results
          dt <- data.table(sapply(epidemic_columns, function(x) x = rep(NA_real_, n_replicates)))
          
          for(j in 1:length(dt_b_list)) {
            
            # Extract baseline simulation j
            dt_b <- dt_b_list[[j]]
            
            # Assign appropriate risk results for simulation j
            if(risk == "MEDIUM") { dt_r <- dt_m_list[[j]] } else { dt_r <- dt_h_list[[j]] }
            
            # Subset data tables to county corresponding to i index
            dt_b <- dt_b[NodeId == i]
            dt_r <- dt_r[NodeId == i]
            
            # Subset risk data table into two data tables for ease of calculations below. 1) Specified gender, year 2018, and specified age group. 2) All specifications in (1), plus only high-risk individuals included.
            dt_r_pop  <- dt_r[Gender == sex & Year == 2018 & Age >= age_groups[[a]][1] & Age < age_groups[[a]][2]]
            dt_r_high <- dt_r_pop[IP_Key.Risk == "HIGH"]
            
            dt[j, Incidence_HighRisk  := dt_r_high[, 100 * sum(Newly.Infected)/(sum(Population) - sum(Infected))]]
            dt[j, Prevalence_HighRisk := dt_r_high[, 100 * sum(Infected)/sum(Population)]]
            
            dt[j, Incidence_All  := dt_r_pop[, 100 * sum(Newly.Infected)/(sum(Population) - sum(Infected))]]
            dt[j, Prevalence_All := dt_r_pop[, 100 * sum(Infected)/sum(Population)]]
            
            dt[j, ART_Coverage := dt_r_pop[, 100 * sum(On_ART)/sum(Infected)]]
          }
          setnames(dt, old = names(dt), new = sapply(names(dt), function(x) paste0(x, "_", sex_names[sex + 1], age_names[a])))
          
          # Add merging variable
          dt[, sim := 1:.N]
          
          assign(x = paste0("dt_", sex_names[sex + 1], age_names[a]), value = dt)
        }
      }
      dt_epi <- Reduce(f = merge, list(dt_Male1520, dt_Male2025, dt_Male1549, dt_Female1520, dt_Female2025, dt_Female1549))
      
      # Create table to store results for remaining male- and female-specific variable calculations
      dt <- data.table(sapply(misc_columns, function(x) x = rep(NA_real_, n_replicates)))
      
      for(j in 1:length(dt_b_list)) {
        # Extract baseline simulation j
        dt_b <- dt_b_list[[j]]
        
        # Assign appropriate risk results for simulation j
        if(risk == "MEDIUM") { dt_r <- dt_m_list[[j]] } else { dt_r <- dt_h_list[[j]] }
        
        # Subset data tables to county corresponding to i index
        dt_b <- dt_b[NodeId == i]
        dt_r <- dt_r[NodeId == i]
        
        # Create subset table for ease of calculations below.
        dt_r_males   <- dt_r[Gender == 0 & Age >= 15 & Age <= 49]
        dt_r_females <- dt_r[Gender == 1 & Age >= 15 & Age <= 49 & Year == 2018]
        
        dt[j, VMMC_Coverage1549 := dt_r_males[Year == 2018 & IP_Key.Risk == "HIGH" & IsCircumcised == 1, sum(Population)]/dt_r_males[Year == 2018 & IP_Key.Risk == "HIGH", sum(Population)]]
        
        dt[j, VMMC_Coverage_ALL15492018 := dt_r_males[Year == 2018 & IsCircumcised == 1, sum(Population)]/dt_r_males[Year == 2018, sum(Population)]]
        
        dt[j, VMMC_Coverage_ALL15492007 := dt_r_males[Year == 2007 & IsCircumcised == 1, sum(Population)]/dt_r_males[Year == 2007, sum(Population)]]
        
        dt[j, FSW_Proportion_WF1549 := 100 * dt_r_females[IP_Key.Risk == "HIGH", sum(Population)]/dt_r_females[, sum(Population)]]
      }
      # Add merging variable
      dt[, sim := 1:.N]
      
      # Combine data tables into one datatable for each node
      dt_node <- Reduce(f = merge, list(dt_years, dt_epi, dt))
      dt_node[, sim := NULL]
      
      setnames(dt_node, old = names(dt_node), new = sapply(names(dt_node), function(x) paste0(x, "_", node_names[i])))
      
      dt_node[, sim := 1:.N]
      
      assign(x = paste0("dt_", node_names[i]), value = dt_node)
    }
    # Assign data tables for risk
    dt_risk <- Reduce(f = merge, list(dt_Homa_Bay, dt_Kisii, dt_Kisumu, dt_Migori, dt_Nyamira, dt_Siaya))
    
    assign(x = paste0("dt_", risk, "_", g), value = dt_risk)
  }
}

# Combine all data tables into results tables for medium- and high-risk targeting strategies
dt_MEDIUM <- rbindlist(l = list(dt_MEDIUM_1, dt_MEDIUM_2, dt_MEDIUM_3, dt_MEDIUM_4, dt_MEDIUM_5))

dt_HIGH <- rbindlist(l = list(dt_HIGH_1, dt_HIGH_2, dt_HIGH_3, dt_HIGH_4, dt_HIGH_5))

dt_MEDIUM[, sim := 1:.N]
dt_HIGH[, sim := 1:.N]

# Write files to csv
write.csv(x = dt_MEDIUM, file = paste0(output_dir, "/ImpactResultsM.csv"))
write.csv(x = dt_HIGH, file = paste0(output_dir, "/ImpactResultsH.csv"))

# Merge parameters with results
params <- read.csv(file = "C:/Users/kpeebles/NyanzaCalib-2018-10-08/Nyanza--0.0525--rep3--resampling/Selected_Trajectory_Params_Clean.csv")
setnames(params, old = "TPI", new = "sim")

dt_MEDIUM_params <- merge(x = dt_MEDIUM, y = params, by = "sim")
dt_HIGH_params <- merge(x = dt_HIGH, y = params, by = "sim")

write.csv(x = dt_MEDIUM_params, file = paste0(output_dir, "/ParamsImpactResultsM.csv"))
write.csv(x = dt_HIGH_params, file = paste0(output_dir, "/ParamsImpactResultsH.csv"))

## Purpose: Plotting functions to be called in rmarkdown document
## Author:  Kathryn Peebles
## Date:    10 July 2019

install.packages(extrafont)
font_import()
loadfonts(device = "win")

library(ggplot2)
library(extrafont)
loadfonts(device = "win")

#### PREVALENCE, PROVINCIAL WITH ZERO-PREVALENCE ####
create_prev_zero_prov_data <- function(dt_list, node_names_dt = node_dt) {
  # Empiric prevalence data is available at the province level by sex and age (age groups 15-25 and 15-50). Create corresponding age groups.
  dt_list <- lapply(1:length(dt_list), function(x) {
    dt_list[[x]][, age_group:= ifelse(Age %in% seq(15, 20, 5), "[15:25)",
                                        ifelse(Age %in% seq(25, 45, 5), "[25:50)", NA))]
  })
  
  # Ages 15-24
  stat_dt_list <- lapply(dt_list, function(x) as.data.table(x[age_group == "[15:25)"] %>% group_by(Year, NodeId, Gender) %>% summarise(prev = sum(Infected)/sum(Population))))
  
  # Add sim number to each of tables and concatenate all data tables
  stat_dt_age1524 <- rbindlist(l = lapply(1:length(stat_dt_list), function(x) stat_dt_list[[x]][, `:=`(sim = x, age_group = "[15:25)")]))
  
  # Ages 15-49
  stat_dt_list <- lapply(dt_list, function(x) as.data.table(x[!is.na(age_group)] %>% group_by(Year, NodeId, Gender) %>% summarise(prev = sum(Infected)/sum(Population))))
  
  # Add sim number to each of tables and concatenate all data tables
  stat_dt_age1549 <- rbindlist(l = lapply(1:length(stat_dt_list), function(x) stat_dt_list[[x]][, `:=`(sim = x, age_group = "[15:50)")]))
  
  stat_dt_province <- rbindlist(l = list(stat_dt_age1524, stat_dt_age1549))
  
  # Create factor gender
  stat_dt_province[, gender := ifelse(Gender == 0, "Male", "Female")]
  
  stat_dt_province <- merge(x = stat_dt_province, y = node_names_dt, by = "NodeId", all = T)
  
  return(stat_dt_province)
}

load_prev_obs <- function() {
  # Load empiric data
  dt <- as.data.table(read_excel(path = paste0(output_dir, "/EmpiricData.xlsx"), sheet = "Prevalence", range = "A1:J101"))
  
  # Set names to be equal between simulated and observed data
  setnames(dt, old = c("Gender", "AgeBin", "Prevalence"), new = c("gender", "age_group", "prev"))
  
  return(dt)
}

plot_prev_province <- function(dt_sim, dt_obs) {
  return_plot <- ggplot(data = dt_sim) +
    geom_line(aes(x = Year, y = prev, group = interaction(sim, gender), col = gender), alpha = 0.025) +
    geom_smooth(aes(x = Year, y = prev, col = gender), method = "loess", span = 0.1, se = F, size = 1) +
    geom_point(data = dt_obs[node_name != "All" & age_group %in% c("[15:25)", "[15:50)")], size = 1, aes(x = Year, y = prev, col = gender)) +
    geom_errorbar(data = dt_obs[node_name != "All" & age_group %in% c("[15:25)", "[15:50)")], aes(x = Year, ymin = lower, ymax = upper, col = gender), width = 1, size = 0.5) +
    labs(x = "Year", y = "Prevalence", col = "Gender", title = "Prevalence by province, age, and gender") +
    facet_grid(node_name ~ age_group, labeller = as_labeller(c(prov_names, "[15:25)" = "[15:25)", "[15:50)" = "[15:50)"))) +
    theme(text = element_text(family = "Times"),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey", size = 0.05),
          panel.border = element_rect(color = "grey", fill = NA),
          strip.background = element_rect(color = "grey", fill = NA),
          legend.key = element_blank())
  
  return(return_plot)
}

#### PREVALENCE, PROVINCIAL WITHOUT ZERO-PREVALENCE ####
create_prev_prov_data <- function(dt_list, node_names_dt = node_dt) {
  # Empiric prevalence data is available at the province level by sex and age (age groups 15-25 and 15-50). Create corresponding age groups.
  dt_list <- lapply(1:length(dt_list), function(x) {
    dt_list[[x]][, age_group:= ifelse(Age %in% seq(15, 20, 5), "[15:25)",
                                      ifelse(Age %in% seq(25, 45, 5), "[25:50)", NA))]
  })
  
  # Ages 15-24
  stat_dt_list <- lapply(dt_list, function(x) as.data.table(x[age_group == "[15:25)"] %>% group_by(Year, NodeId, Gender) %>% summarise(prev = sum(Infected)/sum(Population))))
  
  # Add sim number to each of tables and concatenate all data tables
  stat_dt_age1524 <- rbindlist(l = lapply(1:length(stat_dt_list), function(x) stat_dt_list[[x]][, `:=`(sim = x, age_group = "[15:25)")]))
  
  # Ages 15-49
  stat_dt_list <- lapply(dt_list, function(x) as.data.table(x[!is.na(age_group)] %>% group_by(Year, NodeId, Gender) %>% summarise(prev = sum(Infected)/sum(Population))))
  
  # Add sim number to each of tables and concatenate all data tables
  stat_dt_age1549 <- rbindlist(l = lapply(1:length(stat_dt_list), function(x) stat_dt_list[[x]][, `:=`(sim = x, age_group = "[15:50)")]))
  
  stat_dt_province <- rbindlist(l = list(stat_dt_age1524, stat_dt_age1549))
  
  # Create factor gender
  stat_dt_province[, gender := ifelse(Gender == 0, "Male", "Female")]
  
  stat_dt_province <- merge(x = stat_dt_province, y = node_names_dt, by = "NodeId", all = T)
  
  stat_dt_province[prev == 0, prev := NA]
  
  return(stat_dt_province)
}


#### PREVALENCE, NATIONAL ####
create_prev_nat_data <- function(dt_list) {
  # Empiric prevalence data is available at the national level by sex and age (age groups in 5-year increments from 15-50). Create corresponding age groups.
  dt_list <- lapply(1:length(dt_list), function(x) {
    dt_list[[x]][!(Age %in% c(0, 5, 10, 50, 55, 60, 65, 70, 76, 80)), age_group := c("[15:20)", "[20:25)", "[25:30)", "[30:35)", "[35:40)", "[40:45)", "[45:50)")[findInterval(x = Age, vec = seq(15, 50, 5))]]
  })
  
  stat_dt_list <- lapply(dt_list, function(x) as.data.table(x[!is.na(age_group)] %>% group_by(Year, Gender, age_group) %>% summarise(prev = sum(Infected)/sum(Population))))
  
  # Add sim number to each of tables and concatenate all data tables
  stat_dt_national <- rbindlist(l = lapply(1:length(stat_dt_list), function(x) stat_dt_list[[x]][, sim := x]))
  
  # Create factor gender
  stat_dt_national[, gender := ifelse(Gender == 0, "Male", "Female")]
  
  return(stat_dt_national)
}

plot_prev_national <- function(dt_sim, dt_obs) {
  return_plot <- ggplot(data = dt_sim) +
    geom_line(aes(x = Year, y = prev, group = sim, col = gender), alpha = 0.25) +
    geom_smooth(aes(x = Year, y = prev, col = gender), method = "loess", span = 0.1, se = F, size = .7) +
    geom_point(data = dt_obs[age_group %in% dt_sim[, unique(age_group)]], size = 1, aes(x = Year, y = prev, col = gender)) +
    geom_errorbar(data = dt_obs[age_group %in% dt_sim[, unique(age_group)]], aes(x = Year, ymin = lower, ymax = upper, col = gender), width = 1, size = 0.5) +
    labs(x = "Year", y = "Prevalence", col = "Gender", title = "HIV Prevalence by age and gender") +
    facet_grid(age_group ~ gender) +
    theme(text = element_text(family = "Times"),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey", size = 0.05),
          panel.border = element_rect(color = "grey", fill = NA),
          strip.background = element_rect(color = "grey", fill = NA),
          legend.position = "none")
  
  return(return_plot)
}

#### ART COVERAGE, PROVINCIAL ####
# Create dataset needed for plotting
create_art_cov_prov_data <- function(dt_list, node_names_dt = node_dt) {
  # Empiric ART coverage data is available at the province level by sex, aggregated over ages 15-65. Create corresponding age group.
  dt_list <- lapply(1:length(dt_list), function(x) {
    dt_list[[x]][, age_group:= ifelse(Age %in% seq(15, 60, 5), "[15:65)", NA)]
  })
  
  stat_dt_list <- lapply(dt_list, function(x) as.data.table(x[!is.na(age_group)] %>% group_by(Year, NodeId, Gender) %>% summarise(art_cov = sum(On_ART)/sum(Infected))))
  
  # Add sim number to each of the tables and concatenate all data tables
  stat_dt <- rbindlist(l = lapply(1:length(stat_dt_list), function(x) stat_dt_list[[x]][, sim := x])) # Leave NA coverage values with a value of NA (NA occurs when there are 0 infections - 0/0)
  
  stat_dt_province <- merge(x = stat_dt, y = node_names_dt, by = "NodeId", all = T)
  
  # Create factor gender
  stat_dt_province[, gender := ifelse(Gender == 0, "Male", "Female")]
  
  return(stat_dt_province)
}

# Load and clean empiric data for ART coverage
load_art_cov_obs <- function() {
  # Load empiric data
  dt <- as.data.table(read_excel(path = paste0(output_dir, "/EmpiricData.xlsx"), sheet = "ARTCoverage", range = "A1:J44"))
  
  # Set names to be equal between simulated and observed data
  setnames(dt, old = c("Province", "Gender", "AgeBin", "ARTCoverage"), new = c("node_name", "gender", "age_group", "art_cov"))
  
  return(dt)
}

plot_art_cov_province <- function(dt_sim, dt_obs) {
  return_plot <- ggplot(data = dt_sim) +
    geom_line(aes(x = Year, y = art_cov, group = sim, col = gender), alpha = 0.025) +
    geom_smooth(aes(x = Year, y = art_cov, col = gender), method = "loess", span = 0.1, se = F, size = 1) +
    geom_point(data = dt_obs[node_name != "All" & age_group == "[15:65)"], size = 1, aes(x = Year, y = art_cov, col = gender)) +
    geom_errorbar(data = dt_obs[node_name != "All" & age_group == "[15:65)"], aes(x = Year, ymin = lower, ymax = upper, col = gender), width = 1, size = 0.5) +
    labs(x = "Year", y = "ART Coverage", col = "Gender", title = "ART coverage by province and gender") +
    facet_grid(node_name ~ gender, labeller = as_labeller(c(prov_names, "Female" = "Female", "Male" = "Male"))) +
    theme(text = element_text(family = "Times"),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey", size = 0.05),
          panel.border = element_rect(color = "grey", fill = NA),
          strip.background = element_rect(color = "grey", fill = NA),
          legend.position = "none")
  
  return(return_plot)
} 

#### ART COVERAGE, NATIONAL ####
create_art_cov_nat_data <- function(dt_list) {
  # Empiric ART coverage data is available at the national level by sex and in age groups from 15-65 by 5-year intervals. Create corresponding age group.
  dt_list <- lapply(1:length(dt_list), function(x) {
    dt_list[[x]][Age %in% seq(15, 60, 5), age_group := c("[15:20)", "[20:25)", "[25:30)", "[30:35)", "[35:40)", "[40:45)", "[45:50)", "[50:55)", "[55:60)", "[60:65)")[findInterval(x = Age, vec = seq(15, 60, 5))]]
  })
  
  stat_dt_list <- lapply(dt_list, function(x) as.data.table(x[!is.na(age_group)] %>% group_by(Year, Gender, age_group) %>% summarise(art_cov = sum(On_ART)/sum(Infected))))
  
  # Add sim number to each of the tables and concatenate all data tables
  stat_dt <- rbindlist(l = lapply(1:length(stat_dt_list), function(x) stat_dt_list[[x]][, sim := x])) 
  
  # Replace NA coverage values with a value of 0 (NA occurs when there are 0 infections - 0/0)
  stat_dt[is.na(art_cov), art_cov := 0]
  
  # Create factor gender
  stat_dt[, gender := ifelse(Gender == 0, "Male", "Female")]
  
  return(stat_dt)
}

plot_art_cov_national <- function(dt_sim) {
  return_plot <- ggplot(data = dt_sim) +
    geom_line(aes(x = Year, y = art_cov, group = sim, col = gender), alpha = 0.025) +
    geom_smooth(aes(x = Year, y = art_cov, col = gender), method = "loess", span = 0.1, se = F, size = 1) +
    #geom_point(data = dt_obs[node_name == "All" & age_group != "[15:65)"], size = 1, aes(x = Year, y = art_cov, col = gender)) +
    #geom_errorbar(data = dt_obs[node_name == "All" & age_group != "[15:65)"], aes(x = Year, ymin = lower, ymax = upper, col = gender), width = 1, size = 0.5) +
    labs(x = "Year", y = "ART Coverage", col = "Gender", title = "ART coverage by age and gender") +
    facet_grid(age_group ~ gender) +
    theme(text = element_text(family = "Times"),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey", size = 0.05),
          panel.border = element_rect(color = "grey", fill = NA),
          strip.background = element_rect(color = "grey", fill = NA),
          legend.position = "none")
  
  return(return_plot)
}

#### ART NUMBERS, PROVINCIAL AND NATIONAL ####
create_art_num_data <- function(dt_list) {
  # Empiric ART numbers data are available at the national level by sex and in age groups from 15-65 by 5-year intervals. Create corresponding age group.
  dt_list <- lapply(1:length(dt_list), function(x) {
    dt_list[[x]][Age %in% seq(15, 80, 5), age_group := "[15:100)"]
  })
  
 # stat_dt_list_province <- lapply(dt_list, function(x) as.data.table(x[!is.na(age_group)] %>% group_by(Year, NodeId, Gender) %>% summarise(art_num = sum(On_ART) * pop_scale_factor)))
  stat_dt_list_national <- lapply(dt_list, function(x) as.data.table(x[!is.na(age_group)] %>% group_by(Year, Gender) %>% summarise(art_num = sum(On_ART) * pop_scale_factor)))
  
  # Add sim number to each of prevalence tables
#  stat_dt_province <- rbindlist(l = lapply(1:length(stat_dt_list_province), function(x) stat_dt_list_province[[x]][, sim := x]))
  stat_dt_national <- rbindlist(l = lapply(1:length(stat_dt_list_national), function(x) stat_dt_list_national[[x]][, `:=`(sim = x)]))
  setcolorder(stat_dt_national, c(1, 4, 2:3))
  
  # Concatenate all prevalence data tables
  stat_dt <- rbindlist(l = list(stat_dt_national))
  
  # Create factor gender
  stat_dt[, gender := ifelse(Gender == 0, "Male", "Female")]
  
 # stat_dt <- merge(x = stat_dt, y = node_names_dt, by = "NodeId", all = T)
 # stat_dt[is.na(node_name), node_name := "All"]
  
  return(stat_dt)
}

load_art_num_obs <- function() {
  # Load empiric data
  obs_dt <- as.data.table(read_excel(path = paste0(output_dir, "/EmpiricData.xlsx"), sheet = "OnART", range = "A1:E35"))
  
  # Set names to be equal between simulated and observed data
  setnames(obs_dt, old = c("Gender", "AgeBin", "OnART"), new = c("gender", "age_group", "art_num"))
  
  return(obs_dt)
}
  
plot_art_num <- function(dt_sim, dt_obs) {
  return_plot <- ggplot(data = dt_sim) +
    geom_line(aes(x = Year, y = art_num, group = sim, col = gender), alpha = 0.025) +
    geom_smooth(aes(x = Year, y = art_num, col = gender), method = "loess", span = 0.1, se = F, size = 1) +
    geom_point(data = dt_obs, size = 1, aes(x = Year, y = art_num, col = gender)) +
    scale_y_continuous(labels = comma) +
    labs(x = "Year", y = "Number on ART", col = "Gender", title = "Number on ART by gender") +
    facet_grid(~gender, scales = "free_y", labeller = as_labeller(c("Female" = "Female", "Male" = "Male"))) +
    theme(text = element_text(family = "Times"),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey", size = 0.05),
          panel.border = element_rect(color = "grey", fill = NA),
          strip.background = element_rect(color = "grey", fill = NA),
          legend.position = "none")
  
  return(return_plot)
}

#### POPULATION, PROVINCIAL ####
create_pop_prov_data <- function(dt_list) {
  # Empiric population numbers are available at the provincial level by sex in age group 15-50. Create corresponding age group.
  dt_list <- lapply(1:length(dt_list), function(x) {
    dt_list[[x]][, age_group := ifelse(Age %in% seq(15, 45, 5), "[15:50)", NA)]
  })
  
  stat_dt_list <- lapply(dt_list, function(x) as.data.table(x[!is.na(age_group) & Year == 2015.5] %>% group_by(Gender) %>% summarise(pop = sum(Population) * pop_scale_factor)))
  
  # Add sim number to each of prevalence tables
  stat_dt <- rbindlist(l = lapply(1:length(stat_dt_list), function(x) stat_dt_list[[x]][, sim := x]))
  
  # Create factor gender
  stat_dt[, gender := ifelse(Gender == 0, "Male", "Female")]
  
 # stat_dt <- merge(x = stat_dt, y = node_names_dt, by = "NodeId", all = T)
  
  return(stat_dt)
}

load_pop_obs <- function() {
  # Load empiric data
  obs_dt <- as.data.table(read_excel(path = paste0(output_dir, "/EmpiricData.xlsx"), sheet = "Population", range = "A1:E121"))
  
  # Set names to be equal between simulated and observed data
  setnames(obs_dt, old = c("Gender", "AgeBin", "Population"), new = c("gender", "age_group", "pop"))
  
  return(obs_dt)
}

plot_pop_province <- function(dt_sim, dt_obs) {
  return_plot <- ggplot() +
    geom_point(data = dt_sim, aes(x = node_name, y = pop, group = sim, color = gender), size = 1.2, alpha = 0.01) +
    geom_point(data = dt_obs[node_name != "All"], size = 1, aes(x = node_name, y = pop)) +
    scale_x_discrete(labels = prov_names) +
    scale_y_continuous(labels = comma) +
    facet_grid(. ~ gender) +
    labs(x = "Province", y = "Population size", title = "Population size by province in 2015") +
    guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
    theme(text = element_text(family = "Times"),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey", size = 0.05),
          panel.border = element_rect(color = "grey", fill = NA),
          strip.background = element_rect(color = "grey", fill = NA),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    coord_cartesian(ylim = c(0, 800000))
  
  return(return_plot)
}

#### POPULATION, NATIONAL ####
create_pop_nat_data <- function(dt_list) {
  # Empiric population numbers are available at the national level by sex and in age groups from 0-80 by 5-year intervals and 80-100. Create corresponding age groups.
  dt_list <- lapply(1:length(dt_list), function(x) {
    dt_list[[x]][, age_group := c("[0:5)", "[5:10)", "[10:15)", "[15:20)", "[20:25)", "[25:30)", "[30:35)", "[35:40)", "[40:45)", "[45:50)", "[50:55)", "[55:60)", "[60:65)", "[65:70)", "[70:75)", "[75:80)", "[80:100)")[findInterval(x = Age, vec = c(seq(0, 80, 5)))]]
  })
  
  stat_dt_list <- lapply(dt_list, function(x) as.data.table(x[!is.na(age_group) & Year == 2015.5] %>% group_by(Gender, age_group) %>% summarise(pop = sum(Population) * pop_scale_factor)))
  
  # Add sim number to each of prevalence tables
  stat_dt <- rbindlist(l = lapply(1:length(stat_dt_list), function(x) stat_dt_list[[x]][, sim := x]))
  
  # Create factor gender
  stat_dt[, gender := ifelse(Gender == 0, "Male", "Female")]
  
  # Factor age group to plot in the desired order
  stat_dt[, age_group := factor(age_group, levels = c("[0:5)", "[5:10)", "[10:15)", "[15:20)", "[20:25)", "[25:30)", "[30:35)", "[35:40)", "[40:45)", "[45:50)", "[50:55)", "[55:60)", "[60:65)", "[65:70)", "[70:75)", "[75:80)", "[80:100)"))]
  
  return(stat_dt)
}

plot_pop_national <- function(dt_sim, dt_obs) {
  return_plot <- ggplot() +
    geom_point(data = dt_sim, aes(x = age_group, y = pop, group = sim, color = gender), size = 1.2, alpha = 0.01) +
    geom_point(data = dt_obs, size = 1, aes(x = age_group, y = pop)) +
    scale_y_continuous(labels = comma) +
    facet_grid(. ~ gender) +
    labs(x = "Age group", y = "Population size", title = "National population size in 2015") +
    guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
    theme(text = element_text(family = "Times"),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey", size = 0.05),
          panel.border = element_rect(color = "grey", fill = NA),
          strip.background = element_rect(color = "grey", fill = NA),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust = 0.5))
  
  return(return_plot)
}

#### INCIDENCE ####
create_inc_data <- function(dt_list, node_names_dt = node_dt) {
  # Empiric incidence is available at the national level by sex in age groups 15-25, 25-35, 35-50, 15-50, and 15-65. Create corresponding age groups.
  dt_list <- lapply(1:length(dt_list), function(x) {
    dt_list[[x]][Age %in% seq(15, 45, 5), `:=`(age_group = c("[15:25)", "[25:35)", "[35:50)")[findInterval(x = Age, vec = c(15, 25, 35))],
                                                 age_group_agg = "[15:50)")]
  })
  dt_list <- lapply(1:length(dt_list), function(x) {
    dt_list[[x]][Age %in% seq(15, 60, 5), `:=`(age_group_ext = "[15:65)")]
  })
  
  # Incidence should be calculated as newly infected out of the population at risk. Time intervals include the time from the prior 6 months. For incidence, the population at risk is the number of negative individuals (Population - Infected) at the beginning of an interval (i.e., the end of the prior interval), so incidence should be calculated using a lagged variable for the number of HIV-negative individuals.
  # Disaggregated ages
  pop_at_risk <- lapply(dt_list, function(x) as.data.table(x[age_group %in% c("[15:25)", "[25:35)", "[35:50)")] %>% group_by(Year, Gender, age_group) %>% summarise(negative_pop = sum(Population) - sum(Infected))))
  new_inf     <- lapply(dt_list, function(x) as.data.table(x[age_group %in% c("[15:25)", "[25:35)", "[35:50)")] %>% group_by(Year, Gender, age_group) %>% summarise(inf = sum(Newly.Infected))))
  
  # Add sim number to pop_at_risk and new_inf
  pop_dt <- rbindlist(l = lapply(1:length(pop_at_risk), function(x) pop_at_risk[[x]][, sim := x]))
  inf_dt <- rbindlist(l = lapply(1:length(new_inf), function(x) new_inf[[x]][, sim := x]))
  
  # Merge data tables
  inc_dt_disagg <- merge(x = pop_dt, y = inf_dt, by = c("sim", "Year", "Gender", "age_group"), all = T)
  
  # Ages [15:50)
  pop_at_risk <- lapply(dt_list, function(x) as.data.table(x[age_group_agg == "[15:50)"] %>% group_by(Year, Gender) %>% summarise(negative_pop = sum(Population) - sum(Infected))))
  new_inf     <- lapply(dt_list, function(x) as.data.table(x[age_group_agg == "[15:50)"] %>% group_by(Year, Gender) %>% summarise(inf = sum(Newly.Infected))))
  
  # Add sim number to pop_at_risk and new_inf
  pop_dt <- rbindlist(l = lapply(1:length(pop_at_risk), function(x) pop_at_risk[[x]][, sim := x]))
  inf_dt <- rbindlist(l = lapply(1:length(new_inf), function(x) new_inf[[x]][, sim := x]))
  
  # Merge data tables
  inc_dt_1550 <- merge(x = pop_dt, y = inf_dt, by = c("sim", "Year", "Gender"), all = T)
  
  # Add age_group variable
  inc_dt_1550[, age_group := "[15:50)"]
  
  # Ages [15:65)
  pop_at_risk <- lapply(dt_list, function(x) as.data.table(x[age_group_ext == "[15:65)"] %>% group_by(Year, Gender) %>% summarise(negative_pop = sum(Population) - sum(Infected))))
  new_inf     <- lapply(dt_list, function(x) as.data.table(x[age_group_ext == "[15:65)"] %>% group_by(Year, Gender) %>% summarise(inf = sum(Newly.Infected))))
  
  # Add sim number to pop_at_risk and new_inf
  pop_dt <- rbindlist(l = lapply(1:length(pop_at_risk), function(x) pop_at_risk[[x]][, sim := x]))
  inf_dt <- rbindlist(l = lapply(1:length(new_inf), function(x) new_inf[[x]][, sim := x]))
  
  # Merge data tables
  inc_dt_1565 <- merge(x = pop_dt, y = inf_dt, by = c("sim", "Year", "Gender"), all = T)
  
  # Add age_group variable
  inc_dt_1565[, age_group := "[15:65)"]
  
  # Concatenate all data tables
  setcolorder(inc_dt_disagg, neworder = c("sim", "Year", "Gender", "negative_pop", "inf", "age_group"))
  inc_dt <- rbindlist(l = list(inc_dt_disagg, inc_dt_1550, inc_dt_1565))
  
  # Lag population at risk (the pop at risk at the end of one interval - e.g., 1980.5 - corresponds to the pop at risk at the beginning of the next - e.g., 1981)
  setkey(inc_dt, sim, Gender, age_group, Year)
  inc_dt[, pop_at_risk := shift(negative_pop), by = c("sim", "Gender", "age_group")]
  
  # Calculate incidence
  inc_dt[, incidence := inf/pop_at_risk]
  
  # Create factor gender
  inc_dt[, gender := ifelse(Gender == 0, "Male", "Female")]
  
  # Factor age_group variable so that it plots in desired order
  inc_dt[, age_group := factor(age_group, levels = c("[15:25)", "[25:35)", "[35:50)", "[15:50)", "[15:65)"))]
  
  return(inc_dt)
}

load_inc_obs <- function() {
  # Load empiric data
  obs_dt <- as.data.table(read_excel(path = paste0(output_dir, "/EmpiricData.xlsx"), sheet = "Incidence", range = "A1:J16"))
  
  # Set names to be equal between simulated and observed data
  setnames(obs_dt, old = c("Province", "Gender", "AgeBin", "Incidence"), new = c("node_name", "gender", "age_group", "incidence"))
  
  # Factor age_group variable so that it plots in desired order
  obs_dt[, age_group := factor(age_group, levels = c("[15:25)", "[25:35)", "[35:50)", "[15:50)", "[15:65)"))]
  
  return(obs_dt)
}

plot_inc <- function(dt_sim, dt_obs) {
  return_plot <- ggplot(data = dt_sim) +
    geom_line(aes(x = Year, y = incidence, group = sim, col = gender), alpha = 0.025) +
    geom_smooth(aes(x = Year, y = incidence, col = gender), method = "loess", span = 0.1, se = F, size = 1) +
    geom_point(data = dt_obs[gender != "Both"], size = 1, aes(x = Year, y = incidence, col = gender)) +
    geom_errorbar(data = dt_obs[gender != "Both"], aes(x = Year, ymin = lower, ymax = upper, col = gender), width = 1, size = 0.5) +
    labs(x = "Year", y = "Incidence", col = "Gender", title = "Incidence by age and gender") +
    facet_grid(age_group ~ gender) +
    theme(text = element_text(family = "Times"),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey", size = 0.05),
          panel.border = element_rect(color = "grey", fill = NA),
          strip.background = element_rect(color = "grey", fill = NA),
          legend.position = "none")
  
  return(return_plot)
}

# setting up grids: 
rm(list=ls())
library(quadprog)
library(tidyverse)
library(fpp3)
source("R/1_load_data.R")
source("R/2_functions.R")
mu.NVE <- NVE %>% 
  group_by(locID) %>% 
  summarize(mean = mean(value)) %>% pull(mean)
Sigma.NVE.daily <- 
  NVE %>% mutate(date=as.Date(datetime)) %>% 
  group_by(locID,date) %>% 
  summarize(value = mean(value, na.rm=T)) %>% 
  pivot_wider(names_from = locID, values_from = value) %>% 
  select(-date) %>% 
  cov()
Sigma.NVE.weekly <- 
  NVE %>% mutate(yweek=yearweek(datetime)) %>% 
  group_by(locID,yweek) %>% 
  summarize(value = mean(value, na.rm=T)) %>% 
  pivot_wider(names_from = locID, values_from = value) %>% 
  select(-yweek) %>% 
  cov()
Sigma.NVE.monthly <- 
  NVE %>% mutate(ymonth=yearmonth(datetime)) %>% 
  group_by(locID,ymonth) %>% 
  summarize(value = mean(value, na.rm=T)) %>% 
  pivot_wider(names_from = locID, values_from = value) %>% 
  select(-ymonth) %>% 
  cov()
  



# --------- CASE WITHOUT UN and SN2 --------
candidates = 1:length(mu.NVE)



# ------- CASE A -----------
NVE.case.A.hour <- lapply(c(.58,.6,.62), 
                     optimize_portfolio,
                     mu = mu.NVE,
                     Sigma = Sigma.NVE,
                     maxWeights = maxWeights
) %>% bind_rows() %>% 
  mutate(source = "NVE",
         case = "A",
         resolution = "hourly") %>% 
  select(-std)
NVE.case.A.day <- lapply(c(.58,.6,.62), 
                          optimize_portfolio,
                          mu = mu.NVE,
                          Sigma = Sigma.NVE.daily,
                          maxWeights = maxWeights
) %>% bind_rows() %>% 
  mutate(source = "NVE",
         case = "A",
         resolution = "daily") %>% 
  select(-std)
NVE.case.A.weekly <- lapply(c(.58,.6,.62), 
                          optimize_portfolio,
                          mu = mu.NVE,
                          Sigma = Sigma.NVE.weekly,
                          maxWeights = maxWeights
) %>% bind_rows() %>% 
  mutate(source = "NVE",
         case = "A",
         resolution = "weekly") %>% 
  select(-std)
NVE.case.A.monthly <- lapply(c(.58,.6,.62), 
                            optimize_portfolio,
                            mu = mu.NVE,
                            Sigma = Sigma.NVE.monthly,
                            maxWeights = maxWeights
) %>% bind_rows() %>% 
  mutate(source = "NVE",
         case = "A",
         resolution = "monthly") %>% 
  select(-std)

NVE.sensititivy <- bind_rows(
  NVE.case.A.hour,
  NVE.case.A.day,
  NVE.case.A.weekly,
  NVE.case.A.monthly
) 




NVE.sensititivy<-NVE.sensititivy %>% 
  mutate(turbines = round(2000*weights)) %>% 
  filter(turbines >0) %>% 
  group_by(case,source, powertarget,resolution) %>% 
  mutate(tot = sum(turbines))
# --
rmTurb <- NVE.sensititivy %>% 
  mutate(missingTurbines = 2000-tot,
         diff = weights*2000-turbines) %>% 
  filter(missingTurbines < 0) %>% 
  group_by(source, powertarget, case,resolution) %>% 
  arrange(source, powertarget, resolution,case, diff) %>% 
  filter(row_number() <=abs(missingTurbines)) %>% 
  mutate(addTurbines = -1) %>% 
  select(locID, powertarget, source,resolution, case, addTurbines)
# ____ Locations where we should add turbines: ____
addTurb <- NVE.sensititivy %>% 
  mutate(missingTurbines = 2000-tot,
         diff = weights*2000-turbines) %>% 
  filter(missingTurbines > 0) %>% 
  group_by(source, powertarget, case,resolution) %>% 
  arrange(source, powertarget, case, desc(diff) ) %>% 
  filter(row_number() <=abs(missingTurbines)) %>% 
  mutate(addTurbines = 1) %>% 
  select(locID, powertarget, source,resolution, case, addTurbines)
NVE.sensititivy <- NVE.sensititivy %>% left_join(bind_rows(rmTurb, addTurb)) %>% 
  mutate(addTurbines = ifelse(is.na(addTurbines),0,addTurbines),
         turbines = turbines + addTurbines) %>% 
  select(-addTurbines, -tot)
# --
NVE.sensititivy %>% ungroup() %>% 
  filter(powertarget == .6) %>% 
  select(-weights,-source,-case,-powertarget) %>% 
  pivot_wider(names_from = "resolution", values_from = "turbines")-> turbines.sensitivity
colSums(turbines.sensitivity[,2:5],na.rm=T)

tab <- turbines.sensitivity %>% 
  full_join(tibble(locID=1:20) %>% 
               filter(!(locID %in% turbines.sensitivity$locID)), 
             by="locID") %>% 
  arrange(locID) %>% 
  mutate(
    region =factor(
      ifelse(locID < 5, "North",
             ifelse(locID<8, "North-West",
                    ifelse(locID<14, "West",
                           ifelse(locID<20, "South-West","South-East")))))) %>% 
  pivot_longer(cols = 2:5) %>% 
  mutate(value = ifelse(is.na(value),0,value)) %>% 
    group_by(region,name) %>% 
  summarize(turbines = sum(value,na.rm=T)) %>%
  pivot_wider(names_from = name, values_from = turbines) %>% 
  relocate(region, hourly, daily, weekly,monthly) 
names(tab) <- str_to_title(names(tab))
tab %>% t() %>%  
  xtable::xtable(digits = 0) %>% print(include.colnames = FALSE)
turbines.sensitivity %>% 
  full_join(tibble(locID=1:20) %>% 
              filter(!(locID %in% turbines.sensitivity$locID)), 
            by="locID") %>% 
  arrange(locID) %>% 
  mutate(
    region =factor(
      ifelse(locID < 5, "North",
             ifelse(locID<8, "North-West",
                    ifelse(locID<14, "West",
                           ifelse(locID<20, "South-West","South-East")))))) %>% 
  pivot_longer(cols = 2:5) %>% 
  mutate(value = ifelse(is.na(value),0,value)) %>% 
  group_by(name) %>% 
  summarize(NoWP = sum(value>0)) 

mu.NVE <- NVE %>% 
  group_by(locID) %>% 
  summarize(mean = mean(value)) %>% pull(mean)
Sigma.NVE <- 
  NVE %>% 
  select(locID,datetime,value) %>% 
  #summarize(value = mean(value, na.rm=T)) %>% 
  pivot_wider(names_from = locID, values_from = value) %>% 
  select(-datetime) %>% 
  cor()
Sigma.NVE.daily <- 
  NVE %>% mutate(date=as.Date(datetime)) %>% 
  group_by(locID,date) %>% 
  summarize(value = mean(value, na.rm=T)) %>% 
  pivot_wider(names_from = locID, values_from = value) %>% 
  select(-date) %>% 
  cor()
Sigma.NVE.weekly <- 
  NVE %>% mutate(yweek=yearweek(datetime)) %>% 
  group_by(locID,yweek) %>% 
  summarize(value = mean(value, na.rm=T)) %>% 
  pivot_wider(names_from = locID, values_from = value) %>% 
  select(-yweek) %>% 
  cor()
Sigma.NVE.monthly <- 
  NVE %>% mutate(ymonth=yearmonth(datetime)) %>% 
  group_by(locID,ymonth) %>% 
  summarize(value = mean(value, na.rm=T)) %>% 
  pivot_wider(names_from = locID, values_from = value) %>% 
  select(-ymonth) %>% 
  cor()

bind_rows(
Sigma.NVE %>% as_tibble() %>% 
  mutate(locID1 = 1:20) %>% 
  pivot_longer(cols = 1:20) %>% 
  mutate(resolution = "Hourly"),
Sigma.NVE.daily %>% as_tibble() %>% 
  mutate(locID1 = 1:20) %>% 
  pivot_longer(cols = 1:20) %>% 
  mutate(resolution = "Daily"),
Sigma.NVE.weekly %>% as_tibble() %>% 
  mutate(locID1 = 1:20) %>% 
  pivot_longer(cols = 1:20) %>% 
  mutate(resolution = "Weekly"),
Sigma.NVE.monthly %>% as_tibble() %>% 
  mutate(locID1 = 1:20) %>% 
  pivot_longer(cols = 1:20) %>% 
  mutate(resolution = "Monthly")) %>% 
  mutate(resolution = factor(resolution, levels = c("Hourly", "Daily", "Weekly","Monthly"))) %>% 
  ggplot(aes(x=locID1, y = as.numeric(name), fill = value)) + 
  geom_tile() + facet_wrap(~resolution)+
  scale_x_continuous(expand = c(0,0), name= "Location ID")+
  scale_y_reverse(expand = c(0,0), name= "Location ID") +
  theme_bw()+theme(strip.background = element_rect(fill = "white", color = "transparent"))+
  scale_fill_viridis_c(name = "Correlation")+
  guides(fill = guide_colorbar(barheight = 25))
ggsave("figures/sensitivity_correlation_matrices_per_timescale.pdf", width = 10, height = 8)

# Table results:
rm(list=ls())
library(tidyverse)
library(xtable)
source("R/1_load_data.R")
portfolios <- readRDS("output/portfolioweights_all_cases.rds") 

dat <- SOL %>% mutate(source = "Solbrekke") %>% 
  bind_rows(
    NVE %>% select(value, locID, datetime) %>% mutate(source = "NVE"))


dat <- dat %>% right_join(portfolios %>% select(locID, powertarget, turbines, case, weights, source), 
                            by = c("locID","source"),
                            relationship = "many-to-many")
output <- dat %>% 
  group_by(case, powertarget, datetime, source) %>% 
  summarize(power = sum(15*value*turbines/1e3),# GW
            cf = sum(value*turbines/2000)) 

summarytab <- output %>%  
  mutate(powertarget.txt = paste0(round(powertarget*100),"\\% (", powertarget * 2000*15/1e3, " GW)")) %>% 
  group_by(case,source, powertarget.txt) %>% 
  summarize(sd = sd(power),
            sdcf = sd(cf),
            `hours below 1.5GW (percent)` = paste0(round(100*mean(power<1.5),2),"\\%"),
            `hours above 28.5GW (percent)` = paste0(round(100*mean(power>28.5),2),"\\%"),
            `percentile power 5` = quantile(power, p = 0.05),
            `percentile power 95` = quantile(power, p = 0.95),
            "no production" = sum(power == 0)/24,
            "max production" = sum(power == 30)/24) %>% ungroup() %>% 
  arrange(source, powertarget.txt, case) %>% 
  relocate(powertarget.txt)




port.summary <- portfolios %>% 
  mutate(powertarget.txt = paste0(round(powertarget*100),"\\% (", powertarget * 2000*15/1e3, " GW)")) %>% 
  group_by(source, case, powertarget.txt) %>% 
  summarize("number of windparks" = n(),
            "Smallest windpark" = as.integer(min(turbines)),
            "Largest windpark" = as.integer(max(turbines))) 
summarytab <- summarytab %>% 
  right_join(port.summary, by = c("powertarget.txt", "source", "case")) %>% 
  relocate("source", "powertarget.txt", "case", "sd", "number of windparks", "Smallest windpark","Largest windpark")
output %>%  
  ggplot() +theme_bw()+
  geom_density(aes(x = power, fill = factor(powertarget)), alpha = 0.5, color = NA) + 
  facet_grid(source~case)+
  scale_y_continuous(expand =c(0,0), name = "Density", limits = c(0,0.07))


summarytab$source[1]<- "\\multirow{13}{*}{\\rotatebox[origin=c]{90}{NVE}}"
summarytab$source[14]<- "\\multirow{13}{*}{\\rotatebox[origin=c]{90}{Solbrekke}}"
summarytab$source[-c(1,14)] <- ""
summarytab$powertarget.txt[c(1,14)] <- "\\multirow{3}{*}{58\\% (17.4)}"
summarytab$powertarget.txt[c(4,17)] <- "\\multirow{5}{*}{60\\% (18.0)}"
summarytab$powertarget.txt[c(9,22)] <- "\\multirow{5}{*}{62\\% (18.6)}"
summarytab$powertarget.txt[-c(1,4,9,14,17,22)] <- ""
summarytab$case <- paste0("\\ref{case",summarytab$case,"}")
summarytab$sdtxt <- paste0(
  round(summarytab$sdcf * 100,1),"\\% (", round(summarytab$sd,2),")")
summarytab.print <- summarytab %>% 
  relocate(source, powertarget.txt, case, sdtxt) %>% 
  select(-sd,-sdcf)
# Create latex table: 
xtable(summarytab.print %>% ungroup(), digits = 2) %>% 
  print(include.rownames = FALSE, sanitize.text.function = function(x)x, 
        hline.after =c(0,13),
        file = "tables/summarytable_portfolios.tex")


# ---------------------

namesNVE <- NVE %>%  pull(name) %>% unique()
NVE.port <- portfolios %>% ungroup() %>% 
  filter(source == "NVE") %>% 
  select(case, powertarget, locID, turbines) %>% 
  pivot_wider(names_from = locID, values_from = turbines) 

unique(NVE$locID)[which(!(unique(NVE$locID)%in% names(NVE.port)))]
NVE.port <- NVE.port %>% 
  mutate("10" = NA,
         "15" = NA,
         "16" = NA)
NVE.port <- NVE.port[,c("powertarget", "case", unique(NVE$locID))]

NVE.port <- NVE.port %>% ungroup() %>% 
  arrange(powertarget, case)
names(NVE.port)[-(1:2)] <- unique(NVE$name)

SOL.port <- portfolios %>% 
  filter(source == "Solbrekke") %>% 
  select(case, powertarget, locID, turbines) %>% 
  pivot_wider(names_from = locID, values_from = turbines) 
unique(SOL$locID)[which(!(unique(SOL$locID)%in% names(SOL.port)))]
SOL.port <- SOL.port %>% 
  mutate("3" = NA,
         "8" = NA,
         "13"= NA,
         "24"= NA,
         "25"= NA)
SOL.port <- SOL.port[,c("powertarget", "case",unique(SOL$locID))]
SOL.port <- SOL.port %>% ungroup() %>% 
  arrange(powertarget, case)
#names(NVE)<- paste0("\\rotatebox[origin=c]{90}{", names(NVE),"}")

xtable(t(NVE.port), digits = 0) %>% 
  print(include.rownames = TRUE, sanitize.text.function = function(x)x,
        file = "tables/NVE_turbines_per_location.tex")
xtable(t(SOL.port), digits = 0) %>% 
  print(include.rownames = TRUE, sanitize.text.function = function(x)x,
        file = "tables/SOL_turbines_per_location.tex")

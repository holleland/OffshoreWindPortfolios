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
  mutate(powertarget.txt = paste0(round(powertarget*100),"\\%")) %>% 
  group_by(case,source, powertarget.txt) %>% 
  summarize(sd = sd(power),
            sdcf = sd(cf),
            `hours below 1.5GW (percent)` = paste0(round(100*mean(power<1.5),2),"\\%"),
            `hours above 28.5GW (percent)` = paste0(round(100*mean(power>28.5),2),"\\%"),
            `percentile power 5` = paste0(format(100*quantile(cf, p = 0.05),digits = 3), "\\%"),
            `percentile power 95` = paste0(format(100*quantile(cf, p = 0.95),digits = 3), "\\%"),
            "no production" = sum(power == 0)/24,
            "max production" = sum(power == 30)/24) %>% ungroup() %>% 
  arrange(source, powertarget.txt, case) %>% 
  relocate(powertarget.txt)




port.summary <- portfolios %>% 
  mutate(powertarget.txt = paste0(round(powertarget*100),"\\%")) %>% 
  group_by(source, case, powertarget.txt) %>% 
  summarize("number of windparks" = n(),
            "Smallest windpark" = as.integer(min(turbines)),
            "Largest windpark" = as.integer(max(turbines))) 
summarytab <- summarytab %>% 
  right_join(port.summary, by = c("powertarget.txt", "source", "case")) %>% 
  relocate("source", "powertarget.txt", "case", "sd", "number of windparks", "Smallest windpark","Largest windpark")


summarytab$source[1]<- "\\multirow{13}{*}{\\rotatebox[origin=c]{90}{NVE}}"
summarytab$source[14]<- "\\multirow{13}{*}{\\rotatebox[origin=c]{90}{S\\&S}}"
summarytab$source[-c(1,14)] <- ""
summarytab$powertarget.txt[c(1,14)] <- "\\multirow{3}{*}{58\\%}"
summarytab$powertarget.txt[c(4,17)] <- "\\multirow{5}{*}{60\\%}"
summarytab$powertarget.txt[c(9,22)] <- "\\multirow{5}{*}{62\\%}"
summarytab$powertarget.txt[-c(1,4,9,14,17,22)] <- ""
summarytab$case <- paste0("\\ref{case",summarytab$case,"}")
summarytab$sdtxt <- paste0(
  format(#round(
    summarytab$sdcf * 100,#1),
    digits=3))
summarytab.print <- summarytab %>% 
  relocate(source, powertarget.txt, case, sdtxt) %>% 
  select(-sd,-sdcf)%>% ungroup()
# Create latex table: 
xtable(summarytab.print[, c(1:7, 10:11)] , digits = 3) %>% 
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
  mutate("12" = NA)
SOL.port <- SOL.port[,c("powertarget", "case",sort(unique(SOL$locID)))]
SOL.port <- SOL.port %>% ungroup() %>% 
  arrange(powertarget, case)
#names(NVE)<- paste0("\\rotatebox[origin=c]{90}{", names(NVE),"}")
# adding row with maximum constraint



xtable(cbind(t(NVE.port), "maxTurb"=c(NA,NA,ceiling(maxWeights*2000))),
       digits = 0) %>% 
  print(include.rownames = TRUE, sanitize.text.function = function(x)x,
        file = "tables/NVE_turbines_per_location.tex")
xtable(cbind(t(SOL.port), "maxTurb"=c(NA,NA,ceiling(maxWeightsSOL*2000))), digits = 0) %>% 
  print(include.rownames = TRUE, sanitize.text.function = function(x)x,
        file = "tables/SOL_turbines_per_location.tex")


# ---------------------------------
NVE <- readRDS("data/NVE.rds") %>% 
  select(locID, name, lon,lat, value) %>% 
  group_by(locID, name,lon,lat) %>% 
  summarize(mean = mean(value),
            sd = sd(value))
(area <-readxl::read_excel("data/NVE_areal.xlsx") %>%
    mutate(name = str_replace(name, "\u00f8","o")) %>% 
    select(name, `area(km2)`) %>%
    mutate("Potential capacity (MW)" = `area(km2)`*3.5*1,
           "Max turbines" = `Potential capacity (MW)`/15) %>% 
    rename("Area (km$^2$)" = "area(km2)") %>% 
    left_join(NVE, by = "name") %>% 
    relocate(locID, name,lon,lat,mean,sd) %>% 
    mutate(lon    = as.character(round(lon,1)),
           lat    = as.character(round(lat,1)),
           mean   = paste0(round(100*mean,1),"\\%"),
           sd     = format(round(100*sd,1),digits = 3),
           weight = paste0(round(`Max turbines`/2000*100,1),"\\%"))) %>% 
  xtable::xtable(digits = 0) %>% 
  print(sanitize.text.function = function(x)x,
        include.rownames = FALSE)
sum(area$`Potential capacity (MW)`)/1e3
sum(area$`Max turbines`)
area <- area %>% 
  mutate(reg = c("\\multirow{4}{*}{\\rotatebox[origin=c]{90}{N}}", NA,NA,NA,
                 "\\multirow{3}{*}{\\rotatebox[origin=c]{90}{NW}}", NA, NA,
                 "\\multirow{6}{*}{\\rotatebox[origin=c]{90}{W}}", rep(NA, 5),
                 "\\multirow{6}{*}{\\rotatebox[origin=c]{90}{SW}}", rep(NA, 5),
                 "\\multirow{1}{*}{\\rotatebox[origin=c]{90}{SE}}"))
area  %>%relocate(reg) %>%  xtable::xtable(digits = 0) %>% 
  print(sanitize.text.function = function(x)x,
        include.rownames = FALSE)

# ---------------------------------
SOL <- readRDS("data/Solbrekke.rds") %>% 
  group_by(locID) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  left_join(readRDS("data/Solbrekke_locations.rds"), by = "locID")

base <- as.numeric(R.matlab::readMat("data/WPSS/SCORE_baseline.mat")$SCORE)
fish <- as.numeric(R.matlab::readMat("data/WPSS/SCORE_fisherman.mat")$SCORE)
eco <- as.numeric(R.matlab::readMat("data/WPSS/SCORE_ecologist.mat")$SCORE)
invest <- as.numeric(R.matlab::readMat("data/WPSS/SCORE_investor.mat")$SCORE)
SOL$baseline <-paste0(format(round(100*sapply(SOL$baseline, function(x)
return(sum(base >=x, na.rm=T)/sum(!is.na(base)))),1), digits = 3),"\\%")
SOL$investor <-paste0(format(round(100*sapply(SOL$investor, function(x)
  return(sum(invest >=x, na.rm=T)/sum(!is.na(invest)))),1), digits = 3),"\\%")
SOL$fisherman<- paste0(format(round(100*sapply(SOL$fisherman, function(x)
  return(sum(fish >=x, na.rm=T)/sum(!is.na(fish)))),1), digits = 3),"\\%")
SOL$ecologist <- paste0(format(round(100*sapply(SOL$ecologist, function(x)
  return(sum(eco >=x, na.rm=T)/sum(!is.na(eco)))),1), digits = 3),"\\%")
soltab <- SOL %>% 
  relocate(locID,lon,lat, dist2shore, 
           mean, sd, 
           baseline, investor,fisherman, ecologist) %>% 
  select(-row,-col) %>% 
  mutate(lon    = as.character(round(lon,1)),
         lat    = as.character(round(lat,1)),
         mean   = paste0(format(100*mean,digits = 3),"\\%"),
         sd     = as.character(format(100*sd,digits = 3)))

zeros <- which(soltab[, 7:10]==" 0.0\\%", arr.ind = T)
for(i in 1:nrow(zeros))
  soltab[zeros[i,1], zeros[i,2]+6] <- "<0.1\\%"

soltab[14, ]$fisherman  <- paste0("\\textbf{", soltab[14, ]$fisherman, "}")
soltab[14, ]$ecologist  <- paste0("\\textbf{", soltab[14, ]$ecologist ,"}")
soltab <- cbind("", soltab)
soltab[1,1] <- "\\multirow{6}{*}{\\rotatebox[origin=c]{90}{N}}"
soltab[7,1] <- "\\multirow{3}{*}{\\rotatebox[origin=c]{90}{N-W}}"
soltab[10,1] <- "\\multirow{6}{*}{\\rotatebox[origin=c]{90}{W}}"
soltab[16,1] <- "\\multirow{3}{*}{\\rotatebox[origin=c]{90}{S-W}}"
soltab[19,1] <- "\\multirow{2}{*}{\\rotatebox[origin=c]{90}{S-E}}"
soltab[,-5] %>% xtable::xtable(digits = 0) %>% 
  print(sanitize.text.function = function(x)x,
        include.rownames = FALSE)
# Range of mean and sd for Solbrekke locations: 
round(100*range(SOL$mean),1)
round(100*range(SOL$sd),1)

# Capacity factor of 50% at SN2 and UN:
mean(mu.SOL[c(1,10)])

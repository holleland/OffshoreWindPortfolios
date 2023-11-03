library(tidyverse)
library(parallel)
NVE <- readRDS("data/NVE.rds")
SOL <- readRDS("data/Solbrekke.rds") %>% select(-year)

Sigma.NVE <- NVE %>% 
  select(locID, value, datetime) %>% 
  pivot_wider(names_from = locID, values_from = value) %>% 
  select(-datetime) %>% 
  cov()
mu.NVE <- NVE %>%
  group_by(locID) %>% 
  summarize(m = mean(value)) %>% 
  arrange(locID) %>% 
  pull(m)
Sigma.SOL <- SOL %>% arrange(locID) %>% 
  pivot_wider(names_from = "locID", values_from = "value") %>% 
  select(-datetime) %>% 
  cov()
mu.SOL <- SOL %>%
  group_by(locID) %>% 
  summarize(m = mean(value)) %>% 
  arrange(locID) %>% 
  pull(m)


UNandSN2.NVE <- c(13L,19L)
UNandSN2.SOL <- c(14L,16L)

NVE.areas <- readxl::read_excel("data/NVE_areal.xlsx")
NVE.areas$name <- str_replace(NVE.areas$name, pattern="\u00f8",replacement = "o")
maxWeights <- NVE.areas$maxWeight


maxWeightsSOL <- rep(500/2000, length(mu.SOL))
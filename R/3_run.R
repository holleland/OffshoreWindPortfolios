# setting up grids: 
rm(list=ls())
library(quadprog)
library(tidyverse)
source("R/1_load_data.R")
source("R/2_functions.R")

# --------- CASE WITHOUT UN and SN2 --------
candidates = 1:length(mu.NVE)
grid.5L.NVE <- gtools::combinations(
  n = length(candidates),
  r = 5L, 
  v = candidates
) %>% as.data.frame()
grid.5L.NVE <- bind_rows(
  grid.5L.NVE %>% mutate(powertarget = 0.58),
  grid.5L.NVE %>% mutate(powertarget = 0.60),
  grid.5L.NVE %>% mutate(powertarget = 0.62))%>% 
  as_tibble()

candidates = 1:length(mu.SOL)
grid.5L.SOL <- gtools::combinations(
  n = length(candidates),
  r = 5L, 
  v = candidates
) %>% as.data.frame()
grid.5L.SOL <- bind_rows(
  grid.5L.SOL %>% mutate(powertarget = 0.58),
  grid.5L.SOL %>% mutate(powertarget = 0.60),
  grid.5L.SOL %>% mutate(powertarget = 0.62))%>% 
  as_tibble()


# --------- CASE WITH UN and SN2 --------
candidates = 1:length(mu.NVE)
candidates<-candidates[-UNandSN2.NVE]
grid.3L.NVE <- gtools::combinations(
  n = length(candidates),
  r = 3L, 
  v = candidates
) %>% as.data.frame()
grid.3L.NVE <- bind_rows(
  grid.3L.NVE %>% mutate(powertarget = 0.58),
  grid.3L.NVE %>% mutate(powertarget = 0.60),
  grid.3L.NVE %>% mutate(powertarget = 0.62)) %>% 
  mutate(V4 = UNandSN2.NVE[1], V5 = UNandSN2.NVE[2]) %>% 
  relocate(V1,V2,V3,V4,V5,powertarget)%>% 
  as_tibble()

candidates = 1:length(mu.SOL)
candidates<-candidates[-UNandSN2.SOL]
grid.3L.SOL <- gtools::combinations(
  n = length(candidates),
  r = 3L, 
  v = candidates
) %>% as.data.frame()
grid.3L.SOL <- bind_rows(
  grid.3L.SOL %>% mutate(powertarget = 0.58),
  grid.3L.SOL %>% mutate(powertarget = 0.60),
  grid.3L.SOL %>% mutate(powertarget = 0.62)) %>% 
  mutate(V4 = UNandSN2.SOL[1], V5 = UNandSN2.SOL[2]) %>% 
  relocate(V1,V2,V3,V4,V5,powertarget) %>% 
  as_tibble()



# ------- CASE A -----------
NVE.case.A <- lapply(c(.58,.6,.62), 
                     optimize_portfolio,
                     mu = mu.NVE,
                     Sigma = Sigma.NVE,
                     maxWeights = maxWeights
) %>% bind_rows() %>% 
  mutate(source = "NVE",
         case = "A") %>% 
  select(-std)

SOL.case.A <- lapply(c(.58,.6,.62), 
                     optimize_portfolio,
                     mu = mu.SOL,
                     Sigma = Sigma.SOL,
                     maxWeights = maxWeightsSOL
) %>% bind_rows() %>% 
  mutate(source = "S&S",
         case = "A")%>% 
  select(-std)


# ------- CASE B -----------
cl <- makeCluster(6)
(NVE.case.B <- run.5L.optimization(grid = grid.3L.NVE,
                                         mu = mu.NVE, 
                                         Sigma = Sigma.NVE, 
                                         source = "NVE", 
                                         maxWeights = maxWeights,
                                         cl = cl)%>% 
    mutate(case = "B"))
(SOL.case.B <- run.5L.optimization(grid = grid.3L.SOL,
                                         mu = mu.SOL, 
                                         Sigma = Sigma.SOL, 
                                         source = "S&S",
                                         maxWeights = maxWeightsSOL,
                                         #minWeights = rep(0, length(mu.SOL)),
                                         #maxWeights = rep(1, length(mu.SOL)),
                                         cl = cl)%>% 
    mutate(case = "B"))

# ------- CASE C-----------
(NVE.case.C <- run.5L.optimization(grid = grid.5L.NVE, 
                                         mu = mu.NVE, 
                                         Sigma = Sigma.NVE, 
                                         source = "NVE", 
                                         maxWeights = maxWeights,
                                         cl = cl) %>% 
   mutate(case = "C"))
(SOL.case.C <- run.5L.optimization(grid = grid.5L.SOL, 
                                         mu = mu.SOL, 
                                         Sigma = Sigma.SOL, 
                                         source = "S&S",
                                         maxWeights = maxWeightsSOL, 
                                         cl = cl)%>% 
    mutate(case = "C"))
stopCluster(cl)




# Iterative build: 
# ------- CASE D -----------
NVE.case.D.runs <- lapply(c(.6,.62), function(x)
                     iterative.build(Twp = x, 
                                     find.initial.weights = FALSE,
                              start.locs = UNandSN2.NVE,
                              mu = mu.NVE,
                              stepsize = 1.5,
                              Sigma = Sigma.NVE,
                              maxWeights = maxWeights))
NVE.sequential <- bind_rows(NVE.case.D.runs[[1]]$allsteps,
                            NVE.case.D.runs[[2]]$allsteps) %>% 
  mutate(source = "NVE", case = "D")

NVE.case.D <- bind_rows(NVE.case.D.runs[[1]]$final,
                        NVE.case.D.runs[[2]]$final) %>% 
  mutate(source = "NVE",
         case = "D") %>% 
  select(-step,-total,-std)
SOL.case.D.runs <-  lapply(c(.6,.62), function(x)
  iterative.build(Twp = x, 
                  find.initial.weights = FALSE,
                  start.locs = UNandSN2.SOL,
                  stepsize = 1.5,
                  mu = mu.SOL,
                  Sigma = Sigma.SOL,
                  maxWeights = maxWeightsSOL))
SOL.sequential <- bind_rows(SOL.case.D.runs[[1]]$allsteps,
                            SOL.case.D.runs[[2]]$allsteps) %>% 
  mutate(source = "S&S", case = "D")
SOL.case.D <- bind_rows(SOL.case.D.runs[[1]]$final,
                        SOL.case.D.runs[[2]]$final) %>% 
  mutate(source = "S&S",
         case = "D") %>% 
  select(-step,-total, -std) 

# ----- CASE E -------
NVE.case.E.sequential.runs <- lapply(c(.6,.62), function(x)
  iterative.build(Twp = x, 
                  find.initial.weights = FALSE,
                  start.locs = UNandSN2.NVE,
                  mu = mu.NVE,
                  stepsize = 1.5,
                  maxGW = 7.5,
                  Sigma = Sigma.NVE,
                  maxWeights = maxWeights))

NVE.case.E <- lapply(NVE.case.E.sequential.runs, FUN =function(x){
  tmp <- x$final
  minWeights <- tmp$weights * 7.5 / 30
  locations <- tmp$locID
  try(optimize_portfolio(Twp = tmp$powertarget[1],
                         mu= mu.NVE[locations],
                         Sigma = Sigma.NVE[locations,locations],
                         locID = locations,
                         minWeights = minWeights,
                         maxWeights = maxWeights[locations]), 
      silent = TRUE)
  }) %>% 
  bind_rows() %>% 
  select(-std) %>% 
  mutate(source = "NVE",
         case = "E")

SOL.case.E.sequential.runs <- lapply(c(.6,.62), function(x)
  iterative.build(Twp = x, 
                  find.initial.weights = FALSE,
                  start.locs = UNandSN2.SOL,
                  mu = mu.SOL,
                  stepsize = 1.5,
                  maxGW = 7.5,
                  Sigma = Sigma.SOL,
                  maxWeights = maxWeightsSOL))

SOL.case.E<- lapply(SOL.case.E.sequential.runs, FUN =function(x){
  tmp <- x$final
  minWeights <- tmp$weights * 7.5 / 30
  locations <- tmp$locID
  try(optimize_portfolio(Twp = tmp$powertarget[1],
                         mu= mu.SOL[locations],
                         Sigma = Sigma.SOL[locations,locations],
                         locID = locations,
                         minWeights = minWeights,
                         maxWeights = maxWeightsSOL[locations]), 
      silent = TRUE)
}) %>% 
  bind_rows() %>% 
  select(-std) %>% 
  mutate(source = "S&S",
         case = "E")



# ---------------------
NVE.cases <- bind_rows(
  NVE.case.A,
  NVE.case.B,
  NVE.case.C,
  NVE.case.D,
  NVE.case.E
)
SOL.cases <- bind_rows(
  SOL.case.A,
  SOL.case.B,
  SOL.case.C,
  SOL.case.D,
  SOL.case.E
)

all.cases <- bind_rows(
  NVE.cases,
  SOL.cases
) %>% 
  mutate(turbines = round(2000*weights)) %>% 
  filter(turbines >0) %>% 
  group_by(case,source, powertarget) %>% 
  mutate(tot = sum(turbines))

# -- Correction for number of turbines not adding to 2000: --
# ____ Locations where we should remove turbines: ____
rmTurb <- all.cases %>% 
  mutate(missingTurbines = 2000-tot,
         diff = weights*2000-turbines) %>% 
  filter(missingTurbines < 0) %>% 
  group_by(source, powertarget, case) %>% 
  arrange(source, powertarget, case, diff) %>% 
  filter(row_number() <=abs(missingTurbines)) %>% 
  mutate(addTurbines = -1) %>% 
  select(locID, powertarget, source, case, addTurbines)
# ____ Locations where we should add turbines: ____
addTurb <- all.cases %>% 
  mutate(missingTurbines = 2000-tot,
         diff = weights*2000-turbines) %>% 
  filter(missingTurbines > 0) %>% 
  group_by(source, powertarget, case) %>% 
  arrange(source, powertarget, case, desc(diff) ) %>% 
  filter(row_number() <=abs(missingTurbines)) %>% 
  mutate(addTurbines = 1) %>% 
  select(locID, powertarget, source, case, addTurbines)
all.cases <- all.cases %>% left_join(bind_rows(rmTurb, addTurb)) %>% 
  mutate(addTurbines = ifelse(is.na(addTurbines),0,addTurbines),
         turbines = turbines + addTurbines) %>% 
  select(-addTurbines, -tot)

# --- Check: -----
all.cases %>% 
  group_by(powertarget, case, source) %>% 
  summarize(tot = sum(turbines)) %>% 
  ungroup() %>% summarize(all(tot==2000L))
# -------


saveRDS(all.cases, 
        file = "output/portfolioweights_all_cases.rds")

# case D: All steps
saveRDS(bind_rows(
  NVE.sequential, SOL.sequential
),
file = "output/caseD_allsteps.rds")

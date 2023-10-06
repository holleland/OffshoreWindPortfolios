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
                     Sigma = Sigma.NVE
) %>% bind_rows() %>% 
  mutate(source = "NVE",
         case = "A") %>% 
  select(-std)

SOL.case.A <- lapply(c(.58,.6,.62), 
                     optimize_portfolio,
                     mu = mu.SOL,
                     Sigma = Sigma.SOL
) %>% bind_rows() %>% 
  mutate(source = "Solbrekke",
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
                                         source = "Solbrekke",
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
                                         source = "Solbrekke", 
                                         cl = cl)%>% 
    mutate(case = "C"))
stopCluster(cl)




# Iterative build: 
# ------- CASE D -----------
NVE.case.D.runs <- lapply(c(.6,.62), function(x)
                     iterative.build(Twp = x, 
                                     find.initial.weights = FALSE,
                              start.locs = c(13L, 19L),
                              mu = mu.NVE,
                              stepsize = 1.5,
                              Sigma = Sigma.NVE,
                              maxWeights = maxWeights))
NVE.case.D <- bind_rows(NVE.case.D.runs[[1]]$final,
                        NVE.case.D.runs[[2]]$final) %>% 
  mutate(source = "NVE",
         case = "D") %>% 
  select(-step,-total)
SOL.case.D.runs <-  lapply(c(.6,.62), function(x)
  iterative.build(Twp = x, 
                  find.initial.weights = FALSE,
                  start.locs = c(1L, 10L),
                  stepsize = 1.5,
                  mu = mu.SOL,
                  Sigma = Sigma.SOL,
                  maxWeights = rep(1,length(mu.SOL))))
SOL.case.D <- bind_rows(SOL.case.D.runs[[1]]$final,
                        SOL.case.D.runs[[2]]$final) %>% 
  mutate(source = "Solbrekke",
         case = "D") %>% 
  select(-step,-total)

# ----- CASE E -------
NVE.first5 <- rbind(NVE.case.D.runs[[1]]$locations[1:5],
                    NVE.case.D.runs[[2]]$locations[1:5])

NVE.case.E<-
  bind_rows(
    optimize_portfolio(
      Twp = .6, 
      mu = mu.NVE[NVE.first5[1,]],
      Sigma = Sigma.NVE[NVE.first5[1,],NVE.first5[1,]],
      maxWeights = maxWeights[NVE.first5[1,]],  locID = NVE.first5[1,]
),
optimize_portfolio(
  Twp = .62, 
  mu = mu.NVE[NVE.first5[2,]],
  Sigma = Sigma.NVE[NVE.first5[2,],NVE.first5[2,]],
  maxWeights = maxWeights[NVE.first5[2,]],  locID = NVE.first5[2,]
)) %>% 
  mutate(source = "NVE",
         case = "E") %>% 
  select(-std)
SOL.first5 <-  rbind(SOL.case.D.runs[[1]]$locations[1:5],
                     SOL.case.D.runs[[2]]$locations[1:5])
SOL.case.E<-
  bind_rows(
    optimize_portfolio(
      Twp = .6, 
      mu = mu.SOL[SOL.first5[1,]],
      Sigma = Sigma.SOL[SOL.first5[1,],SOL.first5[1,]],
      locID = SOL.first5[1,]
    ),
    optimize_portfolio(
      Twp = .62, 
      mu = mu.SOL[SOL.first5[2,]],
      Sigma = Sigma.SOL[SOL.first5[2,],SOL.first5[2,]],
      locID = SOL.first5[2,]
    )) %>% 
  mutate(source = "Solbrekke",
         case = "E") %>% 
  select(-std)


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

# -----------------
if(source == "NVE"){
  portfolios <- portfolios %>% rename("locID" = "name") %>%
    mutate(name = locNames[locID])
  portfolios <- readxl::read_excel("data/NVE_20_locations.xlsx") %>%
    mutate(name = str_replace(name, "\u00f8",replacement = "o")) %>% 
    select( name, lon,lat) %>% 
    group_by(name) %>% 
    summarize(lon = mean(lon), lat = mean(lat)) %>% 
    right_join(portfolios , by = "name") %>% 
    mutate(turbines = round(2000*weights))
}else{
  portfolios <- portfolios %>% 
    mutate(locID = as.numeric(locID)) %>% 
    filter(!is.na(powertarget))
  portfolios <- readRDS("data/Solbrekke_locations.rds") %>%
    select(locID, lon,lat) %>% 
    right_join(portfolios , by = "locID") %>% 
    mutate(turbines = round(2000*weights))
}
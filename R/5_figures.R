# --------- FIGURES ----------
library(tidyverse)
library(sf)
locNames <- readRDS("data/NVE.rds") %>% pull(name) %>% unique() %>% 
  as_tibble() %>% mutate(locID = as.integer(1:n())) %>% rename(name = value)
#NVE.portfolios <- readRDS("output/portfolioweights_all_cases.rds") %>% 
  

NVE.portfolios <- readxl::read_excel("data/NVE_20_locations.xlsx") %>%
  mutate(name = str_replace(name, "\u00f8",replacement = "o")) %>%
  left_join(locNames, by = "name") %>% 
  group_by(locID) %>% 
  summarize(lon = mean(lon), lat = mean(lat))  %>% 
  right_join(
    readRDS("output/portfolioweights_all_cases.rds") %>% 
               filter(source == "NVE") %>% ungroup(), by = "locID") 

SOL.portfolios <- readRDS("data/Solbrekke_locations.rds") %>%
  select(locID, lon,lat) %>% 
  right_join( 
    readRDS("output/portfolioweights_all_cases.rds") %>% 
                filter(source == "S&S") %>% ungroup(), by = "locID") 

portfolios <- bind_rows(NVE.portfolios, SOL.portfolios)

proj <-  "+proj=lcc +lat_0=66.3 +lon_0=-42 +lat_1=66.3 +lat_2=66.3 +no_defs +R=6.371e+06"
land <- rnaturalearth::ne_countries(continent = "Europe", 
                                    returnclass = "sf", scale = 10) %>%
  st_transform(proj) %>% 
  select(geometry)

locsSF <- st_as_sf(portfolios, coords = c("lon","lat"), crs= 4326)  %>% st_transform(proj)
locsSF$lon <- st_coordinates(locsSF)[,"X"]
locsSF$lat <- st_coordinates(locsSF)[,"Y"]
locations <- readxl::read_excel("C:/Users/s15052/Dropbox/NHH/Prosjekter/Wind/OptimalWindFarmPortfolio/data/NVE_20_locations.xlsx") %>% 
  mutate(name = str_replace(name, "\u00f8","o")) %>% st_as_sf(coords = c("lon","lat"), crs= 4326)  %>% st_transform(proj)
locations$lon <- st_coordinates(locations)[,"X"]
locations$lat <- st_coordinates(locations)[,"Y"]
locsSF <- locsSF %>% 
  mutate(powertargettxt = factor(paste0(powertarget*100,"%"),
                                 levels = c("62%",
                                            "60%",
                                            "58%")))



p2NVE <- land %>%
  ggplot() + 
  geom_sf(pch = 15) +
  geom_polygon(data=locations, fill = "blue", aes(x=lon,y=lat, group = name), alpha = .2)+
  coord_sf(expand = FALSE, xlim = range(locsSF$lon)+c(-1e5,1e5), ylim = range(locsSF$lat)+c(-1e5,1e5)) + 
  #geom_point(data = locsSF,
  #           aes(x=lon,y=lat, size = turbines, color = turbines))+
  geom_text(data = locsSF %>% filter(source == "NVE"),
            aes(x=lon,y=lat, label = turbines), vjust=0, angle = 90)+
  #scale_color_discrete()+
  facet_grid(powertargettxt~case, switch = "x")+# 
  # geom_text(data = filter(locsSF, `no additional constraints`>0),vjust= 1.5,
  #            aes(x=lon,y=lat, label = name),angle=90, color = "red", size = 4)+
  #  geom_text(data = filter(locsSF, `no additional constraints`>0),vjust= -1,
  #            aes(x=lon,y=lat, label =  `no additional constraints`),angle=90, color = "red", size = 4)+
  
  #geom_text(data = locsSF %>% filter(turbines >0),aes(x=lon,y=lat, label = locID), color = "red", size = 2)+
  theme_bw()+
  #facet_wrap(~name, ncol = 1, strip.position = "right")+
  #scale_color_viridis_c(name = "Number of turbines")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "white", color = "transparent"),
        strip.text.y.right = element_text(angle = 90),
        strip.text.x = element_text(angle = 90),
        text = element_text(size = 13),
        plot.caption.position = "plot",
        legend.title = element_text(angle = 90, vjust =.9, size = 18),
        legend.text = element_text(angle = 90, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = "white"),
        legend.background = element_rect(fill = "white", color = "white"))#+
ggsave(p2NVE, file = "figures/portfolios_maps_NVE.png", height = 12,width = 10)
#guides(color =guide_colorbar(barheight = 25, reverse =FALSE),
#      size = guide_none())) 
p2S <- land %>%
  ggplot() + 
  geom_sf(pch = 15) +
  geom_polygon(data=locations, fill = "blue", aes(x=lon,y=lat, group = name), alpha = .2)+
  coord_sf(expand = FALSE, xlim = range(locsSF$lon)+c(-1e5,1e5), ylim = range(locsSF$lat)+c(-1e5,1e5)) + 
  geom_point(data = locsSF%>% filter(source == "S&S"),
             aes(x=lon,y=lat), color = "red")+
  geom_text(data = locsSF %>% filter(source == "S&S"),
            aes(x=lon,y=lat, label = turbines), vjust=0, angle = 90)+
  #scale_color_discrete()+
  facet_grid(powertargettxt~case, switch = "x")+# 
  # geom_text(data = filter(locsSF, `no additional constraints`>0),vjust= 1.5,
  #            aes(x=lon,y=lat, label = name),angle=90, color = "red", size = 4)+
  #  geom_text(data = filter(locsSF, `no additional constraints`>0),vjust= -1,
  #            aes(x=lon,y=lat, label =  `no additional constraints`),angle=90, color = "red", size = 4)+
  
  #geom_text(data = locsSF %>% filter(turbines >0),aes(x=lon,y=lat, label = locID), color = "red", size = 2)+
  theme_bw()+
  #facet_wrap(~name, ncol = 1, strip.position = "right")+
  #scale_color_viridis_c(name = "Number of turbines")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "white", color = "transparent"),
        strip.text.y.right = element_text(angle = 90),
        strip.text.x = element_text(angle = 90),
        text = element_text(size = 13),
        plot.caption.position = "plot",
        legend.title = element_text(angle = 90, vjust =.9, size = 18),
        legend.text = element_text(angle = 90, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = "white"),
        legend.background = element_rect(fill = "white", color = "white"))#+
ggsave(p2S, file = "Figures/portfolios_maps_Solbrekke.png", height = 12,width = 10)


# Visualizing covariance matrices 
source("R/1_load_data.R")

regions <- tibble(
  source = c(rep("NVE", 4),rep("S&S", 4)),
  ids = c(4.5, 7.5, 13.5, 19.5,
          6.5,9.5, 15.5,18.5)
)

Sigma.NVE %>% reshape2::melt() %>% as_tibble() %>% 
  mutate(source = "NVE", type = "cov") %>% 
  bind_rows(
Sigma.SOL %>% reshape2::melt() %>% as_tibble() %>% 
  mutate(source = "S&S", type = "cov"),
diag(1/sqrt(diag(Sigma.NVE)))%*%Sigma.NVE%*%diag(1/sqrt(diag(Sigma.NVE))) %>% 
  reshape2::melt() %>% as_tibble() %>% 
  mutate(source = "NVE", type = "cor"),
diag(1/sqrt(diag(Sigma.SOL)))%*%Sigma.SOL%*%diag(1/sqrt(diag(Sigma.SOL))) %>% 
    reshape2::melt() %>% as_tibble() %>% 
    mutate(source = "S&S", type = "cor") 
  ) %>% 
  filter(type == "cor") %>% 
  ggplot(aes(x=Var1, y = Var2, fill = value, color = value)) + geom_tile()+
  facet_wrap(~source, scales = "free", strip.position = "bottom") + theme_bw()+
  theme(strip.placement = "outside",
    panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", color = "transparent"),
    strip.text =element_text(size = 16),
    axis.title = element_blank(),
    axis.text = element_text(size = 14))+
  geom_hline(data = regions, aes(yintercept = ids), col = 2, lty = 2)+
  geom_vline(data = regions, aes(xintercept = ids), col = 2, lty = 2)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1,20,1), name = "Location", position = "top")+
  scale_y_reverse(expand = c(0,0), breaks = seq(1,20,1), name = "Location")+ 
  scale_fill_viridis_c(name = "Correlation")+ 
  scale_color_viridis_c(name = "Correlation")+
  guides(fill = guide_colorbar(barheight = 20))
ggsave("Figures/CorrelationMatrices.png", width = 16/4*3, height = 8/4*3)


# ----
source("R/1_load_data.R")
portfolios <- readRDS("output/portfolioweights_all_cases.rds") 


dat <- SOL %>% mutate(source = "S&S") %>% 
  bind_rows(
    NVE %>% select(value, locID, datetime) %>% mutate(source = "NVE"))


dat <- dat %>% right_join(portfolios %>% select(locID, powertarget, turbines, case, weights, source), 
                          by = c("locID","source"),
                          relationship = "many-to-many")
output <- dat %>% 
  group_by(case, powertarget, datetime, source) %>% 
  summarize(power = sum(15*value*turbines/1e3),# GW
            cf = sum(value*turbines/2000)) %>% 
  group_by(case, powertarget, source) %>% 
  summarize(
    sd = sd(cf)
  )
assets <- tibble(
  mean = c(mu.NVE, mu.SOL),
  sd = sqrt(c(diag(Sigma.NVE), diag(Sigma.SOL))),
  source = c(rep("NVE", 20), rep("S&S", 20)),
  locID = c(1:20, 1:20)
)
ggplot(output, aes(y= powertarget, x = 100*sd)) + 
  geom_text(aes(label = case)) +
  geom_point(data = assets, aes(y=mean, x= 100*sd))+
  facet_wrap(~source, ncol = 1) + 
  theme_bw() + 
  theme(panel.grid.major.x= element_blank(),
        panel.grid.minor  = element_blank(),
        strip.background = element_rect(fill = "white", color = "transparent"),
        strip.text = element_text(size = 13)) + 
  scale_x_continuous(name = "Portfolio standard deviation (pp)") + 
  scale_y_continuous(name = "Portfolio capacity factor",labels = scales::percent,
                     breaks = seq(.4,.7,0.02))
ggsave(file = "figures/Portfolios_graph.png", width = 10, height = 3.5)



# ---------- CASE D -----------

sequential <- readRDS("output/caseD_allsteps.rds")

nloc <- sequential %>% 
  mutate(turbines = round(weights * total*1000/15)) %>% 
  filter(turbines >0) %>% 
  group_by( powertarget, source, std,total) %>% 
  summarize(n = n()) %>% 
  ungroup()

ggplot(sequential, 
       aes(x = total, y = 100*std, color = paste0(round(powertarget*100,1), "%")))+ 
  geom_line(show.legend = FALSE)+ 
  geom_point()+
  geom_text(data = nloc, aes(x =total, y = 100*std, label = n, 
                             color = paste0(round(powertarget*100,1), "%")), 
            vjust = -1,
            show.legend = FALSE) +
  facet_wrap(~source)+ 
  theme_bw() + 
  theme(panel.grid.major.x= element_blank(),
        panel.grid.minor  = element_blank(),
        strip.background = element_rect(fill = "white", color = "transparent"),
        strip.text = element_text(size = 13),
        #legend.title = element_blank(),
        legend.position = c(.96,.90),,
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.background = element_rect(fill = "transparent")) +
  geom_hline(data = output %>% filter(case =="A", powertarget != .58),
             aes(yintercept = 100*sd, 
                 color = paste0(round(powertarget*100,1), "%")),
             lty = 2,show.legend = FALSE)+
  scale_color_discrete(name = "Target") + 
  scale_y_continuous(limits = c(19.8,36), name = "Portfolio standard deviation (pp)")+
  scale_x_continuous(expand = c(0,0), limits =c(0,32),name = "Installed capacity (GW)" ,
                     breaks = seq(0,30,3))+
  guides(color = guide_legend(override.aes = list(pch = 15, size = 5)))
ggsave("figures/sequential_build_out_std.png", width = 12, height = 5)
  

NVE60 <- sequential %>% mutate(turbines = round(weights * total*1000/15)) %>% 
  filter(turbines >0) %>% 
  select(total, locID, powertarget, turbines, source) %>% 
  filter(source == "NVE", powertarget ==0.6) %>% 
  pivot_wider(names_from = locID, values_from = turbines) %>% 
  mutate(total = paste0(round(total, digits = 1)),
         powertarget = paste0(round(100*powertarget, digits = 0), "\\%")) %>%
  relocate(source,powertarget)
NVE62 <- sequential %>% mutate(turbines = round(weights * total*1000/15)) %>% 
  filter(turbines >0) %>% 
  select(total, locID, powertarget, turbines, source) %>% 
  filter(source == "NVE", powertarget ==0.62) %>% 
  pivot_wider(names_from = locID, values_from = turbines) %>% 
  mutate(total = paste0(round(total, digits = 1)),
         powertarget = paste0(round(100*powertarget, digits = 0), "\\%")) %>%
  relocate(source,powertarget)
SOL60 <- sequential %>% mutate(turbines = round(weights * total*1000/15)) %>% 
  filter(turbines >0) %>% 
  select(total, locID, powertarget, turbines, source) %>% 
  filter(source == "S&S", powertarget ==0.6) %>% 
  pivot_wider(names_from = locID, values_from = turbines) %>% 
  mutate(total = paste0(round(total, digits = 1)),
         powertarget = paste0(round(100*powertarget, digits = 0), "\\%")) %>%
  relocate(source,powertarget)
SOL62 <- sequential %>% mutate(turbines = round(weights * total*1000/15)) %>% 
  filter(turbines >0) %>% 
  select(total, locID, powertarget, turbines, source) %>% 
  filter(source == "S&S", powertarget ==0.62) %>% 
  pivot_wider(names_from = locID, values_from = turbines) %>% 
  mutate(total = paste0(round(total, digits = 1)),
         powertarget = paste0(round(100*powertarget, digits = 0), "\\%")) %>%
  relocate(source,powertarget)

NVE62$source <- NVE60$source <- ""
NVE60$source[1] <- NVE62$source[1] <- "\\multirow{19}{*}{\\rotatebox[origin=c]{90}{NVE}}"
SOL62$source <- SOL60$source <- ""
SOL60$source[1] <- SOL62$source[1] <- "\\multirow{19}{*}{\\rotatebox[origin=c]{90}{S&S}}"
NVE62$powertarget <- NVE60$powertarget <- ""
NVE60$powertarget[1] <- "\\multirow{19}{*}{\\rotatebox[origin=c]{90}{60\\%}}"
NVE62$powertarget[1] <-  "\\multirow{19}{*}{\\rotatebox[origin=c]{90}{62\\%}}"
SOL62$powertarget <- SOL60$powertarget <- ""
SOL60$powertarget[1] <- "\\multirow{19}{*}{\\rotatebox[origin=c]{90}{60\\%}}"
SOL62$powertarget[1] <-  "\\multirow{19}{*}{\\rotatebox[origin=c]{90}{62\\%}}"

NVE60  %>% 
xtable::xtable( digits = 0) %>% 
  xtable::print.xtable(include.rownames =FALSE,
                       sanitize.text.function = function(x)x)
NVE62  %>% 
  xtable::xtable( digits = 0) %>% 
  xtable::print.xtable(include.rownames =FALSE,
                       sanitize.text.function = function(x)x)
SOL60  %>% 
  xtable::xtable( digits = 0) %>% 
  xtable::print.xtable(include.rownames =FALSE,
                       sanitize.text.function = function(x)x)
SOL62  %>% 
  xtable::xtable( digits = 0) %>% 
  xtable::print.xtable(include.rownames =FALSE,
                       sanitize.text.function = function(x)x)
ncol(SOL60)





# ------------------------------------------
rm(list=ls())
source("R/1_load_data.R")
source("R/2_functions.R")
library(quadprog)
caseA_efficient_frontier <- bind_rows(
  lapply(seq(.56,.65,.001),
                     optimize_portfolio,
                     mu = mu.NVE,
                     Sigma = Sigma.NVE,
         maxWeights=maxWeights
) %>% bind_rows() %>% 
  mutate(source = "NVE",
         case = "A") %>% 
  select(source, case, powertarget, std),
lapply(seq(.56,.65,.001), 
       optimize_portfolio,
       mu = mu.SOL,
       Sigma = Sigma.SOL,
       maxWeights = maxWeightsSOL
) %>% bind_rows() %>% 
  mutate(source = "S&S",
         case = "A"))
min.port <- caseA_efficient_frontier %>% group_by(source) %>%
  select(source, case, powertarget, std) %>% 
  distinct() %>% 
  filter(std == min(std))
caseA_efficient_frontier %>% 
  select(source, case, powertarget, std) %>% 
  ggplot(aes(x =100*std, y= powertarget, color = source))+
  geom_path(lwd = 1, lty = 1) + theme_bw()+ 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.93,.105))+
  geom_segment(data = min.port, aes(x = -Inf, xend = 100*std, y = powertarget, yend = powertarget), lty = 2, lwd = 1)+
  geom_segment(data = min.port, aes(x = 100*std, xend = 100*std, y = -Inf, yend = powertarget), lty = 2, lwd =1)+
  scale_x_continuous(name = "Portfolio standard deviation (pp)",
                     breaks = seq(20,28,2))+
  scale_y_continuous( name = "Portfolio capacity factor", labels = scales::percent, 
                      breaks = seq(.56,.64,.02))
ggsave("figures/effiecient_frontier_caseA.png", width = 8, height = 4)


portfolios <- readRDS("output/portfolioweights_all_cases.rds") 

dat <- SOL %>% mutate(source = "S&S") %>% 
  bind_rows(
    NVE %>% select(value, locID, datetime) %>% mutate(source = "NVE"))


dat <- dat %>% right_join(portfolios %>% select(locID, powertarget, turbines, case, weights, source), 
                          by = c("locID","source"),
                          relationship = "many-to-many")
output <- dat %>% 
  group_by(case, powertarget, datetime, source) %>% 
  summarize(power = sum(15*value*turbines/1e3),# GW
            cf = sum(value*turbines/2000)) %>% 
  group_by(case, powertarget, source) %>% 
  summarize(
    sd = sd(cf)
  )
assets <- tibble(
  mean = c(mu.NVE, mu.SOL),
  sd = sqrt(c(diag(Sigma.NVE), diag(Sigma.SOL))),
  source = c(rep("NVE", length(mu.NVE)), rep("S&S", length(mu.SOL))),
  locID = c(1:length(mu.NVE), 1:length(mu.SOL))
)

diversification <- caseA_efficient_frontier%>%
  filter(source == "NVE") %>% 
  select(powertarget, std) %>% distinct() %>% 
  filter(round(powertarget,3) == round(mu.NVE[13], 3)) %>% 
  rename(xend = std, yend = powertarget) %>% 
  mutate(
    y = mu.NVE[13],
    x = sqrt(diag(Sigma.NVE)[13]),
  )

ggplot(output, aes(y= powertarget, x = 100*sd))+
  geom_path(data = caseA_efficient_frontier,
            aes(x = 100*std, y = powertarget, col = source), lwd = 1, lty = 1,
            show.legend = FALSE) + 
  geom_text(aes(label = case, col = source),show.legend = FALSE) +
  geom_point(data = assets, aes(y=mean, x= 100*sd, col = source))+
  theme_bw() + 
  guides(color = guide_legend(override.aes = list(pch = 15, size = 6)))+
  theme(panel.grid.major.x= element_blank(),
        panel.grid.minor  = element_blank(),
        strip.background = element_rect(fill = "white", color = "transparent"),
        strip.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.position = c(0.05,.915),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent")) + 
  scale_x_continuous(name = "Portfolio standard deviation (pp)") + 
  scale_y_continuous(name = "Portfolio capacity factor",labels = scales::percent,
                     breaks = seq(.4,.7,0.02))+
  geom_segment(data = diversification, aes(x = x*100, xend = xend*100, 
                                           y = y, yend =yend), color = "blue", lwd = 1, 
               arrow = arrow(length = unit(0.2, "cm")))+
  geom_text(data = diversification, 
            aes(x = 100*(xend+x)/2, y = (yend+y)/2, 
                label = paste0("Diversifcation effect: ", round( 100*(1-xend/x),1),"%")),
            hjust = 0.5, vjust = -.6,
            color = "blue")
ggsave(file = "figures/Portfolios_graph_with_efficient_frontier.png", width = 10, height = 4)



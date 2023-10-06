# --------- FIGURES ----------
library(tidyverse)
library(sf)
locNames <- readRDS("data/NVE.rds") %>% pull(name) %>% unique() %>% 
  as_tibble() %>% mutate(locID = as.integer(1:n())) %>% rename(name = value)
NVE.portfolios <- readRDS("output/portfolioweights_all_cases.rds") %>% 
  

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
                filter(source == "Solbrekke") %>% ungroup(), by = "locID") 

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
  mutate(powertargettxt = factor(paste0(powertarget*100,"% (", 2000*powertarget*15/1e3, " GW)"),
                                 levels = c("62% (18.6 GW)",
                                            "60% (18 GW)",
                                            "58% (17.4 GW)")))



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
  geom_point(data = locsSF%>% filter(source == "Solbrekke"),
             aes(x=lon,y=lat), color = "red")+
  geom_text(data = locsSF %>% filter(source == "Solbrekke"),
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

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#                                                         #
#      Map sampling locations of Soil BON project         #
#                 author: Romy Zeiss                      #
#                   date: 2024-10-30                      #
#           last updated: 2026-03-16                      #
#                                                         #
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# requirements:
# - SVG
# - show sites
# - large-enough point size
# - no colors for protected/unprotected
# - panel showing protected and unprotected site
# - borders of countries (with hint about country border discussions)
#   “The boundaries and names shown on this map do not imply official endorsement or acceptance by the project.”
# - color scheme as in example


# load packages
library(tidyverse)
library(maps) # for world map
library(readxl)
library(leaflet) # create interactive map
library(htmlwidgets) # save interactive map
library(htmltools) # add popups in interactive map

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Load data ####

# world map
world_map <- ggplot2::map_data("world")

# sampling sites
data <- readxl::read_xlsx("../../Sampling_sites/field records/field_records_digitalized.xlsx",
                          sheet = "Sheet1")
data

# registered sites
data_registered <- read_csv("../../Sampling_sites/SoilBON_sites_download_20241218.csv")
data_registered

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Clean data ####

data <- data %>%
  mutate("Longitude" = as.double(ifelse(longitude == "na", NA, longitude)),
         "Latitude" = as.double(ifelse(latitude == "na", NA, latitude)),
         "Conservation area" = ifelse(`conservation area` == "na" |
                                        `conservation area` == "cloudy" |
                                        `conservation area` == "(yes)", 
                                      NA, 
                                      `conservation area`),
         "Conservation area" = ifelse(`Conservation area` == "yes", "Protected site",
                                      ifelse(`Conservation area` == "no", "Unprotected site",
                                             `Conservation area`)))

#data %>% filter(`conservation area` == "cloudy") #HU007
#data %>% filter(is.na(`Conservation area`)) %>% pull(ID) ## in case needed
# "AR032" "CA034" "EC096" "EC097" "EC098" "EC099" "FR001" "FR002" "HU007" "IQ004" "UY001" "UY002" "UY003" "UY004" "UY005" "UY006"
#data %>% filter(`conservation area` == "na") %>% pull(ID) ## in case needed
# "CA034" "EC096" "EC097" "EC098" "EC099" "FR001" "FR002" "IQ004" "UY001" "UY002" "UY003" "UY004" "UY005" "UY006"

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Plot site map ####

ggplot()+
  geom_map(data = world_map, map = world_map, 
           aes(map_id = region),  show.legend = FALSE, 
           fill="white", color = "grey80", linewidth = 0.15)+
  #scale_x_continuous(limits = c(-180, 180), expand = c(0,0))+
  #scale_y_continuous(limits = c(-90, 90), expand = c(0,0))+
  # geom_point(data = data %>% filter(!is.na(`Conservation area`)), ### !! warning: this is only based on the field record, not registered sites
  #            aes(x = Longitude, y = Latitude,
  #                color = `Conservation area`),
  #            alpha = 0.5)+
  # scale_color_manual(values = c("Protected site" = "#018571", "Unprotected site" = "#a6611a"),
  #                    na.value = "black")+
  coord_map(projection = "ortho", orientation = c(10, -35, 23.5))+ # Ortho= make a globe, and orientation is for setting the center of the map
  theme_bw()+
  theme(panel.background = element_rect(fill= "aliceblue"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank())

ggsave(filename = paste0("../_Figures/Mapping/Map_sites_fieldrec_", gsub("-", "", Sys.Date()),".png"),
       last_plot(),
       height = 7, width = 15)


# using leaflet ## interactive map
plot_l <- leaflet() %>%
  addTiles() %>%
  setView(lng = -3.7, lat = 40.4, zoom = 1) %>%
  addCircleMarkers(data = data %>% 
                     rename(lng = Longitude, lat = Latitude) %>%
                     filter(!is.na(lng) & !is.na(lat)) %>%
                     filter(is.na(`Conservation area`)) %>%
                     dplyr::select(lng, lat, ID),
                   color = "black",
                   radius = 5,
                   popup = ~htmlEscape(ID)) %>%
  addCircleMarkers(data = data %>% 
                     rename(lng = Longitude, lat = Latitude) %>%
                     filter(!is.na(lng) & !is.na(lat)) %>%
                     filter(`Conservation area` == "Unprotected site") %>%
                     dplyr::select(lng, lat, ID),
                   color = "#a6611a",
                   radius = 5,
                   popup = ~htmlEscape(ID)) %>% 
  addCircleMarkers(data = data %>% 
                     rename(lng = Longitude, lat = Latitude) %>%
                     filter(!is.na(lng) & !is.na(lat)) %>%
                     filter(`Conservation area` == "Protected site") %>%
                     dplyr::select(lng, lat, ID),
                   color = "#018571",
                   radius = 5,
                   popup = ~htmlEscape(ID)) 
plot_l

saveWidget(plot_l, file = paste0("../_Figures/Mapping/Map_sites_fieldrec_interactive_",
                                 gsub("-", "", Sys.Date()),".html"))


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Plot registered site map ####

ggplot()+
  geom_map(data = world_map, map = world_map, 
           aes(map_id = region),  show.legend = FALSE, 
           fill="white", color = "grey80", linewidth = 0.15)+
  #scale_x_continuous(limits = c(-180, 180), expand = c(0,0))+
  #scale_y_continuous(limits = c(-90, 90), expand = c(0,0))+
  geom_point(data = data_registered, 
             aes(x = longitude, y = latitude,
                 color = protectedArea),
             alpha = 0.5)+
  scale_color_manual(values = c("TRUE" = "#018571", "FALSE" = "#a6611a"),
                     na.value = "black")+
  coord_map(projection = "ortho", orientation = c(10, -35, 23.5))+
  theme_bw()+
  theme(panel.background = element_rect(fill= "aliceblue"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank())

ggsave(filename = paste0("../_Figures/Mapping/Map_sites_registered_", gsub("-", "", Sys.Date()),".png"),
       last_plot(),
       height = 7, width = 15)


# using leaflet
plot_l <- leaflet() %>%
  addTiles() %>%
  setView(lng = -3.7, lat = 40.4, zoom = 1) %>%
  addCircleMarkers(data = data_registered %>% 
                     rename(lng = longitude, lat = latitude) %>%
                     filter(!is.na(lng) & !is.na(lat)) %>%
                     filter(is.na(protectedArea)) %>%
                     dplyr::select(lng, lat, id),
                   color = "black",
                   radius = 5,
                   popup = ~htmlEscape(id)) %>%
  addCircleMarkers(data = data_registered %>% 
                     rename(lng = longitude, lat = latitude) %>%
                     filter(!is.na(lng) & !is.na(lat)) %>%
                     filter(protectedArea == FALSE) %>%
                     dplyr::select(lng, lat, id),
                   color = "#a6611a",
                   radius = 5,
                   popup = ~htmlEscape(id)) %>%
  addCircleMarkers(data = data_registered %>% 
                     rename(lng = longitude, lat = latitude) %>%
                     filter(!is.na(lng) & !is.na(lat)) %>%
                     filter(protectedArea == TRUE) %>%
                     dplyr::select(lng, lat, id),
                   color = "#018571",
                   radius = 5,
                   popup = ~htmlEscape(id)) %>%
  addCircleMarkers(data = data_registered %>% 
                     rename(lng = longitude, lat = latitude) %>%
                     filter(!is.na(lng) & !is.na(lat)) %>%
                     filter(id %in% c(1407, 1408)) %>%
                     dplyr::select(lng, lat, id),
                   color = "violet",
                   radius = 5,
                   popup = ~htmlEscape(id))
plot_l

saveWidget(plot_l, file = paste0("../_Figures/Mapping/Map_sites_registered_interactive_",
                                 gsub("-", "", Sys.Date()),".html"))

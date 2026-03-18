#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#                                                         #
#      Map sampling locations of Soil BON project         #
#                 author: Romy Zeiss                      #
#                   date: 2026-03-16                      #
#           last updated: 2026-03-18                      #
#                                                         #
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# requirements:
# - SVG
# X- show sites
# - large-enough point size
# X- no colors for protected/unprotected
# X- panel showing protected and unprotected site
# X- borders of countries (with hint about country border discussions)
#   “The boundaries and names shown on this map do not imply official endorsement or acceptance by the project.”
# X- color scheme as in example


# load packages
library(tidyverse)
library(maps) # for world map
library(readxl)
library(leaflet) # create interactive map
library(htmlwidgets) # save interactive map
library(htmltools) # add popups in interactive map
#install.packages("ggmagnify", repos = c("https://hughjonesd.r-universe.dev", "https://cloud.r-project.org"))
library(ggmagnify) # to add zooms
library(magick) #to add icon in map
library(svglite) # to save as SVG

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Load data ####

# world map
world_map <- ggplot2::map_data("world")

# sampling sites
data_1 <- readxl::read_xlsx("field_records_digitized_campaign1.xlsx",
                          sheet = "Sheet1")
data_1

data_2 <- readxl::read_xlsx("field_records_digitzed_campaign2.xlsx",
                            sheet = "Sheet1")
data_2

# Soil BON logo
logo_soilbon <- "D:/Bilder/Sonstige Bilder/Logos/Soil_BON.jpg"

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Clean data ####

# rename conservation area column in data_1
data_1 <- data_1 %>% rename(conservation_area = `conservation area`)

f_clean_data <- function(data) { 
  data <- data %>%
    mutate("Longitude" = as.double(ifelse(longitude == "na", NA, longitude)),
           "Latitude" = as.double(ifelse(latitude == "na", NA, latitude)),
           "Conservation area" = ifelse(conservation_area == "na" |
                                          conservation_area == "cloudy" |
                                          conservation_area == "(yes)", 
                                        NA, 
                                        conservation_area),
           "Conservation area" = ifelse(`Conservation area` == "yes", "Protected site",
                                        ifelse(`Conservation area` == "no", "Unprotected site",
                                               `Conservation area`)))
  return(data)
}

clean_1 <- f_clean_data(data_1)
clean_2 <- f_clean_data(data_2)

table(clean_1$`Conservation area`)
table(clean_2$`Conservation area`)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Merge data from both sampling campaigns ####

# function to filter and select relevant rows and columns
f_subset <- function(data) {
  data <- data %>%
    filter(!is.na(Latitude) & !is.na(Longitude)) %>%
    dplyr::select(Latitude, Longitude, `Conservation area`) %>%
    unique()
  return(data)
}

# subset both datasets
subset_1 <- f_subset(clean_1) %>% 
  mutate("Campaign_1" = 1) #498 of 523
subset_2 <- f_subset(clean_2) %>% 
  mutate("Campaign_2" = 1) #227 of 237

# merge datasets
data <- subset_1 %>%
  full_join(subset_2)
data #674 (instead of 227+498 = 725 unique sites)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Fix parameters ####

# number of sampling sites
n_sites <- nrow(data); n_sites

# points to zoom in: Unprotected site, Protected site
data <- data %>%
  mutate("Sample_point" = ifelse(round(Longitude) == -48 &    #-48
                                   round(Latitude) == -16, 1, #-16
                                        NA))
sample_points <- data %>% 
  filter(Sample_point==1) %>%
  dplyr::select(Latitude, Longitude, `Conservation area`)
sample_points


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Plot site map ####

p <- ggplot()+
  geom_map(data = world_map, map = world_map, 
           aes(map_id = region),  show.legend = FALSE, 
           fill="white", color = "grey80", linewidth = 0.15)+
  scale_x_continuous(limits = c(-180, 180), expand = c(0,0))+
  scale_y_continuous(limits = c(-90, 90), expand = c(0,0))+
  #geom_point(aes(x = sample_points[,1], y = sample_points[,2]))+
  geom_point(data = data,
             aes(x = Longitude, y = Latitude),
             alpha = 1, color = "#018571")+
  geom_point(data = data %>% filter(!is.na(`Conservation area`)) %>%
               filter(Sample_point==1), 
             aes(x = Longitude, y = Latitude,
                 color = `Conservation area`),
             alpha = 1)+
  scale_color_manual(values = c("Protected site" = "#019900", "Unprotected site" = "#a6611a"),
                     na.value = "black")+
  geom_text(aes(x = -35, y = 22, #x = -15, y = -18 for below
                label = Sys.Date()),
            hjust = 0.5, vjust = 0.5)+
  geom_text(aes(x = -4, y = -33, #x = -15, y = -18 for below
                label = paste0("n = ", n_sites, " sites")),
            hjust = 0.5, vjust = 0.5)+
  ggimage::geom_image(
    aes(x = -40, y = 30, image = logo_soilbon),
    size = 0.3,
    hjust = 0.5)+
  ggmagnify::geom_magnify(
    aes(from = list(c(xmin = sample_points %>% pull(Longitude) %>% min() -1,
                      xmax = sample_points %>% pull(Longitude) %>% max() +1,
                 ymin = sample_points %>% pull(Latitude) %>% min() -2,
                 ymax = sample_points %>% pull(Latitude) %>% max() +2)), #(xmin, xmax, ymin, ymax)
        to = list(c(-17, -12,
                  -25, -10))),
    expand = 1,
    scale.inset = 0.02,
    colour = "grey80",
    proj.linetype = 0, 
    target.linetype = "dotted",
    inset.linetype = "dotted")+
  geom_text(aes(x = -18, y = -17, #x = -15, y = -18 for below
                label = paste0("                      Protected\n\n\nUnprotected")),
            hjust = 0.5, vjust = 0.5,
            size = 2)+
  labs(caption = "The boundaries shown on this map do not imply official endorsement or acceptance by the project.")+
  coord_map(projection = "ortho", orientation = c(10, -35, 23.5))+ # Ortho= make a globe, and orientation is for setting the center of the map
  theme_bw()+
  theme(panel.background = element_rect(fill= "aliceblue"),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.caption = element_text(size = 7),
        legend.title = element_blank()); p

ggsave(filename = paste0("Map_sites_globe_", gsub("-", "", Sys.Date()),".png"),
       p,
       height = 5.65, width = 5.65,
       dpi = 600)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Splayed globe ####

# points to zoom in: Unprotected site, Protected site
data <- data %>%
  mutate("Sample_point" = ifelse(round(Longitude) == -91 &    #-48, -70, -91
                                   round(Latitude) == 15, 2, #-16, -21, -4, 15
                                 NA))
sample_points_2 <- data %>% 
  filter(Sample_point==2) %>%
  dplyr::select(Latitude, Longitude, `Conservation area`)
sample_points_2

splay <- ggplot()+
  geom_map(data = world_map, map = world_map, 
           aes(map_id = region),  show.legend = FALSE, 
           fill="white", color = "grey80", linewidth = 0.15)+
  scale_x_continuous(limits = c(-180, 180), expand = c(0,0))+
  scale_y_continuous(limits = c(-90, 90), expand = c(0,0))+
  geom_point(data = data, 
             aes(x = Longitude, y = Latitude),
             alpha = 1, color = "#018571")+
  geom_point(data = data %>% filter(Sample_point == 2), 
             aes(x = Longitude, y = Latitude),
             color = "black", shape = 0, size = 5)+
  coord_map(projection = "mercator")+ # Ortho= make a globe, and orientation is for setting the center of the map
  labs(caption = "The boundaries shown on this map do not imply official endorsement or acceptance by the project.")+
  theme_bw()+
  theme(panel.background = element_rect(fill= "aliceblue"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank()); splay

zoom <- ggplot()+
  scale_x_continuous(limits = c(sample_points_2 %>% pull(Longitude) %>% min()-0.05, 
                                sample_points_2 %>% pull(Longitude) %>% max()+0.05), expand = c(0,0))+
  scale_y_continuous(limits = c(sample_points_2 %>% pull(Latitude) %>% min()-0.05,
                                sample_points_2 %>% pull(Latitude) %>% max()+0.05), expand = c(0,0))+
  geom_point(data = data %>% filter(Sample_point == 2), 
             aes(x = Longitude, y = Latitude,
                 color = `Conservation area`),
             alpha = 1, size = 5)+
  scale_color_manual(values = c("Protected site" = "#019900", "Unprotected site" = "#a6611a"),
                     na.value = "black")+
  geom_text(data = data %>% filter(Sample_point == 2), 
            aes(x = Longitude, y = Latitude,
                label = `Conservation area`), 
            vjust = -2, size = 2)+
  coord_map(projection = "mercator")+ # Ortho= make a globe, and orientation is for setting the center of the map
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"); zoom

splay + patchwork::inset_element(zoom, left = 0.02,
                                bottom = 0.15,
                                right = 0.23,
                                top = 0.45)


ggsave(filename = paste0("Map_sites_splayed_", gsub("-", "", Sys.Date()),".svg"),
       last_plot(),
       dpi = 600)


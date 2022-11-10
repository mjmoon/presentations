rm(list = ls())
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world_map <- ne_countries(scale = 50, returnclass = "sf", type = "map_units")
europe <- c("Belgium", "Germany", "United Kingdom", "Spain", 
            "France", "Ireland", "Netherlands", "Portugal",
            "Switzerland")
leagues <- c("Spain", "France", "England", "Germany")

europe_map <- world_map |>
  filter(name %in% leagues) |>
  mutate(in_data = name %in% leagues )
bbox <- st_bbox(c(xmin = -10, ymin = 10, xmax = 20, ymax = 80),
                crs = st_crs(europe_map))
europe_map_cropped <- st_crop(europe_map, bbox)

ggplot(europe_map_cropped) +
  theme_void() +
  geom_sf(aes(fill = name), colour = NA, show.legend = FALSE,
          data = europe_map_cropped |> filter(name %in% leagues)) +
  geom_sf(fill = NA) +
  scale_fill_brewer(palette = "Pastel2")
ggsave("img/countries.png", device = "png", width = 4, height = 4,
       bg = NULL)

ggplot(europe_map_cropped) +
  theme_void() +
  geom_sf(fill = "grey80", colour = NA, show.legend = FALSE,
          data = europe_map_cropped |> filter(name %in% leagues)) +
  geom_sf(fill = NA)
ggsave("img/countries-bw.png", device = "png", width = 4, height = 4,
       bg = NULL)


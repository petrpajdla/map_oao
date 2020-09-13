# prepare shapefile for oao from projekty


# libs --------------------------------------------------------------------

library(tidyverse)
library(sf)
library(here)


# data --------------------------------------------------------------------

proj <- read_delim(here("data/raw", "export_2020-08-06_projekt.csv"), 
                   delim = "#", 
                   col_types = paste0(rep("c", 48), collapse = ""), 
                   quote = "")


# data prep ---------------------------------------------------------------

proj_clean <- proj %>% 
  select(ident = ident_cely, 
         oao = organizace_prihlaseni, 
         x = geometry_e, 
         y = geometry_n,
         stav = stav_popis, 
         typ = typ_projektu) %>% 
  filter(!is.na(oao), !is.na(x), !is.na(y))


# sf ----------------------------------------------------------------------

proj_sf <- st_as_sf(proj_clean, coords = c("x", "y")) %>% 
  st_set_crs(4326)

st_write(proj_sf, here("data", "oao_points"), 
         layer = "oao_proj", 
         driver = "ESRI Shapefile", 
         layer_options = "ENCODING=UTF-8",
         delete_dsn = FALSE, delete_layer = TRUE)


# XXX ---------------------------------------------------------------------

oao_sf <- st_read(dsn = "data/oao_scope", layer = "oao_scope")

oao_sf$oao[!oao_sf$oao %in% proj_sf$oao]
proj_sf$oao[!proj_sf$oao %in% oao_sf$oao]

proj_sf_filt <- proj_sf %>% filter(oao %in% oao_sf$oao)

kraje <- RCzechia::kraje(resolution = "low")

ggplot(oao_sf) +
  geom_sf(data = kraje, color = "white") +
  geom_sf(aes(fill = oao), show.legend = FALSE) +
  geom_sf(data = proj_sf_filt, shape = 4, show.legend = FALSE, alpha = 0.4) +
  theme_void() +
  facet_wrap(~oao, ncol = 5)

ggsave(here::here("plots", "oao_points.png"), width = 15, height = 49)


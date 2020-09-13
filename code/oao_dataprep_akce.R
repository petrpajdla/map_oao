# prepare shapefile for oao from akce


# libs --------------------------------------------------------------------

library(tidyverse)
library(sf)
library(here)


# data --------------------------------------------------------------------

akce <- read_csv(here("data/processed", "pian_akce.csv"))


# data prep ---------------------------------------------------------------

akce_cl <- akce %>% 
  select(ident = ident_cely,
         oao = organizace,
         typ = hlavni_typ,
         pristup = pristupnost,
         negativni = negativni_jednotka,
         dj,
         pian,
         x = centroid_e,
         y = centroid_n)


# sf ----------------------------------------------------------------------

akce_sf <- akce_cl %>% st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(4326)

st_write(akce_sf, here("data", "oao_points"), 
         layer = "oao_akce", 
         driver = "ESRI Shapefile", 
         layer_options = "ENCODING=UTF-8",
         delete_dsn = FALSE, delete_layer = TRUE)


akce_sf %>% ggplot() + geom_sf() + facet_wrap(~negativni)

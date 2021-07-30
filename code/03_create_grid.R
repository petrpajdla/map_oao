# create grid with intensity of actions of each oao

library(tidyverse)
library(sf)
library(googledrive)
library(googlesheets4)


# data  -------------------------------------------------------------------

# grid
grid <- RCzechia::KFME_grid("high")

# akce
akce <- read_csv(here::here("data/processed", "pian_akce.csv"))

# heslar
heslar <- read_csv(here::here("data/raw", "heslar_organizace.csv"))

# GD data
updated_gd_url <- "https://docs.google.com/spreadsheets/d/1RXXRGpgkrgtBhF9taEtCVuHxVIJcbeATF9RBxBtORJY/edit?usp=sharing"
gd_updated <- drive_get(updated_gd_url)

# oao s platnou dohodou
oao_platne <- read_sheet(gd_updated, sheet = "Dohody_pracovni") %>% 
  filter(dohoda_platna == 1) %>% 
  pull(nazev_zkraceny)

# data prep ---------------------------------------------------------------

# change MU <fakulta> to generic one MU
# change NPU <uop> to generic NPU
akce_clean <- akce %>% 
  select(ident = ident_cely,
         nazev_zkraceny = organizace,
         # typ = hlavni_typ,
         # pristup = pristupnost,
         # negativni = negativni_jednotka,
         # datum = datum_ukonceni_v,
         # dj,
         # pian,
         x = centroid_e,
         y = centroid_n) %>% 
  mutate(nazev_zkraceny = if_else(str_detect(nazev_zkraceny, "MU Brno"), 
                                  "MU Brno", nazev_zkraceny),
         nazev_zkraceny = if_else(str_detect(nazev_zkraceny, "NPÚ"), 
                                  "NPÚ generální ředitelství", nazev_zkraceny),
         nazev_zkraceny = if_else(str_detect(nazev_zkraceny, "Archeologický ústav Brno"), 
                                  "Archeologický ústav Brno", nazev_zkraceny))

# check if all oao present
akce_clean$nazev_zkraceny %in% heslar$nazev_zkraceny %>% all()
oao_platne[!oao_platne %in% akce_clean$nazev_zkraceny]
akce_clean$nazev_zkraceny[!akce_clean$nazev_zkraceny %in% oao_platne] %>% 
  factor() %>% levels()

# filter only platne OAO dle GD seznamu
akce_clean <- akce_clean %>% filter(nazev_zkraceny %in% oao_platne) 

# sf ----------------------------------------------------------------------

akce_sf <- akce_clean %>% st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(4326)

akce_nest <- akce_sf %>% 
  group_by(nazev_zkraceny) %>% 
  nest()

# create grid -------------------------------------------------------------

contained_in_grid <- function(x, grid) {
  contains <- st_contains(grid, x)
  n <- contains[contains %>% lengths() > 0] %>% 
    lapply(length) %>% 
    lapply(as_tibble) %>% 
    bind_rows()
  grid %>% filter(contains %>% lengths() > 0) %>% 
    bind_cols(n) %>% 
    mutate(scaled = log10(value + 1))
}

grids <- akce_nest %>% 
  mutate(grid = map(data, contained_in_grid, grid)) %>% 
  select(-data) %>% 
  unnest(grid) %>% 
  st_as_sf()


# export grid -------------------------------------------------------------

st_write(grids, here::here("data/final", "oao_grid.geojson"))

file.copy(here::here("data/final/oao_grid.geojson"),
          here::here("app/oao_grid.geojson"), overwrite = TRUE)


# playground --------------------------------------------------------------

x <- grids %>% 
  filter(nazev_zkraceny == "ÚAPP Brno")

x %>% 
  ggplot() +
  geom_sf(data = RCzechia::republika(), fill = "white") +
  geom_sf(aes(fill = scaled), alpha = 0.8, color = NA) +
  geom_sf_text(aes(label = value)) +
  scale_fill_gradient(low = "#E5F5E0", high = "#31A354") +
  theme_void()

# leaflet map
pal <- leaflet::colorNumeric(palette = "YlGnBu", domain = x$scaled)
# YlGn YlGnBu Greens Blues

x %>% 
  leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::addProviderTiles(provider = leaflet::providers$CartoDB.Positron) %>% 
  leaflet::addPolygons(stroke = F, smoothFactor = 0.2,
                       color = ~pal(scaled), fillOpacity = 0.6)

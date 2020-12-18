library(tidyverse)
library(leaflet)
library(sf)

# oao spatial scope -------------------------------------------------------

oao_scope_krovak <- st_read(here::here("data", "oao_scope"), 
                            layer = "oao_scope_krovak")

# simplify to reduce size
oao_scope_simple <- st_simplify(oao_scope_krovak) %>% 
  st_transform(4326) %>% 
  arrange(oao)

# output into app directory
st_write(oao_scope_simple, here::here("app", "oao_scope.geojson"), 
         delete_dsn = TRUE)


# oao points (akce) -------------------------------------------------------

oao_points <- st_read(here::here("data", "oao_points"),
                      layer = "oao_akce") %>% 
  select(oao, typ, pristup, negativni, datum) %>% 
  filter(oao %in% oao_scope_simple$oao,
         datum >= "2017-06-01") %>% 
  st_transform(5514) %>% 
  st_simplify() %>% 
  st_transform(4326)

st_write(oao_points, here::here("app", "oao_points.geojson"), delete_dsn = TRUE)


# oao names ---------------------------------------------------------------

na_to_dash <- function(x) {
  if_else(is.na(x), "–", x)
}

prevodnik <- read_csv(here::here("data", "oao_names_prevodnik.csv"))

mk <- read_csv(here::here("data/raw", "seznam_oao_mkcr.csv"))
arup <- read_csv(here::here("data", "oao_arup_processed.csv"))

oao_names <- full_join(prevodnik, mk, 
          by = c("name_mkcr" = "Oprávněný subjekt   (www stránky)")) %>% 
  full_join(arup, by = c("name_arup" = "oao")) %>% 
  select(id = id.x, 
         name = name_mkcr, 
         oao = abbrv_aiscr, 
         address, 
         avcr_note = loc,
         avcr_date = dohoda_avcr,
         mkcr_note = "Povolení MK",
         mkcr_date = "Datum rozhodnutí MK") %>% 
  filter(!is.na(oao)) %>% 
  mutate(address = str_replace(address, ",", "<br>"),
         across(where(is.character), na_to_dash)) %>% 
  arrange(oao)

write_csv(oao_names, here::here("app", "oao_names.csv"))

# leaflet map -------------------------------------------------------------

filter(oao_scope_simple, oao == "ÚAPP Brno") %>% 
  leaflet(options = leafletOptions(minZoom = 7, maxZoom = 14)) %>% 
  addTiles() %>% 
  addPolygons() %>% 
  setView(zoom = 8, lng = 15.4730, lat = 49.8175) %>% 
  setMaxBounds(11, 48, 20, 52)

filter(oao_scope_simple, oao == "ÚAPP Brno") %>% 
  leaflet(options = leafletOptions(minZoom = 7, maxZoom = 14)) %>% 
  addTiles() %>% 
  addPolygons(fill = NA) %>% 
  addCircleMarkers(data = filter(oao_points, oao == "ÚAPP Brno"), stroke =  NA) %>% 
  setView(zoom = 8, lng = 15.4730, lat = 49.8175) %>% 
  setMaxBounds(11, 48, 20, 52)





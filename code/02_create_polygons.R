# This script reads updated data from GD and creates polygons for oao

library(tidyverse)
library(googledrive)
library(googlesheets4)


# funs --------------------------------------------------------------------

separate2longer <- function(x, col, ncol) {
  x %>% separate(sym(col), into = str_c("c", seq(1, ncol, 1)), sep = ";") %>% 
    pivot_longer(starts_with("c"), values_drop_na = TRUE) %>% 
    select(-name) %>% 
    mutate(value = str_trim(value, side = "both"))
}

add_polygon <- function(x) {
  x %>% select(ico, geometry) %>% 
    group_by(ico) %>% 
    nest() %>% 
    mutate(data = map(data, sf::st_as_sf),
           data = map(data, sf::st_union)) %>% 
    unnest(data)
}

# data --------------------------------------------------------------------

revised_gd_url <- "https://docs.google.com/spreadsheets/d/1knxDiUuCVqwgzgQkodhGg0vMe6w6LsKiGVqrsvi5dpw/edit#gid=0"
gd_revised <- drive_get(revised_gd_url)

# updated_gd_url <- "https://docs.google.com/spreadsheets/d/1RXXRGpgkrgtBhF9taEtCVuHxVIJcbeATF9RBxBtORJY/edit?usp=sharing"
# gd_updated <- drive_get(updated_gd_url)

# bg data
kraje <- RCzechia::kraje()
okresy <- RCzechia::okresy()
katastry <- sf::read_sf(here::here("data/raw", "katastry_shp"))

# dataprep ----------------------------------------------------------------

# # oao s platnou dohodou
# oao_platne <- read_sheet(gd_updated, sheet = "Dohody_pracovni") %>% 
#   filter(dohoda_platna == 1) %>% 
#   pull(nazev_zkraceny)

# # correct names
# heslar <- read_csv(here::here("data/raw", "heslar_organizace.csv"))
# # NOTE: Manually added NZM Praha to heslar + MU Brno
# oao_platne[!oao_platne %in% heslar$nazev_zkraceny]
# # heslar %>% filter(str_detect(nazev_zkraceny, "Národní"))

# uzemni pusobnost --------------------------------------------------------

oao_uzemi <- read_sheet(gd_revised, sheet = "oao_webapp") %>% 
  select(ico, nazev_zkraceny, starts_with("is"), kraj, okres, katastr)

# oao_uzemi <- read_sheet(gd_updated, sheet = "Uzemi_pracovni") %>% 
#   filter(nazev_zkraceny %in% oao_platne) %>% 
#   select(-c_m, -uzemi, -is_praha)

# republika
oao_republika <- oao_uzemi %>% 
  filter(is_rep) %>% 
  select(ico) %>% 
  bind_cols(RCzechia::republika()) %>% 
  select(ico, data = geometry) %>% 
  sf::st_as_sf()

# kraje
oao_kraje <- oao_uzemi %>% 
  filter(is_kraj) %>% 
  select(ico, kraj) %>% 
  separate2longer("kraj", 10) %>% 
  left_join(kraje, by = c("value" = "NAZ_CZNUTS3")) %>% 
  add_polygon() %>% 
  sf::st_as_sf()

# okresy
oao_okresy <- oao_uzemi %>% 
  filter(is_okres) %>% 
  select(ico, okres) %>% 
  separate2longer("okres", 30) %>% 
  left_join(okresy, by = c("value" = "NAZ_LAU1")) %>% 
  add_polygon() %>% 
  sf::st_as_sf()

# katastry
oao_katastry <- oao_uzemi %>% 
  filter(is_katastr) %>% 
  select(ico, katastr) %>% 
  separate2longer("katastr", 200) %>% 
  left_join(katastry, by = c("value" = "NAZ_KU")) %>% 
  add_polygon() %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(4326)

# result
oao_uzemi_poly <- oao_republika %>% 
  bind_rows(oao_kraje) %>% 
  bind_rows(oao_okresy) %>% 
  bind_rows(oao_katastry) %>% 
  sf::st_make_valid()

# valid geometry?
sf::st_is_valid(oao_uzemi_poly) %>% all()

# missing polygons
all(oao_uzemi$ico %in% oao_uzemi_poly$ico)


# total covered area (arranging in tables) --------------------------------

oao_uzemi_poly <- oao_uzemi_poly %>% 
  mutate(area = as.numeric(sf::st_area(oao_uzemi_poly)))

# export resulting polygons -----------------------------------------------

sf::st_write(oao_uzemi_poly, 
             here::here("data/final", "oao_territory_poly.geojson"))


# simplify polygons for the web use ---------------------------------------

oao_uzemi_poly_simple <- sf::st_simplify(oao_uzemi_poly, dTolerance = 150)

sf::st_write(oao_uzemi_poly_simple, 
             here::here("data/final", "oao_territory_poly_simple.geojson"))


# update file for app -----------------------------------------------------

file.copy(here::here("data/final/oao_territory_poly_simple.geojson"),
          here::here("app/data/oao_scope.geojson"), overwrite = TRUE)



# playground --------------------------------------------------------------

# oao_uzemi_poly_simple <- sf::st_read(here::here("data/final", "oao_territory_poly_simple.geojson"))

# oao_katastry %>%
#   ggplot() +
#     geom_sf() +
#     facet_wrap(~ico)

# oao_uzemi_poly_simple %>% 
#   filter(nazev_zkraceny == "MU Brno") %>% 
#   leaflet::leaflet() %>% 
#   leaflet::addTiles() %>% 
#   leaflet::addPolygons()
# 
# oao_uzemi_poly %>% 
#   filter(nazev_zkraceny == "Muzeum Vrchlabí") %>% 
#   leaflet::leaflet() %>% 
#   leaflet::addTiles() %>% 
#   leaflet::addPolygons()

# # Searches through katastr names and plots their position.
# katastry[str_detect(katastry$NAZ_KU, "Střítež"), ] %>%
#   ggplot() +
#   geom_sf(data = RCzechia::republika()) +
#   geom_sf() +
#   geom_sf_text(aes(label = NAZ_KU)) +
#   theme_void()
# 
# oao_republika %>% 
#   sf::st_as_sf() %>% 
#   ggplot() +
#   geom_sf() +
#   facet_wrap(~nazev_zkraceny)

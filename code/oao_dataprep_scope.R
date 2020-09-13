# create shapefiles of oao scopes

# libs --------------------------------------------------------------------

library(tidyverse)
library(sf)
library(rvest)
library(RCzechia)


# # scraping data from web --------------------------------------------------
# 
# url <- "https://www.arup.cas.cz/kdo-je-opravnen-provadet-archeologicke-vyzkumy/"
# xpath_oao <- "/html/body/main/section/div/div[2]/div/div[3]/div/div/div/table"
# 
# oao_data <- url %>% 
#   read_html() %>% 
#   html_nodes(xpath = xpath_oao) %>% 
#   html_table(fill = TRUE)
# 
# 
# # cleaning data -----------------------------------------------------------
# 
# oao_in <- oao_data[[1]] %>% 
#   tibble()
# 
# # save scraped data for later use...
# # write_csv(oao_in, here::here("data", "oao_arup_raw.csv"))
# 
# colnames(oao_in) <- c("oao", "id", "address", "loc")
# oao_in <- oao_in[-1, ]
# oao_in[(oao_in$id == "Teplice"), "id"] <- "370941"
# oao_in[str_detect(oao_in$oao, "Břeclav"), "id"] <- "60680920"
# 
# 
# # oao abbreviated names ---------------------------------------------------
# heslar_oao <- read_csv2(here::here("data", "heslar_organizace.csv"))
# 
# # oao_names <- oao_in %>% select(oao, id)
# # write_csv(oao_names, here::here("data", "oao_names.csv"))
# oao_names <- read_csv(here::here("data", "oao_names.csv")) %>% 
#   mutate(abbr = str_remove(abbr, "\\s-\\sF.*$"),
#          abbr = str_remove(abbr, "\\sgener.*"))
# # oao_names[!oao_names$abbr %in% heslar_oao$nazev_zkraceny, ]
# 
# oao_in <- left_join(oao_in, oao_names, by = c("oao", "id"))


# i/o processed data ------------------------------------------------------

# write_csv(oao_in, here::here("data", "oao_arup_processed.csv"))
oao_in <- read_csv(here::here("data", "oao_arup_processed.csv"))

oao_names <- oao_in %>% select(id, oao = abbr)

# cleaning regions --------------------------------------------------------

oao_loc <- oao_in %>% mutate(kraj = str_detect(loc, "kraj"),
                             okres = str_detect(loc, "okres"),
                             praha = str_detect(loc, "Prah(a|y)"),
                             czrep = str_detect(loc, "republika"))

index <- oao_loc %>% 
  select(kraj, okres, praha, czrep) %>% 
  rowSums() %>% factor()

# republika
oao_czrep <- bind_cols(oao_loc, index = index) %>% 
  filter(czrep, index == 1) %>% 
  select(id, loc) %>% 
  bind_rows(tibble(id = "60162694", loc = "Česká republika"))

# clean kraje
# bind_cols(oao_loc, index = index) %>% 
#   filter(kraj) %>%
#   select(id, loc) %>% 
#   mutate(loc = str_remove(loc, "kraj")) %>% 
#   separate(loc, sep = ",", into = paste("x", 1:10), fill = "right") %>%
#   pivot_longer(-id) %>%
#   filter(!is.na(value)) %>% 
#   select(-name) %>% 
#   mutate(value = str_trim(value, side = "both")) %>% 
#   write_csv(here::here("data", "kraje.csv"))

oao_kraje <- read_csv(here::here("data", "kraje.csv")) %>% 
  bind_rows(tribble(~id, ~value,
                    "94862",	"Zlínský",
                    "94862",	"Jihomoravský",
                    "94862",	"Olomoucký",
                    "89982",	"Zlínský",
                    "90735",	"Vysočina"))

# clean okresy
# bind_cols(oao_loc, index = index) %>%
#   filter(okres) %>%
#   select(id, loc) %>%
#   mutate(loc = str_remove(loc, "okres")) %>%
#   separate(loc, sep = ",", into = paste("x", 1:40), fill = "right") %>%
#   pivot_longer(-id) %>%
#   filter(!is.na(value)) %>%
#   select(-name) %>%
#   mutate(value = str_trim(value, side = "both")) %>%
#   write_csv(here::here("data", "okresy2.csv"))

oao_okresy <- read_csv(here::here("data", "okresy.csv")) %>% 
  mutate(id = as.character(id)) %>% 
  bind_rows(tribble(~id, ~value,
                    "175722",	"Nymburk",
                    "175722",	"Praha-východ",
                    "85804",	"Semily",
                    "101427",	"Brno-město",
                    "60680920", "Břeclav",
                    "66567", "Mělník",
                    "305847", "Karviná",
                    "305847", "Frýdek-Místek",
                    "97594", "Ostrava-město",
                    "374652", "Písek"))

# clean katastry aj.
# Praha
oao_praha <- bind_cols(oao_loc, index = index) %>%
  filter(praha, index == 1) %>%
  select(id, loc) %>% 
  filter(id != 69850) %>% 
  mutate(loc = "Hlavní město Praha")

# ostatní
# bind_cols(oao_loc, index = index) %>%
#   filter(!id %in% oao_czrep$id,
#          !id %in% oao_kraje$id,
#          !id %in% oao_okresy$id,
#          !id %in% oao_praha$id) %>%
#   select(id, loc) %>%
#   mutate(loc = str_remove(loc, "území|k\\.ú|kat\\.úz\\.")) %>%
#   separate(loc, sep = ",", into = paste("x", 1:140), fill = "right") %>% 
#   pivot_longer(-id) %>%
#   filter(!is.na(value)) %>%
#   select(-name) %>%
#   mutate(value = str_trim(value, side = "both")) %>% 
#   write_csv(here::here("data", "other.csv"))

# oao_other <- read_csv(here::here("data", "other.csv")) %>% 
#   mutate(id = as.character(id))


# spatial data ------------------------------------------------------------
# read spatial data (package RCzechia)  
kraje <- kraje(resolution = "low") %>% mutate(nazev = str_remove(NAZ_CZNUTS3, "\\skraj|Kraj\\s"))
okresy <- okresy(resolution = "low")
czrep <- republika(resolution = "low")
# katastry <- st_read(here::here("data", "katastry_shp"), "Katastr")


# check if all names fit --------------------------------------------------

oao_czrep$loc %in% czrep$NAZ_STAT %>% all()
oao_kraje$value %in% kraje$nazev %>% all()
oao_okresy$value %in% okresy$NAZ_LAU1 %>% all()
oao_praha$loc %in% kraje$NAZ_CZNUTS3 %>% all()


# simple features ---------------------------------------------------------

oao_sf_czrep <- right_join(czrep, oao_czrep, by = c("NAZ_STAT" = "loc")) %>% 
  union_sf("id") %>%
  left_join(oao_names, by = "id")

oao_sf_kraje <- right_join(kraje, oao_kraje, by = c("nazev" = "value")) %>% 
  select(-nazev) %>% 
  union_sf("id") %>%
  left_join(oao_names, by = "id")

oao_sf_okresy <- right_join(okresy, oao_okresy, by = c("NAZ_LAU1" = "value")) %>% 
  union_sf("id") %>%
  left_join(oao_names, by = "id")

oao_sf_praha <- right_join(kraje, oao_praha, by = c("nazev" = "loc")) %>% 
  select(-nazev) %>% 
  union_sf("id") %>%
  left_join(oao_names, by = "id")

# oao_sf_other <- left_join(katastry, oao_other, by = c("NAZ_KU" = "value")) %>% 
#   union_sf("id") %>%
#   left_join(oao_names, by = "id")


# plotting ----------------------------------------------------------------

# oao_sf_czrep %>% 
#   ggplot() +
#   geom_sf(data = kraje, color = "white") +
#   geom_sf(aes(fill = oao), alpha = 0.4, show.legend = FALSE) +
#   facet_wrap(vars(oao), ncol = 4) +
#   theme_void()
# 
# oao_sf_kraje %>% 
#   mutate(area = as.numeric(st_area(oao_sf_kraje)),
#          oao = fct_reorder(oao, area, .desc = TRUE)) %>% 
#   ggplot() +
#   geom_sf(data = kraje, color = "white") +
#   geom_sf(aes(fill = oao), alpha = 0.4, show.legend = FALSE) +
#   facet_wrap(vars(oao), ncol = 4) +
#   theme_void()
# 
# oao_sf_okresy %>% 
#   mutate(area = as.numeric(st_area(oao_sf_okresy)),
#          oao = fct_reorder(oao, area, .desc = TRUE)) %>% 
#   ggplot() +
#   geom_sf(data = kraje, color = "white") +
#   geom_sf(aes(fill = oao), alpha = 0.4, show.legend = FALSE) +
#   facet_wrap(vars(oao)) +
#   theme_void()
# 
# oao_sf_praha %>% 
#   filter(!is.na(oao)) %>% 
#   ggplot() +
#   # geom_sf(data = kraje, color = "white") +
#   geom_sf(aes(fill = oao), alpha = 0.4, show.legend = FALSE) +
#   facet_wrap(vars(oao)) +
#   theme_void()

# oao_sf_other %>% 
#   filter(!is.na(oao)) %>% 
#   ggplot() +
#   geom_sf(data = kraje, color = "white") +
#   geom_sf(aes(fill = oao), alpha = 0.4, show.legend = FALSE) +
#   facet_wrap(vars(oao)) +
#   theme_void()


# combined shapefile ------------------------------------------------------

oao_sf <- bind_rows(oao_sf_czrep, oao_sf_kraje, oao_sf_okresy, oao_sf_praha)


# i/o shapefile -----------------------------------------------------------

# dir.create(here::here("data", "oao_scope"))
st_write(oao_sf, here::here("data", "oao_scope"), 
         layer = "oao_scope", 
         driver = "ESRI Shapefile", 
         layer_options = "ENCODING=UTF-8",
         delete_dsn = FALSE, delete_layer = TRUE)


# krovak ----------------------------------------------------------------

oao_sf_krovak <- st_transform(oao_sf, 5514) %>% 
  st_simplify(dTolerance = 2)

# object.size(oao_sf_krovak)
st_write(oao_sf_krovak, here::here("data", "oao_scope"), 
         layer = "oao_scope_krovak", 
         driver = "ESRI Shapefile", 
         layer_options = "ENCODING=UTF-8",
         delete_dsn = FALSE, delete_layer = TRUE)



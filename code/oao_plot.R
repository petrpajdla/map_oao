# plot oao (geom from proj and akce) + oao scope


# libs --------------------------------------------------------------------

library(tidyverse)
library(sf)
library(here)

# theme not to "cut" lower/upper parts of letters
theme_add <- theme(strip.text.x = element_text(margin = margin(b = 5, t = 5)))


# spatia data -------------------------------------------------------------

# spatial scope
oao_scope <- st_read(here("data", "oao_scope"), 
                     layer = "oao_scope") %>% 
  filter(oao != "Zemědělské muzeum")
# oao_sf_krovak <- st_read(here::here("data", "oao_scope"), 
#                          layer = "oao_scope_krovak")

# akce
oao_akce <- st_read(here("data", "oao_points"),
                    layer = "oao_akce") %>% 
  filter(!is.na(oao)) %>% 
  rename(nazev_zkraceny = oao)

# projekty
oao_proj <- st_read(here("data", "oao_points"),
                    layer = "oao_proj") %>% 
  rename(nazev_zkraceny = oao)


# read background data (package RCzechia) 
czrep <- RCzechia::republika(resolution = "low")
kraje <- RCzechia::kraje(resolution = "low")

# heslar organizaci
# heslar <- read_csv2(here("data", "heslar_organizace.csv"))

# data prep ---------------------------------------------------------------
# edit names in oao scope
nazev_zkraceny_tab <- tribble(~oao, ~nazev_zkraceny,
                              "MU Brno", "MU Brno - Filozofická fakulta",
                              "MU Brno", "MU Brno - Pedagogická fakulta",
                              "MU Brno", "MU Brno - Přírodovědecká fakulta",
                              "UK Praha", "UK Praha - Filozofická fakulta",
                              "UK Praha", "UK Praha - Přírodověcká fakulta",
                              "ZČU Plzeň", "ZČU Plzeň - Filozofická fakulta",
                              "JČU České Budějovice", "JČU České Budějovice - Filozofická fakulta",
                              "UP Olomouc", "UP Olomouc - Filozofická fakulta",
                              "UHK", "UHK - Filozofická fakulta",
                              "SU Opava", "SU Opava - Filozoficko-přírodovědecká fakulta",
                              "NPÚ", "NPÚ generální ředitelství",
                              "NPÚ", "NPÚ u.o.p. střední Čechy",
                              "NPÚ", "NPÚ ú.o.p. České Budějovice",
                              "NPÚ", "NPÚ ú.o.p. Praha",
                              "NPÚ", "NPÚ ú.o.p. Liberec",
                              "NPÚ", "NPÚ ú.o.p. Pardubice",
                              "NPÚ", "NPÚ ú.o.p. Plzeň",
                              "NPÚ", "NPÚ ú.o.p. Telč",
                              "NPÚ", "NPÚ ú.o.p. Ústí nad Labem",
                              "NPÚ", "NPÚ ú.o.p. Josefov",
                              "NPÚ", "NPÚ ú.o.p. Loket",
                              "NPÚ", "NPÚ ú.o.p. Brno",
                              "NPÚ", "NPÚ ú.o.p. Kroměříž",
                              "NPÚ", "NPÚ ú.o.p. Olomouc",
                              "NPÚ", "NPÚ ú.o.p. Ostrava",
                              "Archeologický ústav Brno", "Archeologický ústav Brno",
                              "Archeologický ústav Brno", "Archeologický ústav Brno - Dolní Dunajovice",
                              "Archeologický ústav Brno", "Archeologický ústav Brno - Dolní Věstonice",
                              "Archeologický ústav Brno", "Archeologický ústav Brno - Mikulčice",
                              "Archeologický ústav Brno", "Archeologický ústav Brno - Opava")

# oao_scope_cl <- left_join(oao_scope, nazev_zkraceny_tab, 
#                           by = c("oao")) %>% 
#   mutate(nazev_zkraceny = if_else(is.na(nazev_zkraceny), 
#                                   oao, 
#                                   nazev_zkraceny))

oao_akce_cl <- left_join(oao_akce, nazev_zkraceny_tab, 
                         by = c("nazev_zkraceny")) %>% 
  mutate(oao = if_else(is.na(oao), 
                       nazev_zkraceny, 
                       oao))

oao_proj_cl <- left_join(oao_proj, nazev_zkraceny_tab, 
                         by = c("nazev_zkraceny")) %>% 
  mutate(oao = if_else(is.na(oao), 
                       nazev_zkraceny, 
                       oao))

# stats -------------------------------------------------------------------

# oao_akce_multipoint <- oao_akce_cl %>% 
#   group_by(oao) %>% 
#   count() %>% 
#   ungroup() %>% 
#   arrange(desc(n))
# 
# oao_proj_multipoint <- oao_proj_cl %>% 
#   group_by(oao) %>% 
#   count() %>% 
#   ungroup() %>% 
#   arrange(desc(n))


# joining -----------------------------------------------------------------

oao_points <- bind_rows(select(oao_akce_cl, oao, geometry),
                        select(oao_proj_cl, oao, geometry)) %>% 
  group_by(oao) %>% 
  count() %>% 
  arrange(desc(n))

# filtering ---------------------------------------------------------------
# ostatní organizace
oao_points_filt <- oao_points %>% 
  semi_join(st_drop_geometry(oao_scope), by = c("oao")) %>% 
  filter(!str_detect(oao, "(m|M)uzeum")) %>% 
  arrange(desc(n)) %>%
  # slice(1:25) %>% 
  mutate(oao = fct_reorder(oao, n, .desc = TRUE))

oao_scope_filt <- semi_join(oao_scope, st_drop_geometry(oao_points_filt), 
                            by = c("oao")) %>% 
  mutate(oao = fct_drop(oao),
         oao = fct_relevel(oao, levels(oao_points_filt$oao)))

oao_points_filt %>% 
  ggplot() +
  geom_sf(data = czrep, color = "gray88", fill = NA) +
  geom_sf(data = oao_scope_filt, aes(fill = oao), color = NA, alpha = 0.2,
          show.legend = FALSE) +
  geom_sf(aes(color = oao), shape = 4, size = 0.8, alpha = 0.05,
          show.legend = F) +
  facet_wrap(~oao, nrow = 4) + 
  theme_void() +
  theme_add

ggsave(here("plots", "oao_ost.png"), width = 21, height = 8)

# muzea
oao_points_muz <- oao_points %>% 
  semi_join(st_drop_geometry(oao_scope), by = c("oao")) %>% 
  filter(str_detect(oao, "(m|M)uzeum")) %>% 
  arrange(desc(n)) %>%
  slice(1:30) %>% 
  mutate(oao = fct_reorder(oao, n, .desc = TRUE))

oao_scope_muz <- semi_join(oao_scope, st_drop_geometry(oao_points_muz), 
                            by = c("oao")) %>% 
  mutate(oao = fct_drop(oao),
         oao = fct_relevel(oao, levels(oao_points_muz$oao)))

oao_points_muz %>% 
  ggplot() +
  geom_sf(data = czrep, color = "gray88", fill = NA) +
  geom_sf(data = oao_scope_muz, aes(fill = oao), color = NA, alpha = 0.2,
          show.legend = FALSE) +
  geom_sf(aes(color = oao), shape = 4, size = 0.8, alpha = 0.05,
          show.legend = F) +
  facet_wrap(~oao) + 
  theme_void() +
  theme_add

ggsave(here("plots", "oao_muzea.png"), width = 18, height = 10)






# leaflet plotting --------------------------------------------------------
# server will filter given oao
# only one? shown in ui?
x <- oao_sf %>% 
  mutate(area = as.numeric(st_area(.)),
         oao = fct_reorder(oao, area, .desc = TRUE)) %>% 
  arrange(desc(area)) %>% 
  slice(30:40) 

polypal <- RColorBrewer::brewer.pal(name = "Paired", n = nrow(x))

# library(leaflet)
leaflet(x) %>%
  addTiles() %>% 
  addPolygons(color = polypal, opacity = 1, fillOpacity = 0)




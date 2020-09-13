# overall overview


# libs --------------------------------------------------------------------

library(tidyverse)
library(sf)
library(here)

# theme not to "cut" lower/upper parts of letters
theme_add <- theme(strip.text.x = element_text(margin = margin(b = 5, t = 5)))


# data --------------------------------------------------------------------
akce <- st_read("data/oao_points", "oao_akce")

czrep <- RCzechia::republika(resolution = "low")
kraje <- RCzechia::kraje(resolution = "low")


# data prep ---------------------------------------------------------------

akce_proc <- akce %>% 
  mutate(posneg = if_else(as.logical(negativni), 
                          "Negativní zjištění", 
                          "Pozitivní zjištění"))

# plotting ----------------------------------------------------------------
# full overview
akce_coords <- st_coordinates(akce_proc)

akce_proc %>% 
  ggplot() +
  geom_hex(aes(x = akce_coords[, 1], 
               y = akce_coords[, 2], 
               fill = log10(..density..)), 
           binwidth = c(0.1, 0.04), show.legend = FALSE) +
  scale_fill_viridis_c() +
  geom_sf(data = czrep, color = "gray88", fill = NA, size = 2) +
  geom_sf(data = kraje, color = "gray88", fill = NA, size = 0.8) +
  theme_void()

ggsave(here("plots", "map_density.png"), width = 9, height = 6)

# pozitivní/negativní
akce_proc %>% 
  ggplot() +
  geom_sf(data = czrep, color = "gray88", fill = NA, size = 2) +
  geom_sf(alpha = 0.2) + 
  facet_wrap(vars(posneg)) +
  theme_void() + 
  theme_add

ggsave(here("plots", "map_posneg.png"), width = 18, height = 6)





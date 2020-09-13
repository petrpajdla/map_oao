# plot období

library(tidyverse)
library(sf)
library(here)

# read data ---------------------------------------------------------------
pian <- read_csv(here("data/processed/pian_obdobi.csv"))

# background data
czrep <- RCzechia::republika(resolution = "low")
kraje <- RCzechia::kraje(resolution = "low")
reky <- RCzechia::reky() %>% 
  filter(Major)

# table obdobi
obdobi <- tribble(~poradi, ~obdobi,
                  1, "Paleolit",
                  2, "Mezolit",
                  3, "Paleolit-Mezolit",
                  4, "Neolit",
                  5, "Eneolit",
                  6, "Neolit-Eneolit",
                  7, "Doba bronzová",
                  8, "Doba halštatská",
                  9, "Bronz-Halštat",
                  10, "Doba laténská",
                  11, "Halštat-Latén",
                  12, "Doba římská",
                  13, "Latén-Řím",
                  14, "Doba stěhování národů",
                  15, "DŘ-DSN",
                  16, "Zemědělský pravěk",
                  17, "Pravěk",
                  18, "Raný středověk",
                  19, "Vrcholný středověk",
                  20, "Středověk",
                  21, "Novověk",
                  22, "Vrcholný středověk-Novověk",
                  23, "Středověk-Novověk",
                  24, "Industrial",
                  25, "Novověk-Industrial")


obdobi_vect <- obdobi$obdobi
names(obdobi_vect) <- obdobi$poradi


# data prep ---------------------------------------------------------------

pian_cl <- pian %>% mutate(poradi = as.character(obdobi_poradi),
                           poradi = str_remove(poradi, "..$"), 
                           poradi = as.integer(poradi),
                           obdobi_sim = unname(obdobi_vect[poradi]),
                           obdobi_sim = fct_reorder(obdobi_sim, poradi)) %>% 
  filter(!is.na(obdobi), 
         obdobi != "nedat")

# sf ----------------------------------------------------------------------

pian_sf <- pian_cl %>% 
  filter(!str_detect(obdobi_sim, "-"),
         !str_detect(obdobi_sim, "(P|p)ravěk"),
         !str_detect(obdobi_sim, "Středověk")) %>% 
  mutate(obdobi_sim = if_else(obdobi_sim %in% c("Doba římská", 
                                                "Doba stěhování národů"),
                              "Doba římská a doba stěhování národů", 
                              as.character(obdobi_sim)),
         obdobi_sim = fct_reorder(obdobi_sim, poradi)) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(4326)
  

# plotting ----------------------------------------------------------------

# edit Paired palette (RColorBrewer)
clrs <- RColorBrewer::brewer.pal(n = 12, name = "Paired")
clrs[11] <- "#C1891A"

# plot
pian_sf %>% 
  ggplot() +
  geom_sf(data = czrep, color = "gray88", fill = NA) +
  geom_sf(data = reky, color = "gray88") +
  geom_sf(aes(color = obdobi_sim), size = 0.8, alpha = 0.4, show.legend = FALSE) +
  scale_color_manual(values = clrs) +
  facet_wrap(vars(obdobi_sim)) +
  theme_void()

ggsave(here("plots", "datace_map.png"), width = 12, height = 6)




  
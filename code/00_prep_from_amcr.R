# joining tables exported from AMCR

library(tidyverse)
library(here)

# read data ---------------------------------------------------------------

# proj <- read_delim(here("data/raw", "export_2021-07-30_projekt.csv"), 
#                    delim = "#", 
#                    col_types = paste0(rep("c", 48), collapse = ""), 
#                    quote = "")

akce <- read_delim(here("data/raw", "export_2021-07-30_akce.csv"), 
                   delim = "#", 
                   col_types = paste0(rep("c", 40), collapse = ""), 
                   quote = "")

# vazba <- read_delim(here("data/raw", "export_2021-07-30_vazba_projekt_akce.csv"), 
#                     delim = "#", 
#                     col_types = paste0(rep("c", 3), collapse = ""), 
#                     quote = "")

pian <- read_delim(here("data/raw", "export_2021-07-30_pian.csv"), 
                   delim = "#", 
                   col_types = paste0(rep("c", 13), collapse = ""), 
                   quote = "")

dj <- read_delim(here("data/raw", "export_2021-07-30_dokumentacni_jednotka.csv"), 
                 delim = "#", 
                 col_types = paste0(rep("c", 7), collapse = ""), 
                 quote = "")

# komp <- read_delim(here("data/raw", "export_2020-09-12_komponenta.csv"), 
#                    delim = "#", 
#                    col_types = paste0(rep("c", 19), collapse = ""), 
#                    quote = "")


# select neccessary columns -----------------------------------------------

# proj <- proj %>% select(ident_cely, id_rok, stav_popis, typ_projektu, organizace_prihlaseni, starts_with("geometry"))
akce <- akce %>% select(ident_cely, organizace, hlavni_typ, pristupnost, datum_ukonceni_v)
pian <- pian %>% select(ident_cely, geom_wkt, starts_with("centroid"))
dj <- dj %>% select(ident_cely, parent, pian, negativni_jednotka)
# komp <- komp %>% select(ident_cely, parent, obdobi, obdobi_poradi, areal)


# filtering ---------------------------------------------------------------

# only last 5 years used
akce_five_y <- akce %>% 
  mutate(date = lubridate::year(lubridate::ymd(datum_ukonceni_v)),
         five_y = date >= lubridate::year(lubridate::today()) - 5) %>% 
  filter(five_y)


# joining tables ----------------------------------------------------------

# akce > dok. j. > pian
akce_pian <- inner_join(akce_five_y, dj, 
                        by = c("ident_cely" = "parent"), 
                        suffix = c("akce", ".dj")) %>% 
  rename(dj = ident_cely.dj) %>% 
  inner_join(pian, 
             by = c("pian" = "ident_cely"))

# # akce_dok.j_pian > komponenta
# pian_komp <- akce_pian %>% 
#   left_join(komp, 
#             by = c("dj" = "parent"), 
#             suffix = c(".akce", ".dj")) %>% 
#   rename(ident_cely = ident_cely.akce) %>% 
#   select(-ident_cely.dj)


# export data -------------------------------------------------------------

akce_pian %>% 
  write_csv(here("data/processed/", "pian_akce.csv"))

# pian_komp %>% 
#   select(pian, obdobi, obdobi_poradi, pristupnost, hlavni_typ, 
#          negativni_jednotka, x = centroid_e, y = centroid_n) %>% 
#   write_csv(here("data/processed", "pian_obdobi.csv"))




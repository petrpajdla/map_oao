# joining tables exported from AMCR

library(tidyverse)
library(here)

# read data ---------------------------------------------------------------

proj <- read_delim(here("data/raw", "export_2020-08-06_projekt.csv"), 
                   delim = "#", 
                   col_types = paste0(rep("c", 48), collapse = ""), 
                   quote = "")

akce <- read_delim(here("data/raw", "export_2020-09-12_akce.csv"), 
                   delim = "#", 
                   col_types = paste0(rep("c", 40), collapse = ""), 
                   quote = "")

vazba <- read_delim(here("data/raw", "export_2020-09-12_vazba_projekt_akce.csv"), 
                    delim = "#", 
                    col_types = paste0(rep("c", 3), collapse = ""), 
                    quote = "")

pian <- read_delim(here("data/raw", "export_2020-09-12_pian.csv"), 
                   delim = "#", 
                   col_types = paste0(rep("c", 13), collapse = ""), 
                   quote = "")

dj <- read_delim(here("data/raw", "export_2020-09-12_dokumentacni_jednotka.csv"), 
                 delim = "#", 
                 col_types = paste0(rep("c", 7), collapse = ""), 
                 quote = "")

komp <- read_delim(here("data/raw", "export_2020-09-12_komponenta.csv"), 
                   delim = "#", 
                   col_types = paste0(rep("c", 19), collapse = ""), 
                   quote = "")


# select neccessary columns -----------------------------------------------

dj <- dj %>% select(ident_cely, parent, pian, negativni_jednotka)
pian <- pian %>% select(ident_cely, geom_wkt, starts_with("centroid"))
akce <- akce %>% select(ident_cely, organizace, hlavni_typ, pristupnost, datum_ukonceni_v)
proj <- proj %>% select(ident_cely, id_rok, stav_popis, typ_projektu, organizace_prihlaseni, starts_with("geometry"))
komp <- komp %>% select(ident_cely, parent, obdobi, obdobi_poradi, areal)


# joining tables ----------------------------------------------------------

# akce > dok. j. > pian
akce_pian <- inner_join(akce, dj, 
                        by = c("ident_cely" = "parent"), 
                        suffix = c("akce", ".dj")) %>% 
  rename(dj = ident_cely.dj) %>% 
  inner_join(pian, 
             by = c("pian" = "ident_cely"))

akce_pian %>% 
  write_csv(here("data/processed/", "pian_akce.csv"))

# akce_dok.j_pian > komponenta
pian_komp <- akce_pian %>% 
  left_join(komp, 
            by = c("dj" = "parent"), 
            suffix = c(".akce", ".dj")) %>% 
  rename(ident_cely = ident_cely.akce) %>% 
  select(-ident_cely.dj)

pian_komp %>% 
  select(pian, obdobi, obdobi_poradi, pristupnost, hlavni_typ, 
         negativni_jednotka, x = centroid_e, y = centroid_n) %>% 
  write_csv(here("data/processed", "pian_obdobi.csv"))




library(tidyverse)
library(googledrive)
library(googlesheets4)


# paths -------------------------------------------------------------------

dir_data <- here::here("data")
# dir_raw <- paste0(dir_data, "/raw")
# dir_der <- paste0(dir_data, "/derived")
dir_fin <- paste0(dir_data, "/final")


# data from GD ------------------------------------------------------------

updated_gd_url <- "https://docs.google.com/spreadsheets/d/1RXXRGpgkrgtBhF9taEtCVuHxVIJcbeATF9RBxBtORJY/edit?usp=sharing"
gd_updated <- drive_get(updated_gd_url)

original_gd_url <- "https://docs.google.com/spreadsheets/d/1oeUfIfMG2nA5vDvxYRnvWuKtUUEuO-SxKLEB9f05lW8/edit#gid=1589848804"
gd_original <- drive_get(original_gd_url)


# updated -----------------------------------------------------------------

# oao s platnou dohodou
oao_platne <- read_sheet(gd_updated, sheet = "Dohody_pracovni") %>% 
  filter(dohoda_platna == 1) %>% 
  pull(nazev_zkraceny)

oao_dohoda <- read_sheet(gd_updated, sheet = "Dohody_pracovni", ) %>% 
  filter(nazev_zkraceny %in% oao_platne)

oao_kontakt <- read_sheet(gd_updated, sheet = "Kontakty_pracovni") %>% 
  filter(nazev_zkraceny %in% oao_platne) %>% 
  mutate(adresa = str_remove(adresa, "(?<=\\d{3})\\s(?=\\d{2})"))

oao_uzemi <- read_sheet(gd_updated, sheet = "Uzemi_pracovni") %>% 
  filter(nazev_zkraceny %in% oao_platne) %>% 
  select(-c_m, -uzemi, -is_praha)

# oao_platne %in% oao_kontakt$nazev_zkraceny %>% table()
# oao_platne[!oao_platne %in% oao_kontakt$nazev_zkraceny]
# oao_dohoda$nazev_zkraceny[oao_dohoda$nazev_zkraceny %>% duplicated()]

# # original ----------------------------------------------------------------
# 
# read_sheet(gd_original, sheet = "")


# geocode address ---------------------------------------------------------

oao_address <- oao_kontakt %>% 
  pull(adresa) %>% 
  RCzechia::geocode() %>% 
  left_join(select(oao_kontakt, nazev_zkraceny, adresa), 
            by = c("address" = "adresa")) %>% 
  select(-type)

# oao_kontakt %>% 
#   left_join(oao_address, by = c("adresa" = "address")) %>% 
#   filter(is.na(result)) %>% 
#   select(nazev_zkraceny, adresa, result, ico) %>% View()


# get ARES names ----------------------------------------------------------

#' Get company name from ARES
#'
#' @param x IČO 
#'
#' @return Character
#' @export
#'
#' @examples
ares_name <- function(x) {
  Sys.sleep(0.01)
  url_ares <- "https://wwwinfo.mfcr.cz/cgi-bin/ares/darv_std.cgi?ico="
  res <- xml2::read_xml(paste0(url_ares, x)) %>% 
    xml2::xml_child(".//are:Obchodni_firma") %>% 
    xml2::xml_text()
  return(res)
}

oao_nazvy <- oao_kontakt %>% 
  select(nazev_zkraceny, ico) %>% 
  mutate(nazev_full = map_chr(ico, \(x) ares_name(x)),
         ico = str_pad(ico, width = 8, pad = "0", side = "left"))


# uzemi textem ------------------------------------------------------------

oao_uzemi_up <- oao_uzemi %>% 
  mutate(uzemi = if_else(is_republika, "Celé území ČR.", NA_character_),
         uzemi = if_else(!is.na(kraj), paste0(kraj, "."), uzemi),
         uzemi = if_else(!is.na(okres), paste0("Okres ", okres, "."), uzemi),
         uzemi = if_else(!is.na(katastr), paste0("Kat. úz. ", katastr, "."), uzemi),
         uzemi = str_replace_all(uzemi, ";", ",")) %>% 
  select(nazev_zkraceny, uzemi)


# dohody a data -----------------------------------------------------------

oao_dohoda_up <- oao_dohoda %>% 
  select(nazev_zkraceny, web, dohoda_mk, starts_with("datum")) %>% 
  mutate(across(starts_with("datum"), \(x) lubridate::ymd(x)))


# bind together -----------------------------------------------------------

oao_out <- oao_nazvy %>% 
  left_join(oao_address, by = c("nazev_zkraceny")) %>% 
  left_join(oao_uzemi_up, by = c("nazev_zkraceny")) %>% 
  left_join(oao_dohoda_up, by = c("nazev_zkraceny")) %>% 
  select(-result) %>% 
  sf::st_as_sf()


# data export -------------------------------------------------------------

sf::write_sf(oao_out, paste0(dir_fin, "/oao_meta.geojson"))

file.copy(paste0(dir_fin, "/oao_meta.geojson"), 
          here::here("./app/oao_meta.geojson"), 
          overwrite = TRUE)

# playground --------------------------------------------------------------

# names to ascii
oao_out %>% 
  mutate(nazev_ascii = iconv(nazev_zkraceny, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
       local_url = paste0("<a href=", "http://127.0.0.1:5774?org=", nazev_ascii, ">", "fuu", "</a>")) %>% 
  select(local_url)

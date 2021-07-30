# This script served to read data from various sources and prepare
# data into google sheets tables to check it manually...

library(tidyverse)
library(rvest)
library(xml2)
library(googledrive)
library(googlesheets4)

dir_create <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}

date_excel2iso <- function(x) {
  y <- if_else(str_length(x) == 5 & !is.na(x), 
               x,
               NA_character_
  )
  
  if_else(!is.na(y),
          as.character(as.Date(as.numeric(y), origin = "1899-12-30")),
          x
  )
}

equal_not_na <- function(x, y) {
  if_else(x == y & !is.na(x) & !is.na(y), x, NA_character_)
}

na_not_na <- function(x, y) {
  if_else(is.na(x) & !is.na(y), y, x)
}

fill_xy <- function(x, y) {
  z = equal_not_na(x, y)
  z = na_not_na(z, x)
  z = na_not_na(z, y)
}

fill_na <- function(x, y, z) {
  r <- na_not_na(x, y)
  na_not_na(r, z)
}

detect_uzemi <- function(x, string) {
  if_else(str_detect(x, string), TRUE, FALSE)
}

# dir structure -----------------------------------------------------------

dir_data <- here::here("data")
dir_raw <- paste0(dir_data, "/raw")
dir_der <- paste0(dir_data, "/derived")

dir_create(dir_data)
dir_create(dir_raw)
dir_create(dir_der)


# cas data ----------------------------------------------------------------

url_cas <- "https://www.arup.cas.cz/kdo-je-opravnen-provadet-archeologicke-vyzkumy/"

html_cas <- read_html(url_cas)

tab_cas <- html_cas %>% 
  html_nodes(xpath = "//table//td[text() = 'Oprávněná organizace']") %>% 
  html_nodes(xpath = "../../..") %>% 
  html_table(header = TRUE, trim = TRUE, fill = TRUE) %>% 
  first() %>% 
  as_tibble() %>% 
  rename(oao_cas = `Oprávněná organizace`, 
         id = IČ, 
         adresa_cas = Adresa, 
         uzemi = `Územní působnost`) %>% 
  mutate(tab_av = TRUE)

# mk cr data --------------------------------------------------------------

# from https://www.mkcr.cz/seznam-organizaci-opravnenych-k-provadeni-archeologickych-vyzkumu-278.html
url_mk <- "https://www.mkcr.cz/doc/cms_library/tabulka-oopav-zakl-udaje-web-20-4618.xls"

mk_xls <-  paste0(dir_raw, "/mk.xls")

# # no need to repeat --------------------
# download.file(url_mk, mk_xls)
# 
# # Extracting hyperlinks
# # Need to manually export xls to xlsx to be able to extract hyperlinks
# mk_xlsx <- sub("raw", "derived", paste0(mk_xls, "x"))
# 
# # stack link?
# # rename file to .zip
# mk_zip <- sub("xlsx", "zip", mk_xlsx)
# file.copy(from = mk_xlsx, to = mk_zip)
# 
# # unzip the file
# unzip(mk_zip, exdir = paste0(dir_der, "/mk"))
# # --------------------------------------

# unzipping produces a bunch of xml files
ws <- paste0(dir_der, "/mk/xl/worksheets")

# extract hyperlinks
mk_id1 <- as_xml_document(paste0(ws, "/sheet1.xml")) %>% 
  xml_child(7) %>% 
  xml_contents() %>% 
  xml_attr("id")

mk_text1 <- as_xml_document(paste0(ws, "/sheet1.xml")) %>% 
  xml_child(7) %>% 
  xml_contents() %>% 
  xml_attr("display")

mk_id2 <- as_xml_document(paste0(ws, "/_rels/sheet1.xml.rels")) %>% 
  xml_contents() %>% 
  xml_attr("Id")

mk_text2 <- as_xml_document(paste0(ws, "/_rels/sheet1.xml.rels")) %>% 
  xml_contents() %>% 
  xml_attr("Target")

# url table
mk_urls <- bind_cols(id = mk_id1, oao_mk = mk_text1) %>% 
  left_join(bind_rows(id = mk_id2, url = mk_text2)) %>% 
  select(-id)

# data table
tab_mk <- readxl::read_excel(mk_xls) %>%
  select(-...1) %>% 
  rename(oao_mk = `Oprávněný subjekt   (www stránky)`,
         id = IČ,
         povoleni_mk = `Povolení MK`,
         datum_mk = `Datum rozhodnutí MK`,
         datum_cas = `Datum uzavření dohody s AV ČR`) %>% 
  left_join(mk_urls, by = c("oao_mk")) %>% 
  mutate(adresa1 = str_c(ulice, `čp./evč.`, sep = " "),
         adresa2 = str_c(PSČ, město, sep = " "),
         adresa_mk = str_c(adresa1, adresa2, sep = ", ")) %>% 
  select(-ulice, -`čp./evč.`, -město, -PSČ, -adresa1, -adresa2) %>% 
  mutate(across(starts_with("datum"), date_excel2iso),
         tab_mk = TRUE)


# aiscr data --------------------------------------------------------------

url_ais <- "https://docs.google.com/spreadsheets/d/1lCR1q8wbfnURUPjO7IJZim99pP4EB14H9fhg0QRJyuw/edit#gid=1915033575"

file_ais <- googledrive::drive_download(url_ais,
                                        path = paste0(dir_raw, "/ais_oao.xlsx"),
                                        overwrite = TRUE)

tab_ais <- readxl::read_excel(file_ais$local_path) %>% 
  mutate(kontrola = str_remove(kontrola, "\\.0$"),
         kontrola = date_excel2iso(kontrola),
         tab_ais = TRUE)


# arub table --------------------------------------------------------------

url_arub <- "https://docs.google.com/spreadsheets/d/1N7Bz3O6gmOfanVuVM8K6pH0ewv-vjvpAX0HgOmS-2Gs/edit#gid=0"

file_arub <- googledrive::drive_download(url_arub,
                                         path = paste0(dir_raw, "/arub_oao.xlsx"),
                                         overwrite = TRUE)

tab_arub <- readxl::read_excel(file_arub$local_path) %>% 
  janitor::clean_names() %>% 
  select(-starts_with("x"), -navrh_nazvu_pro_citace) %>% 
  mutate(tab_arub = TRUE)

# heslar organizaci -------------------------------------------------------

# prevodnik web cas vs web mk cr
prevod <- read_csv(paste0(dir_raw, "/oao_names_prevodnik.csv")) %>% 
  rename(name_mk = name_mkcr, 
         name_cas = name_arup,
         abbrv_ais = abbrv_aiscr)

tab_mk$oao_mk %in% prevod$name_mk %>% table()
tab_cas$oao_cas %in% prevod$name_cas %>% table()

# zaniklé Muzeum Boskovicka
tab_mk[!(tab_mk$oao_mk %in% prevod$name_mk), ]
tab_cas[!(tab_cas$oao_cas %in% prevod$name_cas), ]

# heslar amcr
heslar <- read_csv2(paste0(dir_raw, "/heslar_organizace.csv"))

prevod$abbrv_ais %in% heslar$nazev_zkraceny %>% table()
prevod[!prevod$abbrv_ais %in% heslar$nazev_zkraceny, ]


# joining tables ----------------------------------------------------------

# nerelevantní oao
oao_rm <- c("Space Systems Czech", "INCAD", "SmartGIS", 
            "Archeologický ústav Nitra")

tab <- prevod %>% 
  full_join(tab_mk, by = c("name_mk" = "oao_mk")) %>%
  full_join(tab_cas, by = c("name_cas" = "oao_cas")) %>% 
  select(nazev_zkraceny = abbrv_ais, nazev_mk = name_mk, nazev_av = name_cas,
         starts_with("tab"),
         ico = id.x, ico_mk = id.y, ico_av = id,
         url, adresa_mk, adresa_av = adresa_cas,
         povoleni_mk, datum_mk, datum_av = datum_cas, 
         uzemi) %>% 
  full_join(heslar, by = "nazev_zkraceny") %>% 
  mutate(ico = if_else(ico_mk == ico_av, ico_av, NA_character_),
         adresa = if_else(adresa_mk == adresa_av, adresa_mk, NA_character_)) %>% 
  select(-ico_av, -ico_mk, -adresa_mk, -adresa_av,
         -id, -months_to_publication, -published_accessibility) %>% 
  full_join(tab_ais, by = c("nazev_zkraceny")) %>% 
  full_join(tab_arub, by = c("nazev_zkraceny")) %>% 
  filter(!str_detect(nazev_zkraceny, "^\\["),
         !nazev_zkraceny %in% oao_rm) %>% 
  janitor::clean_names()

# duplicates
# tab %>% 
#   filter(nazev_zkraceny %in% tab$nazev_zkraceny[duplicated(tab$nazev_zkraceny)])


# cleaning data -----------------------------------------------------------

url <- tab %>% select(nazev_zkraceny, contains("url"), contains("web")) %>%
  mutate(l = fill_xy(web_x, url),
         l = fill_na(l, web_y, web_x)) %>%
  select(-url, -starts_with("web"), url = l)

adresa <- tab %>% 
  select(nazev_zkraceny, contains("adresa")) %>%
  mutate(a = fill_xy(adresa, adresa_x),
         a = fill_na(a, adresa_x, adresa_y)) %>%
  select(-contains("adresa"), adresa = a)

email <- tab %>% select(nazev_zkraceny, contains("email")) %>% 
  mutate(e_os = fill_xy(email, email_2),
         e_in = fill_xy(email_inst, email_inst_2)) %>%
  select(-contains("email"), email_kont_os = e_os, email_inst = e_in)

tel <- tab %>% select(nazev_zkraceny, contains("tel")) %>% 
  mutate(t_os = fill_xy(tel, tel_2),
         t_in = fill_xy(tel_inst, tel_inst_2)) %>%
  select(-contains("tel"), tel_kont_os = t_os, tel_inst = t_in)

funkce <- tab %>% select(nazev_zkraceny, contains("funkce")) %>%
  mutate(f = fill_xy(funkce, funkce_2)) %>%
  select(-contains("funkce"), funkce = f)

kont_os <- tab %>% select(nazev_zkraceny, contains("osoba")) %>%
  mutate(o = fill_xy(kont_osoba, kont_osoba_2)) %>%
  select(-contains("osoba"), kont_os = o)

pozn <- tab %>% select(nazev_zkraceny, starts_with("pozn_ob")) %>%
  mutate(p = fill_xy(pozn_ob, pozn_ob_2)) %>%
  select(nazev_zkraceny, pozn = p)

# tab %>% select(nazev_zkraceny, starts_with("amcr")) %>% View()

# result ------------------------------------------------------------------

zaklad <- tab %>% 
  select(starts_with("nazev"), zkratka,
         starts_with("tab"),
         typ_organizace, oao, c_m) %>%
  mutate(nazev = fill_xy(nazev_x, nazev_y)) %>%
  select(nazev_zkraceny, nazev, zkratka, starts_with("nazev"), 
         -nazev_x, -nazev_y, 
         typ_organizace, oao, c_m,
         starts_with("tab")) %>% 
  mutate(pozn = NA_character_) %>% 
  arrange(nazev_zkraceny)

kontakt <- tab %>% select(nazev_zkraceny, c_m, ico) %>% 
  full_join(adresa) %>%
  left_join(kont_os) %>% 
  left_join(funkce) %>% 
  left_join(email) %>% 
  left_join(tel) %>% 
  left_join(pozn) %>% 
  select(nazev_zkraceny, c_m, adresa, ico, 
         kont_os, fuknce_kont_os = funkce, email_kont_os, tel_kont_os,
         email_inst, tel_inst, pozn) %>% 
  arrange(nazev_zkraceny)

uzemi <- tab %>% select(nazev_zkraceny, c_m, uzemi) %>% 
  mutate(is_republika = detect_uzemi(uzemi, "republika"),
         is_kraj = detect_uzemi(uzemi, "kraj"),
         is_okres = detect_uzemi(uzemi, "okres"),
         is_praha = detect_uzemi(uzemi, "Prah"),
         is_katastr = NA,
         kraj = NA_character_,
         okres = NA_character_,
         katastr = NA_character_) %>% 
  mutate(pozn = NA_character_) %>% 
  arrange(nazev_zkraceny)

dohoda <- tab %>% transmute(nazev_zkraceny, 
                            c_m,
                            dohoda_mk = povoleni_mk,
                            datum_mk_od = datum_mk,
                            datum_mk_do = NA,
                            dohoda_av = NA_character_,
                            datum_av_od = datum_av,
                            dohoda_av_do= NA,
                            dohoda_amcr = NA_character_,
                            amcr_pristupnost = dokum_x,
                            pozn = NA_character_) %>% 
  arrange(nazev_zkraceny)

provozni <- tab %>% select(nazev_zkraceny, c_m, dotaznik = dotaznik_x, 
                         amcr_praha = amcr, amcr_brno = amcr_2, pozn) %>% 
  arrange(nazev_zkraceny)

pozn_praha <- tab %>% select(nazev_zkraceny, c_m, kontrola, 
                             sak, kosa, vrak, kvak, jak, lak, pak, zak) %>% 
  mutate(pozn = NA_character_) %>% 
  arrange(nazev_zkraceny)

pozn_brno <- tab %>% select(nazev_zkraceny, c_m, dotaznik = dotaznik_y, 
                            odevzdavani_nz_tom, 
                            zapis_projektu_v_amcr_tom, 
                            urgovano_posunuti_projektu_v_amcr_2020,
                            archivace_amcr_vaclav) %>% 
  mutate(pozn = NA_character_) %>% 
  arrange(nazev_zkraceny)


# output ------------------------------------------------------------------

gd_url <- "https://docs.google.com/spreadsheets/d/1oeUfIfMG2nA5vDvxYRnvWuKtUUEuO-SxKLEB9f05lW8/edit#gid=0"

gd_file <- drive_get(gd_url)

gs4_get(gd_file)

# # only once, then it is effectively overwritten
# sheet_add(ss = gd_file, sheet = "Zaklad", .before = 1)
# sheet_add(ss = gd_file, sheet = "Kontakty", .after = 1)
# sheet_add(ss = gd_file, sheet = "Dohody", .after = 2)
# sheet_add(ss = gd_file, sheet = "Uzemi", .after = 3)
# sheet_add(ss = gd_file, sheet = "Pracovni", .after = 4)
# sheet_add(ss = gd_file, sheet = "PoznPraha", .after = 5)
# sheet_add(ss = gd_file, sheet = "PoznBrno", .after = 6)

sheet_write(ss = gd_file, data = zaklad, sheet = "Zaklad")
sheet_write(ss = gd_file, data = kontakt, sheet = "Kontakty")
sheet_write(ss = gd_file, data = dohoda, sheet = "Dohody")
sheet_write(ss = gd_file, data = uzemi, sheet = "Uzemi")
sheet_write(ss = gd_file, data = provozni, sheet = "Pracovni")
sheet_write(ss = gd_file, data = pozn_praha, sheet = "PoznPraha")
sheet_write(ss = gd_file, data = pozn_brno, sheet = "PoznBrno")

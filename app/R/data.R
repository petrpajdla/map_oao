#' Read metadata on OAO
#' 
#' Reads and manipulates metadata on OAO.
#'
#' @param dsn Data.
#'
#' @return A \code{sf} object.
#' @export
#'
#' @examples
oao_meta <- function(dsn) {
  sf::st_read(dsn = dsn) %>% 
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("datum"), \(x) format(x, "%d. %m. %Y")),
      datum_mk = dplyr::if_else(is.na(datum_mk_do),
                                stringr::str_c("od ", datum_mk_od),
                                stringr::str_c("od ", datum_mk_od, " do ", datum_mk_do)),
      datum_av = dplyr::if_else(is.na(datum_av_do),
                                stringr::str_c("od ", datum_av_od),
                                stringr::str_c("od ", datum_av_od, " do ", datum_av_do)),
      dplyr::across(c("datum_av", "datum_mk"), 
                    \(x) stringr::str_remove_all(x, "(?<=\\s)0")),
      opravneni = dplyr::if_else(
        !nazev_zkraceny %in% c("Archeologický ústav Brno", "Archeologický ústav Praha"),
        dplyr::if_else(
          !stringr::str_detect(nazev_zkraceny, "ÚAPP"), 
          paste0(dplyr::if_else(
            !is.na(datum_mk), 
            paste0("Platnost oprávnění MK ČR ",
                   dplyr::if_else(!is.na(dohoda_mk), 
                                  paste0("(", dohoda_mk, ") "), 
                                  ""),
                   datum_mk, ". "), 
            ""), 
            "Dohoda s AV ČR ", datum_av, "."), 
          paste0(dohoda_mk, ". Dohoda s AV ČR ", datum_av, ".")), 
        "Oprávnění v plném rozsahu dle zákona o státní památkové péči."),
      web = dplyr::if_else(
        !is.na(web), 
        paste0("<a target='_blank' href='", web, "'>",
               icon("fas fa-external-link-alt"), " ", web, "</a>"),
        "")) %>% 
    dplyr::arrange(nazev_zkraceny)
}

#' Wrapper around \code{sf::st_read}
#'
#' @param dsn Data.
#'
#' @return A \code{sf} object.
#' @export
#'
#' @examples
oao_sf <- function(dsn) {
  sf::st_read(dsn = dsn)
}

oao_filter <- function(data, oao) {
  data %>% 
    dplyr::filter(nazev_zkraceny == oao)
}
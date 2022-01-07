
#' Create DT
#'
#' @param data Prepared tibble with metadata. 
#'
#' @return
#' @export
#'
#' @examples
dt_create <- function(data) {
  data %>% 
    DT::datatable(
      escape = FALSE,
      extensions = 'Scroller', 
      rownames = FALSE,
      colnames = c(
        "Organizace" = "nazev",
        "IČO" = "ico",
        "Webové stránky" = "web",
        "Adresa" = "adresa",
        "PovolenÍ MK ČR" = "mk_to",
        "Smlouva s AV ČR" = "av_to"),
      options = list(
        dom = "ti",
        deferRender = TRUE,
        scrollY = "calc(100vh - 280px)", 
        scroller = TRUE,
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      )
    )
}


#' Prepare data for DT
#'
#' @param data A tibble with metadata.
#'
#' @return
#' @export
#'
#' @examples
dt_data_prep <- function(data) {
  data %>% 
    dplyr::mutate(
      dplyr::across(
        c("nazev", "adresa"), 
        \(x) stringr::str_replace_all(
          stringr::str_wrap(x, width = 36), "\\n", "<br>")),
      dplyr::across(
        dplyr::ends_with(
          c("from", "to")), 
        \(x) lubridate::dmy(x)),
    ) %>% 
    dplyr::select(nazev, ico, web, adresa, mk_to, av_to)
}


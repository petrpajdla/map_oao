oao_filter_poly <- function(data, click, rep) {
  # oprávnění mají
  poly_pred <- sf::st_intersects(data, click)
  
  sf::st_drop_geometry(data)[poly_pred %>% lengths() > 0, ] %>% 
    dplyr::arrange(area) %>% 
    # removes whole republic oao
    dplyr::filter(!nazev_zkraceny %in% rep$nazev_zkraceny) %>%
    dplyr::select(Organizace = nazev_zkraceny)
}

oao_filter_grid <- function(data, click, buffer) {
  # výzkumy provádí
  grid_pred <- sf::st_intersects(
    data,
    sf::st_buffer(click, buffer * 1e3)
  )
  
  sf::st_drop_geometry(data)[grid_pred %>% lengths() > 0, ] %>%
    dplyr::group_by(nazev_zkraceny) %>%
    dplyr::summarize(value = sum(value)) %>%
    dplyr::arrange(dplyr::desc(value)) %>%
    dplyr::select(Organizace = nazev_zkraceny)
}

click_cell <- function(click) {
  sf::st_bbox(sf::st_buffer(click, 1e3))
}

click_buffer <- function(click, buffer) {
  sf::st_bbox(sf::st_buffer(click, buffer * 1e3))
}


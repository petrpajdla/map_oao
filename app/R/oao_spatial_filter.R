oao_filter_poly <- function(data, click, rep, oao_names) {
  # oprávnění mají
  poly_pred <- sf::st_intersects(data, click)
  
  sf::st_drop_geometry(data)[poly_pred %>% lengths() > 0, ] %>% 
    dplyr::arrange(area) %>% 
    # removes whole republic oao
    dplyr::filter(!ico %in% rep$ico) %>%
    dplyr::mutate(nazev = unname(oao_names[ico])) %>% 
    dplyr::select(Organizace = nazev)
}

oao_filter_grid <- function(data, click, buffer, oao_names) {
  # výzkumy provádí
  grid_pred <- sf::st_intersects(
    data,
    sf::st_buffer(click, buffer * 1e3)
  )
  
  sf::st_drop_geometry(data)[grid_pred %>% lengths() > 0, ] %>%
    dplyr::group_by(ico) %>%
    dplyr::summarize(value = sum(value)) %>%
    dplyr::arrange(dplyr::desc(value)) %>%
    dplyr::mutate(nazev = oao_names[ico]) %>% 
    dplyr::select(Organizace = nazev)
}

click_cell <- function(click) {
  sf::st_bbox(sf::st_buffer(click, 1e3))
}

click_buffer <- function(click, buffer) {
  sf::st_bbox(sf::st_buffer(click, buffer * 1e3))
}

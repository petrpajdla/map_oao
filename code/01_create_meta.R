


mutate(nazev_ascii = iconv(nazev_zkraceny, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
       local_url = paste0("<a href=", "http://127.0.0.1:5774?org=", nazev_zkraceny, ">", "fuu", "</a>"))

oao_names %>% colnames

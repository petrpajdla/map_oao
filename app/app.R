# Libraries
library(shiny)
library(leaflet)
library(DT)
library(shinycssloaders)


# Funs --------------------------------------------------------------------

# url main part
# "https://knytt.shinyapps.io/map_oao_test/?org=%C3%9AAPP%20Brno"
url_main <- "https://knytt.shinyapps.io/map_oao_test/"

add_link <- function(x) {
  x %>% 
    dplyr::mutate(text = paste0(
      "<a href=", url_main, 
      "?org=", stringr::str_replace_all(nazev_zkraceny, "\\s", "%20"), ">", 
      as.character(icon("fas fa-link")), "</a> ", nazev_zkraceny
    ))
}

# Data input ---------------------------------------------------------------
oao_names <- readr::read_csv("oao_names.csv")
oao_scope <- sf::st_read(dsn = "oao_scope.geojson") %>% 
  add_link()
oao_grid <- sf::st_read(dsn = "oao_grid.geojson")

# oao scope whole country
oao_rep <- oao_scope %>% dplyr::filter(area >= 7.8e10) %>% 
  sf::st_drop_geometry()

# spinner image
spinner = "aiscr_spinner.gif"
sleep <- 1.2 # 1.2

# UI definition ------------------------------------------------------------

ui <- function(request) {
  navbarPage(
    title = "Organizace s oprávněním provádět archeologický výzkum",
    id = "mainTab",
    # selected = "Mapa",
    windowTitle = "Mapa OAO", 
    lang = "cs",
    theme = shinythemes::shinytheme('sandstone'),
    header = tags$head(
      tags$style(HTML("
      footer {
        position:absolute;
        bottom:0;
        width:100%;
        height:100px;
        color: white;
        padding-left: 40px;
        background-color: #3E3F3A;
        z-index: 1000;
      }
    
      .flex-container {
        display: flex;
        align-items: stretch;
        background-color: #3E3F3A;
      }

      .flex-container > div {
        background-color: #3E3F3A;
        color: white;
        margin: 10px;
        text-align: center;
        line-height: 75px;
        font-size: 12px;
      }
      
      .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
        background: #3E3F3A;
        opacity: 0.6;
      }
      
      .leaflet-control-layers-base {
        text-align: left;
      }
      "))
    ),
    footer = tags$footer(
      tags$div(
        class = 'flex-container',
        tags$div(style = 'flex-grow: 1', 
                 tags$a(href = 'https://www.arup.cas.cz/', target = '_blank',
                        tags$img(src = 'ARU_logo_bile.png', height = '60px'))),
        tags$div(style = 'flex-grow: 1', 
                 tags$a(href = 'https://www.aiscr.cz/', target = '_blank',
                        tags$img(src = 'AISCR_CZ_H_White.png', height = '75px'))),
        tags$div(style = 'flex-grow: 1', 
                 tags$a(href = 'https://arub.avcr.cz/', target = '_blank',
                        tags$img(src = 'ARUB_logo_bile_RGB_HR.png', height = '80px')))
      )
    ), 
    
    
    # Main map ----------------------------------------------------------------
    
    tabPanel(
      value = "Mapa",
      title = "Mapa", 
      icon = icon("fas fa-map"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("oao", "Organizace:", 
                      choices = c(Vyberte = "", oao_names$oao), 
                      selectize = TRUE, multiple = FALSE),
          fluidRow(
            column(7,
                   checkboxInput("poly", "Zobrazit územní působnost", value = TRUE)
            ),
            column(5,
                   checkboxInput("grid", "Zobrazit akce", value = TRUE)
            )),
          hr(),
          h3(textOutput("name")),
          htmlOutput("address"),
          h4("Detaily povolení MK ČR"),
          textOutput("mk"),
          textOutput("mk_note"),
          h4("Detaily dohody s AV ČR"),
          textOutput("av"),
          textOutput("av_note"),
          # shinythemes::themeSelector()
        ),
        mainPanel(width = 9,
                  tags$style(type = "text/css", "#map {height: calc(100vh - 200px) !important;}"),
                  leafletOutput("map") %>% 
                    withSpinner(image = spinner)
        ),
      )
    ),
    
    # Small map ---------------------------------------------------------------
    
    tabPanel(
      value = "Hledej",
      title = "Hledej dle polohy", 
      icon = icon("fas fa-search"),
      fluidRow(
        column(6,
               tags$style(type = "text/css", "#small_map {height: calc(100vh - 200px) !important;}"),
               leafletOutput("small_map") %>% 
                 withSpinner(image = spinner)
        ),
        column(6,
               tabsetPanel(
                 tabPanel(
                   "Oprávnění mají",
                   HTML("<div style = 'padding: 15px;'>
                      Kliknutím do mapy zvolte bod zájmu.
                      Na zadaném území mohou archeologický 
                      výzkum provádět organizace uvedené v tabulce níže.
                      Organizace jsou řazeny vzestupně dle velikosti území, 
                      na kterém jsou oprávněny provádět archeologický výzkum.
                      </div>"),
                   fluidRow(
                     column(6, h4("Oprávnění ve vybrané oblasti"), 
                            tableOutput("click_table")
                            # withSpinner(color = "#3E3F3A", size = 0.6)
                     ),
                     column(6, h4("Oprávnění na celé území ČR"),
                            tableOutput("republika_table")
                     )
                   ),
                   uiOutput("buffer_bbox1")
                 ),
                 tabPanel(
                   "Výzkumy provádí",
                   fluidRow(
                     column(7,
                            tags$div(
                              style = 'padding: 15px;',
                              "Kliknutím do mapy zvolte bod zájmu. V okruhu ",
                              textOutput("buffer", inline = TRUE, ),
                              " km archeologický výzkum v posledních 5 letech
                              prováděly organizace uvedené v tabulce níže.
                              Organizace jsou řazeny sestupně dle počtu 
                              archeologických výzkumů v zadané vzdálenosti.")
                     ),
                     column(5,
                            tags$div(
                              style = 'padding-top: 10px;',
                              sliderInput("buffer", "Zvolte vzdálenost",
                                          min = 5, max = 20,
                                          value = 5, step = 5,
                                          ticks = FALSE, post = " km")
                            )
                     )
                   ),
                   h4("Výzkumy v zadané vzdálenosti"),
                   tableOutput("buffer_table"),
                   uiOutput("buffer_bbox2")
                 )
               )
        )
      )
    ),
    
    # Seznam tab ----------------------------------------------------------------
    
    tabPanel(
      value = "Seznam",
      title = "Seznam organizací", 
      icon = icon("fas fa-bars"),
      # fluidRow(
      #   column(2, selectInput("kraj", "Kraj:",
      #                         choices = c("Všechny"),
      #                         selectize = TRUE, multiple = TRUE),
      #   ),
      #   column(2, selectInput("ores", "Okres:",
      #                         choices = c("Všechny"),
      #                         selectize = TRUE, multiple = TRUE),
      #   ),
      # ),
      tags$div(
        DT::dataTableOutput("table") %>%
          withSpinner(image = spinner)
        # style = "overflow-y: auto; height: calc(100vh - 100px) !important;"
      )
    ),
    
    # About tab ---------------------------------------------------------------
    
    tabPanel(
      title = "O aplikaci", 
      icon = icon("fas fa-info-circle"),
      includeMarkdown("readme.md")
    )
  )
  
}

# Define server logic -----------------------------------------------------

server <- function(input, output, session) {
  
  
  # Input from URL ----------------------------------------------------------
  
  # query parameters: 
  # ?org=Archeo pro
  
  # selecting tabs:
  # ?tab=Mapa ?tab=Hledej ?tab=Seznam
  
  observe({
    url_query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(url_query[["tab"]])) {
      updateNavlistPanel(session, "mainTab", selected = url_query[["tab"]])
    }
    
    if (!is.null(url_query[["org"]])) {
      updateTextInput(session, "oao", value = url_query[["org"]])
    }
  })
  
  # allowing Bookmarking
  # observe({
  #   reactiveValuesToList(input)
  #   session$doBookmark()
  # })
  # 
  # onBookmarked(updateQueryString)
  
  # Reactives ---------------------------------------------------------------
  
  oao_scope_flt <- reactive({
    oao_scope %>% dplyr::filter(nazev_zkraceny == input$oao)
  })
  
  oao_grid_flt <- reactive({
    oao_grid %>% dplyr::filter(nazev_zkraceny == input$oao)
  })
  
  oao_names_flt <- reactive({
    oao_names %>% dplyr::filter(oao == input$oao)
  })
  
  output$buffer <- renderText({
    input$buffer
  })
  
  # Main text -------------------------------------------------------------
  
  output$name <- renderText({
    oao_names_flt() %>% dplyr::pull(name)
  })
  
  output$address <- renderText({
    oao_names_flt() %>% dplyr::pull(address)
  })
  
  output$mk <- renderText({
    oao_names_flt() %>% dplyr::pull(mkcr_date)
  })
  
  output$mk_note <- renderText({
    oao_names_flt() %>% dplyr::pull(mkcr_note)
  })
  
  output$av <- renderText({
    oao_names_flt() %>% dplyr::pull(avcr_date)
  })
  
  output$av_note <- renderText(({
    oao_names_flt() %>% dplyr::pull(avcr_note)
  }))
  
  
  # Main map -----------------------------------------------------------------
  
  leaflet_map <- oao_scope %>% 
    leaflet(options = leafletOptions(minZoom = 7, maxZoom = 13)) %>% 
    setView(zoom = 8, lng = 15.4730, lat = 49.8175) %>% 
    setMaxBounds(11, 48, 20, 52) %>% 
    addTiles(group = "Open Street Map") %>% 
    addProviderTiles(providers$CartoDB.Positron, 
                     group = "Desaturovaná mapa") %>% 
    addTiles(urlTemplate = paste0("http://ags.cuzk.cz/arcgis/rest/services/zmwm/",
                                  "MapServer/tile/{z}/{y}/{x}?blankTile=false"), 
             attribution = paste0("Základní Mapy ČR ©",
                                  "<a href='https://www.cuzk.cz/'",
                                  "target = '_blank'>ČÚZK</a>"), 
             group = "Základní mapy ČR") %>% 
    addLayersControl(baseGroups = c(
      "Open Street Map", 
      "Desaturovaná mapa", 
      "Základní mapy ČR"),
      options = layersControlOptions(collapsed = TRUE))
  
  output$map <- renderLeaflet({
    Sys.sleep(sleep)
    leaflet_map
  })
  
  # clear map when org is switched
  observeEvent(input$oao, {
    leafletProxy("map") %>%
      clearShapes()
  })
  
  # add/remove grid and polygon
  observe({
    
    if (!input$poly) {
      leafletProxy("map") %>%
        removeShape("poly")
    }
    
    if (!input$grid) {
      leafletProxy("map") %>%
        removeShape(oao_grid_flt()$ctverec)
    }
    
    if (!input$poly & !input$grid) {
      leafletProxy("map") %>%
        clearShapes()
    }
    
    if (input$grid) {
      pal <- colorNumeric(palette = "Blues", domain = oao_grid_flt()$scaled)
      leafletProxy("map", data = oao_grid_flt()) %>% 
        addPolygons(layerId = oao_grid_flt()$ctverec, color = ~pal(scaled), 
                    stroke =  FALSE, fillOpacity = 0.6)
    }
    
    if (input$poly) {
      leafletProxy("map", data = oao_scope_flt()) %>%
        addPolygons(layerId = "poly", fill = NA, color = "#3E3F3A", weight = 4)
    }
    
  })
  
  # Small map ---------------------------------------------------------------
  
  output$small_map <- renderLeaflet({
    Sys.sleep(sleep)
    leaflet_map
  })
  
  observe({
    # coordinates of map click
    click <- input$small_map_click
    
    if (!is.null(click)) {
      
      leafletProxy("small_map") %>%
        clearMarkers() %>%
        addCircleMarkers(click$lng, click$lat, 
                         color = "#3E3F3A", radius = 16, 
                         stroke = TRUE, fillOpacity = 0.6,
                         popup = "Zvolená poloha")
      
      # point click
      click_sf <- sf::st_as_sf(data.frame(click), 
                               coords = c("lng", "lat"), crs = 4326)
      
      # oprávnění mají
      scope_pred <- sf::st_intersects(oao_scope, click_sf)
      
      click_oao <- sf::st_drop_geometry(oao_scope)[scope_pred %>% lengths() > 0, ] %>% 
        dplyr::arrange(area) %>% 
        dplyr::filter(!nazev_zkraceny %in% oao_rep$nazev_zkraceny) %>% 
        dplyr::select(Organizace = text)
      
      # click table
      output$click_table <- renderTable({
        click_oao
      }, sanitize.text.function = function(x) x)
      
      # click link
      click_bbox1 <- sf::st_bbox(sf::st_buffer(click_sf, 1e3))
      
      output$buffer_bbox1 <- renderUI({
        tagList(
          "Zobrazit okolí vybraného bodu v ",
          tags$a(
            icon('fas fa-external-link-alt'), "Digitálním archivu AMČR.", 
            href = paste0("https://digiarchiv.aiscr.cz/results?mapa=true&loc_rpt=", 
                          click_bbox1[2], ",", click_bbox1[1], ",", 
                          click_bbox1[4], ",", click_bbox1[3],
                          "&entity=projekt"),
            target = "_blank"
          ))
      })
      
      # výzkumy provádí (grid table)
      grid_pred <- sf::st_intersects(oao_grid, 
                                     sf::st_buffer(click_sf, input$buffer * 1e3))
      
      buffer_oao <- sf::st_drop_geometry(oao_grid)[grid_pred %>% lengths() > 0, ] %>% 
        dplyr::group_by(nazev_zkraceny) %>% 
        dplyr::summarize(value = sum(value)) %>% 
        dplyr::arrange(dplyr::desc(value)) %>% 
        add_link() %>% 
        dplyr::select(Organizace = text)
      
      # buffer table
      output$buffer_table <- renderTable({
        buffer_oao
      }, sanitize.text.function = function(x) x)
      
      # buffer bbox
      click_bbox2 <- sf::st_bbox(sf::st_buffer(click_sf, input$buffer * 1e3))
      
      output$buffer_bbox2 <- renderUI({
        tagList(
          "Zobrazit vybranou oblast v ",
          tags$a(
            icon('fas fa-external-link-alt'), "Digitálním archivu AMČR.", 
            href = paste0("https://digiarchiv.aiscr.cz/results?mapa=true&loc_rpt=", 
                          click_bbox2[2], ",", click_bbox2[1], ",", 
                          click_bbox2[4], ",", click_bbox2[3],
                          "&entity=projekt"),
            target = "_blank"
          ))
      })
    }
  })
  
  # oao scope republika table
  output$republika_table <- renderTable({
    oao_rep %>% 
      dplyr::select(Organizace = text)
  }, sanitize.text.function = function(x) x)
  
  # Seznam ---------------------------------------------------------------
  
  output$table <- DT::renderDataTable({
    Sys.sleep(sleep)
    DT::datatable(oao_names, 
                  extensions = 'Scroller', 
                  # height = '240px',
                  rownames = oao_names$oao,
                  colnames = c("IČO" = "id", "Název organizace" = "name"),
                  options = list(
                    # pageLength = 40,
                    deferRender = TRUE,
                    scrollY = "calc(100vh - 300px)", 
                    scroller = TRUE
                  )
    )
  })
}


# Run the App -------------------------------------------------------------

shinyApp(ui, server, enableBookmarking = "url")

# Libraries
library(shiny)
library(leaflet)
library(DT)
library(shinycssloaders)

# Data input
oao_names <- readr::read_csv("oao_names.csv")
oao_scope <- sf::st_read(dsn = "oao_scope.geojson")
oao_grid <- sf::st_read(dsn = "oao_grid.geojson")

# spinner image
spinner = "aiscr_spinner.gif"
sleep <- 0 # 1.2

# navbarPage("Location of Blood Banks", id="main",
#            tabPanel("Map", leafletOutput("bbmap", height=1000)),
#            tabPanel("Data", DT::dataTableOutput("data")),
#            tabPanel("Read Me",includeMarkdown("readme.md")))


# Define UI ---------------------------------------------------------------

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
                 tags$a(href = 'http://arub.avcr.cz/', target = '_blank',
                        tags$img(src = 'ARUB_logo_bile_RGB_HR.png', height = '80px')))
      )
    ), 
    
    
    # Main map ----------------------------------------------------------------
    
    tabPanel(
      value = "Mapa",
      title = "Mapa", 
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
                      Na zadáném území mohou archeologický výzkum provádět organizace uvedené níže v tabulce.
                      Organizace jsou řazeny vzestupně dle velikosti území, na kterém jsou oprávněny provádět archeologický výzkum.
                      </div>"),
                   tableOutput("click_table") %>% 
                     withSpinner(color = "#3E3F3A", size = 0.6)
                 ),
                 tabPanel(
                   "Výzkumy provádí",
                   fluidRow(
                     column(5,
                            tags$div(
                              style = 'padding: 15px;', 
                              "V okruhu ", 
                              textOutput("radius", inline = TRUE),
                              " km archeologický výzkum v minulosti prováděly následující organizace."),
                     ),
                     column(7,
                            tags$div(
                              style = 'padding: 10px;',
                              sliderInput("radius", "Zvolte vzdálenost", min = 1, max = 15, value = 5, ticks = FALSE, post = " km")
                            )
                     )
                   ),
                   tableOutput("table_filtered")
                 )
               )
        )
      )
    ),
    
    # List tab ----------------------------------------------------------------
    
    tabPanel(
      value = "Seznam",
      title = "Seznam organizací",
      fluidRow(
        column(2, selectInput("kraj", "Kraj:",
                              choices = c("Všechny"),
                              selectize = TRUE, multiple = TRUE),
        ),
        column(2, selectInput("ores", "Okres:",
                              choices = c("Všechny"),
                              selectize = TRUE, multiple = TRUE),
        ),
      ),
      # tags$style(type = "text/css", "#table {height: calc(100vh - 200px) !important;}"),
      tags$div(
        DT::dataTableOutput("table") %>%
          withSpinner(image = spinner),
        style = "overflow-y: auto; height: calc(100vh - 300px) !important;" # "height: 600px"
      )
    ),
    
    # About tab ---------------------------------------------------------------
    
    tabPanel(
      title = "O aplikaci",
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
  
  output$radius <- renderText({
    input$radius
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
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 13)) %>% 
    setView(zoom = 8, lng = 15.4730, lat = 49.8175) %>% 
    setMaxBounds(11, 48, 20, 52) %>% 
    addTiles(group = "OpenStreetMap") %>% 
    addProviderTiles(providers$CartoDB.Positron, 
                     group = "CARTO Pozitron") %>% 
    addTiles(urlTemplate = "http://ags.cuzk.cz/arcgis/rest/services/zmwm/MapServer/tile/{z}/{y}/{x}?blankTile=false", 
             attribution = "Základní Mapy ČR © <a href='https://www.cuzk.cz/' target = '_blank'>ČÚZK</a>", 
             group = "Základní mapy ČR") %>% 
    addLayersControl(baseGroups = c(
      "OpenStreetMap", 
      "CARTO Pozitron", 
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
        addPolygons(layerId = "poly", fill = NA, color = "#3E3F3A", weight = 8)
    }
    
  })
  
  # Small map ---------------------------------------------------------------
  
  output$small_map <- renderLeaflet({
    Sys.sleep(sleep)
    leaflet_map
  })
  
  # map_click <- list(15.4553852098021, 49.8612331525854)
  # names(map_click) <- c("lng", "lat")
  
  # predicate <- sf::st_intersects(oao_scope, map_click_point)
  # sf::st_drop_geometry(oao_scope)[predicate %>% lengths() > 0, ]
  
  observe({
    click <- input$small_map_click
    
    if (!is.null(click)) {
      
      leafletProxy("small_map") %>%
        clearMarkers() %>%
        addCircleMarkers(click$lng, click$lat, 
                         color = "#3E3F3A", radius = 16, 
                         stroke = TRUE, fillOpacity = 0.6,
                         popup = "Zvolená poloha")
      
      click_sf <- sf::st_as_sf(data.frame(click), 
                               coords = c("lng", "lat"), crs = 4326)
      click_predicate <- sf::st_intersects(oao_scope, click_sf)
      click_oao <- sf::st_drop_geometry(oao_scope)[click_predicate %>% lengths() > 0, ] %>% 
        dplyr::arrange(area) %>% 
        dplyr::mutate() %>% 
        dplyr::select(Organizace = nazev_zkraceny, Detail = local_url, nazev_ascii)
      
      
      output$click_table <- renderTable({
        Sys.sleep(sleep)
        click_oao
      }, sanitize.text.function = function(x) x)
    }
  })
  
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
                    scrollY = "calc(100vh - 410px)", 
                    scroller = TRUE
                  )
    )
  })
  
  output$table_filtered <- renderTable({
    oao_names[1:6, 1:6]
  })
  
}


# Run the App -------------------------------------------------------------

shinyApp(ui, server, enableBookmarking = "url")

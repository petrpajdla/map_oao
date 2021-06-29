# Libraries
library(shiny)
library(leaflet)
library(DT)
library(shinycssloaders)

# Data input
oao_names <- readr::read_csv("oao_names.csv")
oao_scope <- sf::st_read(dsn = "oao_scope.geojson")
oao_points <- sf::st_read(dsn = "oao_points.geojson")

# spinner image
spinner = "aiscr_spinner.gif"
sleep <- 1.2

# navbarPage("Location of Blood Banks", id="main",
#            tabPanel("Map", leafletOutput("bbmap", height=1000)),
#            tabPanel("Data", DT::dataTableOutput("data")),
#            tabPanel("Read Me",includeMarkdown("readme.md")))


# Define UI ---------------------------------------------------------------

ui <- navbarPage(
  selected = "Hledej dle polohy",
  windowTitle = "Mapa OAO", 
  lang = "cs", id = "fuu",
  theme = shinythemes::shinytheme('sandstone'),
  "Organizace s oprávněním provádět archeologický výzkum",
  # footer = HTML("<br><div style='height: 100%; min-height: 100%; display: flex;	flex-direction: column;'>",
  #               "<div style='background-color: #3E3F3A; border: 20px solid #3E3F3A;'>",
  #               ,
  #               "</div></div>"),
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
    "Mapa", 
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("oao", "Organizace:", 
                    choices = c(Vyberte = "", oao_names$oao), 
                    selectize = TRUE, multiple = FALSE),
        checkboxInput("points", "Zobrazit akce vybrané OAO"),
        conditionalPanel("input.points",
                         dateRangeInput("date", "Vyberte datum:", 
                                        min = "2017-06-01",
                                        max = "2020-12-31",
                                        start = "2020-01-01",
                                        end = "2020-12-31", 
                                        startview = "year", 
                                        weekstart = 1, 
                                        separator = "až", 
                                        format = "d. m. yyyy", 
                                        language = "cs")
        ),
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
        # textOutput("z")
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
    "Hledej dle polohy",
    fluidRow(
      column(6,
             tags$style(type = "text/css", "#small_map {height: calc(100vh - 200px) !important;}"),
             leafletOutput("small_map")
      ),
      column(6,
             tabsetPanel(
               tabPanel(
                 "Oprávnění mají",
                 HTML("<div style = 'padding: 15px;'>Na zadáném území mohou archeologický výzkum provádět následující organizace.</div>")
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
    "Seznam organizací",
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
    DT::dataTableOutput("table") %>%
      withSpinner(image = spinner)
  ),
  
  # About tab ---------------------------------------------------------------
  
  tabPanel(
    "O aplikaci",
    includeMarkdown("readme.md")
  )
)


# Define server logic -----------------------------------------------------

server <- function(input, output, session) {
  
  
  # Input from URL ----------------------------------------------------------
  
  observe({
    url_query <- parseQueryString(session$clientData$url_search)
    if (!is.null(url_query[["org"]])) {
      updateTextInput(session, "oao", value = url_query[["org"]])
    }
  })
  
  # Reactives ---------------------------------------------------------------
  
  oao_scope_flt <- reactive({
    oao_scope %>% dplyr::filter(oao == input$oao)
  })
  
  oao_points_flt <- reactive({
    oao_points %>% dplyr::filter(oao == input$oao,
                                 datum >= input$date[1],
                                 datum <= input$date[2])
  })
  
  oao_names_flt <- reactive({
    oao_names %>% dplyr::filter(oao == input$oao)
  })
  
  output$radius <- renderText({
    input$radius
  })
  
  # Render text -------------------------------------------------------------
  
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
  
  
  # Leaflet -----------------------------------------------------------------
  
  leaflet_map <- oao_scope %>% 
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 15)) %>% 
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
      # overlayGroups = c("Quakes", "Outline"),
      options = layersControlOptions(collapsed = TRUE))
  
  output$map <- renderLeaflet({
    Sys.sleep(sleep)
    leaflet_map
  })
  
  observe({
    leafletProxy("map", data = oao_scope_flt()) %>%
      clearShapes() %>% 
      clearMarkers() %>%
      addPolygons(fill = NA, color = "#3E3F3A", weight = 8)
    
    if (input$points) {
      leafletProxy("map", data = oao_points_flt()) %>% 
        clearMarkers() %>% 
        addCircleMarkers(stroke =  NA)
    }
    
  })
  
  output$small_map <- renderLeaflet({
    Sys.sleep(sleep)
    leaflet_map
  })
  
  
  # DataTable ---------------------------------------------------------------
  
  output$table <- DT::renderDataTable({
    Sys.sleep(sleep)
    DT::datatable(oao_names, height = '240px',
                  rownames = oao_names$oao,
                  colnames = c("IČO" = "id", "Název organizace" = "name"),
                  options = list(
                    pageLength = 20
                  )
    )
  })
  
  output$table_filtered <- renderTable({
    oao_names[1:6, 1:6]
  })
  
}


# Run the App -------------------------------------------------------------

shinyApp(ui = ui, server = server)

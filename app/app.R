#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

oao_names <- readr::read_csv("oao_names.csv")
oao_scope <- sf::st_read(dsn = "oao_scope.geojson")
oao_points <- sf::st_read(dsn = "oao_points.geojson")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel(
        "Mapa působností organizací s oprávněním provádět archeologický výzkum",
        windowTitle = "Mapa OAO"
    ),
    sidebarLayout(
        sidebarPanel(
            selectInput("oao", "Organizace:", 
                        choices = oao_names$oao,
                        multiple = FALSE),
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
            hr(),
            HTML("Informace jsou čerpány ze <a href=https://www.arup.cas.cz/kdo-je-opravnen-provadet-archeologicke-vyzkumy/>seznamu dohod</a> uzavřených s Akademií věd  ČR", 
                 "a <a href=https://www.mkcr.cz/seznam-organizaci-opravnenych-k-provadeni-archeologickych-vyzkumu-278.html>seznamu oprávnění</a> udělených Ministerstvem kultury ČR.",
                 "Aktualizováno 16. 12. 2020."),
            hr(),
            HTML('<center><img src="logo-aiscr-en2.png", width=160px></center>'),
            br(),
            HTML("Aplikace je součástí infrastruktury <a href=https://www.aiscr.cz/>Archeologický informační systém ČR</a>.<br>",
                 "Autor aplikace: Petr Pajdla (<a href=mailto:pajdla@arub.cz>pajdla@arub.cz</a>).<br>")
        ),
        mainPanel(
            tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
            leaflet::leafletOutput("map")
        ),
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # reactive filtered variables
    oao_scope_flt <- reactive({
        dplyr::filter(oao_scope, oao == input$oao)
    })
    
    oao_points_flt <- reactive({
        oao_points %>% dplyr::filter(oao == input$oao,
                                     datum >= input$date[1],
                                     datum <= input$date[2])
    })
    
    oao_names_flt <- reactive({
        oao_names %>% dplyr::filter(oao == input$oao)
    })
    
    # rendering texts
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
    
    # leaflet map
    output$map <- renderLeaflet({
        if (input$points) {
            oao_scope_flt() %>% 
                leaflet(options = leafletOptions(minZoom = 8, maxZoom = 10)) %>% 
                addTiles() %>% 
                addPolygons(fill = NA) %>% 
                addCircleMarkers(data = oao_points_flt(), stroke =  NA) %>% 
                setView(zoom = 8, lng = 15.4730, lat = 49.8175) %>% 
                setMaxBounds(11, 48, 20, 52)
        } else {
            oao_scope_flt() %>% 
                leaflet(options = leafletOptions(minZoom = 8, maxZoom = 10)) %>% 
                addTiles() %>% 
                addPolygons() %>% 
                setView(zoom = 8, lng = 15.4730, lat = 49.8175) %>% 
                setMaxBounds(11, 48, 20, 52)
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

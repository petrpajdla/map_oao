# app
# deployApp(appDir = here::here("app/"), appName = "app_oao")

# packages ----------------------------------------------------------------

library(shiny)
library(shiny.router)
library(leaflet)
library(DT)
library(dplyr)
library(sf)
library(stringr)

source("R/data.R")
source("R/leaflet_czechrep.R")
source("R/oao_spatial_filter.R")

# constant ----------------------------------------------------------------

sleep <- 0.1
icon_ext_link <- icon("fas fa-external-link-alt")
url_da <- "https://digiarchiv.aiscr.cz/results?entity=projekt&f_organizace="


# data input --------------------------------------------------------------

oao_meta <- oao_meta("data/oao_meta.geojson")
oao_scope <- oao_sf("data/oao_scope.geojson")
oao_grid <- oao_sf("data/oao_grid.geojson")

oao_rep <- oao_scope %>% 
  dplyr::filter(area >= 7.8e10) %>% 
  sf::st_drop_geometry()


# navbar ui ---------------------------------------------------------------

menu <- tags$nav(
  class = "navbar navbar-inverse navbar-static-top",
  tags$div(
    class = "container-fluid",
    tags$div(
      class = "navbar-header",
      tags$p(
        class = "navbar-brand",
        "Organizace s oprávněním provádět archeologický výzkum")
    ),
    tags$ul(
      class = "nav navbar-nav",
      tags$li(
        a(href = route_link("/"),
          icon("fas fa-search"), "hledej podle polohy")),
      tags$li(
        a(href = route_link("detail"), 
          icon("fas fa-map"), "mapa působnosti ")),
      tags$li(
        a(href = route_link("list"), 
          icon("fas fa-bars"), "seznam organizací")),
      tags$li(
        a(href = route_link("about"), 
          icon("fas fa-info-circle"),
          "o aplikaci"))
    ),
    tags$div(
      class = "navbar-right navbar-logo",
      a(href = 'https://www.aiscr.cz/', target = '_blank',
        tags$img(src = 'AISCR_CZ_H_White.png', height = '60px')))
  )
)


# mapclick page -----------------------------------------------------------

# mapclick ui
mapclick_page <- div(
  fluidRow(
    column(
      6, leafletOutput("clickmap")
    ),
    column(
      6, tabsetPanel(
        # výzkumy v zadané vzdálenosti provádí tab (grid)
        tabPanel(
          "Výzkumy provádí",
          fluidRow(
            column(
              7, tags$div(
                style = 'padding: 15px;',
                tags$b("Kliknutím do mapy zvolte bod zájmu."),
                "V okruhu ",
                textOutput("buffer", inline = TRUE),
                " km archeologický výzkum v posledních 5 letech
                prováděly organizace uvedené v tabulce níže.
                Organizace jsou řazeny sestupně dle počtu 
                archeologických výzkumů v zadané vzdálenosti.")
            ),
            column(
              5, tags$div(
                style = 'padding-top: 10px;',
                sliderInput("buffer", "Zvolte vzdálenost",
                            min = 5, max = 20,
                            value = 5, step = 5,
                            ticks = FALSE, post = " km")
              )
            )
          ),
          h4("Výzkumy v zadané vzdálenosti"),
          tableOutput("tab_grid"),
          uiOutput("link_da_buffer")
        ),
        # oprávnění mají tab (poly)
        tabPanel(
          "Oprávnění mají",
          HTML("<div style = 'padding: 15px;'>
               <b>Kliknutím do mapy zvolte bod zájmu.</b>
               Na zadaném území mohou archeologický 
               výzkum provádět organizace uvedené v tabulce níže.
               Organizace jsou řazeny vzestupně dle velikosti území, 
               na kterém jsou oprávněny provádět archeologický výzkum.</div>"),
          fluidRow(
            column(
              6, h4("Oprávnění ve vybrané oblasti"), 
              tableOutput("tab_poly")
            ),
            column(
              6, h4("Oprávnění na celé území ČR"),
              tableOutput("tab_rep")
            )
          ),
          uiOutput("link_da_cell")
        )
      )
    )
  )
)

# mapclick server
mapclick_server <- function(input, output, session) {
  
  output$buffer <- renderText({
    input$buffer
  })
  
  output$clickmap <- renderLeaflet({
    Sys.sleep(sleep)
    leaflet_map
  })
  
  observeEvent(input$clickmap_click, {
    # coordinates of map click
    click_sf <- click2sf(input$clickmap_click)
    
    # add marker
    leaflet_czechrep_add_marker(input$clickmap_click)
    
    # filter oao
    output$tab_poly <- renderTable({
      oao_filter_poly(oao_scope, click_sf, oao_rep)
    }, sanitize.text.function = function(x) x)
    
    output$tab_grid <- renderTable({
      oao_filter_grid(oao_grid, click_sf, input$buffer)
    }, sanitize.text.function = function(x) x)
    
    output$tab_rep <- renderTable({
      oao_rep %>% 
        dplyr::select(Organizace = nazev_zkraceny)
    }, sanitize.text.function = function(x) x)
    
    # links to da
    click_buffer_bbox <- reactive({
      click_buffer(click_sf, input$buffer)
    })
    click_cell_bbox <- click_cell(click_sf)
    
    output$link_da_buffer <- renderUI({
      tagList(
        "Zobrazit vybranou oblast v ",
        tags$a(
          icon_ext_link, "Digitálním archivu AMČR.",
          href = paste0("https://digiarchiv.aiscr.cz/results?mapa=true&loc_rpt=",
                        click_buffer_bbox()[2], ",", 
                        click_buffer_bbox()[1], ",",
                        click_buffer_bbox()[4], ",", 
                        click_buffer_bbox()[3],
                        "&entity=projekt"),
          target = "_blank"
        ))
    })
    
    output$link_da_cell <- renderUI({
      tagList(
        "Zobrazit okolí vybraného bodu v ",
        tags$a(
          icon_ext_link, "Digitálním archivu AMČR.",
          href = paste0("https://digiarchiv.aiscr.cz/results?mapa=true&loc_rpt=",
                        click_cell_bbox[2], ",", 
                        click_cell_bbox[1], ",",
                        click_cell_bbox[4], ",", 
                        click_cell_bbox[3],
                        "&entity=projekt"),
          target = "_blank"
        ))
    })
  })
}


# details page ------------------------------------------------------------

# details ui
details_page <- div(
  fluidRow(
    column(
      4, fluidRow(
        column(
          7, selectInput("oao", "Organizace:", 
                         choices = c(Vyberte = "", oao_meta$nazev_zkraceny), 
                         selectize = TRUE, multiple = FALSE),
        ),
        column(
          5, checkboxInput("poly", "Zobrazit územní působnost", value = TRUE),
          checkboxInput("grid", "Zobrazit akce", value = TRUE)
        ),
      ),
      tags$hr(),
      uiOutput("detail")
      # parse as a single html
      # tags$h3(htmlOutput("link_local", inline = TRUE),
      #         textOutput("name", inline = TRUE)),
      # tags$p(textOutput("ico", inline = TRUE)),
      # tags$p(htmlOutput("web")),
      # tags$p(htmlOutput("link_da")),
      # tags$h4(textOutput("h4_adresa")),
      # tags$p(htmlOutput("address")),
      # tags$h4(textOutput("h4_opravneni")),
      # tags$p(textOutput("auth_text")),
      # tags$h4(textOutput("h4_uzemi")),
      # tags$p(textOutput("scope_text"))
    ),
    column(
      8, leafletOutput("map")
    )
  )
)

# details server
details_server <- function(input, output, session) {
  
  # reactive data
  oao_scope_flt <- reactive({
    oao_filter(oao_scope, input$oao)
  })
  oao_grid_flt <- reactive({
    oao_filter(oao_grid, input$oao)
  })
  oao_meta_flt <- reactive({
    oao_filter(oao_meta, input$oao)
  })
  
  # text details
  output$detail <- renderText({
    if (input$oao == "MU Brno") {
      includeHTML("text/mu_brno.html")
    } else if (input$oao == "Národní památkový ústav") {
      includeHTML("text/npu.html")
    } else {
      oao_meta_flt() %>% dplyr::transmute(
        text = HTML(paste0(
          "<h3>", nazev_full, "</h3>",
          "<h4>(", nazev_zkraceny, ")</h4>",
          "<p>Webové stránky: ", web, "</p>",
          "<p>IČO: ", ico, "</p>",
          "<h4>Adresa</h4>",
          "<p>", address, "</p>",
          "<h4>Detaily oprávnění</h4>",
          "<p>", opravneni, "</p>",
          "<h4>Územní působnost</h4>",
          "<p>", uzemi, ".</p>",
          "<p>Zobrazit projekty vybrané organizace v ",
          "<a href='", url_da, 
          stringr::str_replace_all(nazev_zkraceny, "\\s", "%20"), 
          "'>", 
          icon_ext_link, " Digitálním archivu AMČR", "</a>."
        ))) %>% 
        dplyr::pull(text)
    }
  })
  
  # map
  output$map <- renderLeaflet({
    Sys.sleep(sleep)
    leaflet_map
  })
  
  # clear map when oao is switched
  observeEvent(input$oao, {
    leafletProxy("map") %>%
      clearShapes()
  })
  
  observeEvent(input$oao, {
    leafletProxy("map") %>%
      leaflet::setView(zoom = 8, lng = 15.4730, lat = 49.8175)
  })
  
  # remove grid and polygon
  observe({
    if (!input$poly) {
      leafletProxy("map") %>%
        removeShape("poly")
    }
    if (!input$grid) {
      leafletProxy("map") %>%
        removeShape(oao_grid_flt()$ctverec) %>%
        removeControl("legend")
    }
    if (!input$poly & !input$grid) {
      leafletProxy("map") %>%
        clearShapes()
    }
  })
  
  # add grid and polygon
  observe({
    req(input$oao)
    # add grid
    if (input$grid) {
      pal <- colorNumeric(palette = "YlGnBu", domain = oao_grid_flt()$scaled)
      leafletProxy("map", data = oao_grid_flt()) %>% 
        addPolygons(layerId = oao_grid_flt()$ctverec, color = ~pal(scaled), 
                    stroke =  FALSE, fillOpacity = 0.6) %>% 
        addControl("<img src='legend.png' width=110 height=40>", 
                   position = "bottomleft", layerId = "legend")
    }
    
    # add polygon
    if (input$poly) {
      leafletProxy("map", data = oao_scope_flt()) %>%
        addPolygons(layerId = "poly", fill = NA, color = "#3E3F3A", weight = 6)
    }
  })
}


# list page ---------------------------------------------------------------

list_page <- div(
  DT::dataTableOutput("table")
)

# list server
list_server <- function(input, output, session) {
  
  # DT table
  output$table <- DT::renderDataTable({
    oao_meta %>% 
      sf::st_drop_geometry() %>%
      dplyr::mutate(
        nazev = paste0("<b>", nazev_full, "</b><br>(", nazev_zkraceny, ")")) %>% 
      dplyr::select(nazev, ico, web, address) %>% 
      DT::datatable(
        escape = FALSE,
        extensions = 'Scroller', 
        rownames = FALSE,
        colnames = c(
          "Organizace" = "nazev",
          "IČO" = "ico", 
          "Webové stránky" = "web",
          "Adresa" = "address"),
        options = list(
          deferRender = TRUE,
          scrollY = "calc(100vh - 240px)", 
          scroller = TRUE,
          columnDefs = list(list(className = 'dt-left', targets = "_all"))
        )
      )
  })
  
}


# about page --------------------------------------------------------------

about_page <- div(
  includeMarkdown("text/about.md")
)


# server calls ------------------------------------------------------------

leaflet_map <- oao_scope %>% 
  leaflet_czechrep()


# router ------------------------------------------------------------------

router <- make_router(
  route("/", mapclick_page, mapclick_server),
  route("detail", details_page, details_server),
  route("list", list_page, list_server),
  route("about", about_page)
)


# ui ----------------------------------------------------------------------

ui <- fluidPage(
  title = "Mapa OAO",
  theme = "main.css",
  tags$head(includeHTML("google-analytics.html")),
  menu,
  router$ui,
)


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  router$server(input, output, session)
  
  # greeter ---------------------------------------------------------------
  
  greeter <- modalDialog(
    title = "Vítejte!",
    easyClose = TRUE, 
    size = "l",
    includeHTML("text/greeter.html"),
    footer = modalButton(label = "Zavřít")
  )
  
  showModal(greeter)
  
}


# app ---------------------------------------------------------------------

shinyApp(ui, server)


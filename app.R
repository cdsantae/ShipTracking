suppressPackageStartupMessages({
    library(shiny)
    library(shiny.semantic)
    library(shiny.router)
    library(shinyWidgets)
    library(leaftime)
    library(leaflet)
    library(DT)
})

source("Longest.R")
source("Traffic.R")
source("Travel.R")

router <- make_router(
    route("long", Lship_ui("S1")),
    route("traffic", TRship_ui("S2")),
    route("travel", Tship_ui("S3"))
)

ui <- semanticPage(
    title = "Ship Tracking Dashboard",
    h1("Ship Tracking Dashboard",align = "center"),
    tags$head(
        tags$link(rel="stylesheet", href="style.css", type="text/css")
    ),
    setBackgroundColor(
        color = c("#FFFFFF", "#BFA3A3"),
        gradient = "linear",
        direction = "bottom"
    ),
    horizontal_menu(
        list(
            list(name = "Longest Distance", link = route_link("long"), icon = "anchor"),
            list(name = "Traffic Sails", link = route_link("traffic"), icon = "ship"),
            list(name = "Total Traveled", link = route_link("travel"), icon = "map marker")
        )
    ),
    router$ui
)

server <- function(input, output, session) {
    router$server(input, output, session)
    Lship_server("S1")
    TRship_server("S2")
    Tship_server("S3")
}

shinyApp(ui = ui, server = server)
source("R/functions.R")

Lship_ui <- function(id,label = "Counter") {
  ns <- NS(id)
  tagList(
    div(class="ui two column stackable grid container",
        div(class="eight wide column",
            p(strong("Select ship type:")),
            dropdown_input(input_id = ns("Lship_type"), choices = NULL)
        ),
        div(class="eight wide column",
            p(strong("Select ship name:")),
            dropdown_input(input_id = ns("Lship_name"), choices = NULL)
        )
    ),br(),
    main_panel(
        div(class="ui two column stackable grid container",
            div(class="ten wide column",
                segment(
                  div(class="ui black ribbon label", "Map"),
                  leafletOutput(ns("Longest_map")))
                ),
            div(class="six wide column",
                segment(class = "raised segment",
                        uiOutput(ns("Lresume_ship")))
                )
            )
        )
    )
}

Lship_server <- function(id){
  ns <- NS(id)
  moduleServer(id,function(input,output,session){
    
    shipIcon <- iconList(
      start = makeIcon("cargo_ship_grey.svg", iconWidth = 60, iconHeight = 60),
      end = makeIcon("cargo_ship_col.svg", iconWidth = 60, iconHeight = 60)
    )
    
    update_dropdown_input(session, "Lship_type",
                          choices = unique(data$ship_type))
    observe({
      req(input$Lship_type)
      LS_data <- data %>%
        filter(ship_type == input$Lship_type)
      update_dropdown_input(session,"Lship_name",
                            choices = unique(LS_data$SHIPNAME))
    })
    Lselect_info <- reactive({
      req(input$Lship_name)
      data %>%
        long_travel(input$Lship_name) %>%
        data.frame
    })
    
    output$Lresume_ship <- renderUI(
      segment(
        h2(class="ui icon header",icon("ship"),align = "center",
           div(class="content",Lselect_info()$SHIPNAME),align = "center",
           div(Class="sub header",
               paste("Type:",Lselect_info()$ship_type),align = "center")
           ),
        div(class="ui divider"),
        h3("Ship Details:"),
        strong("ID: "),Lselect_info()$SHIP_ID,br(),
        strong("Flag: "),Lselect_info()$FLAG,br(),
        strong("Length: "),Lselect_info()$LENGTH," m",br(),
        strong("Width: "),Lselect_info()$WIDTH," m",br(),
        strong("Deadweight Tonnage: "),Lselect_info()$DWT," tonne",br(),
        strong("Average Speed: "),round(Lselect_info()$AvSp,2)," kn",br(),
        strong("Top Speed: "),Lselect_info()$TpSp," kn",br(),
        strong("Longest sailed distance: "),round(Lselect_info()$dist,2)," m",br()
        )
      )
    
    output$Longest_map <- renderLeaflet({
      leaflet(data = Lselect_info()) %>%
        addTiles() %>%
        addMarkers(lng = ~lon_i,lat=~lat_i,
                   icon = ~shipIcon$start,
                   popup = ~paste("<b>Start</b><br>",
                                  "<b>LAT</b>:",lat_i,
                                  "<br><b>LON</b>:",lon_i,
                                  "<br><b>Time measure:</b>",start)) %>%
        addMarkers(lng = ~lon_f,lat=~lat_f,
                   icon = ~shipIcon$end,
                   popup = ~paste("<b>End</b><br>",
                                  "<b>LAT</b>:",lat_f,
                                  "<br><b>LON</b>:",lon_f,
                                  "<br><b>Time measure:</b>",end)) %>%
        addPolylines(lng = ~c(lon_i,lon_f),
                     lat = ~c(lat_i,lat_f),
                     dashArray = 5,weight = 3,
                     label = ~HTML(
                       paste("<b>Distance:</b>",
                             round(dist,2),"(m)<br/>",
                             "<b>Time elapsed:</b>",
                             round(difftime(start,end),2),"days")),
                     labelOptions = labelOptions(permanent = TRUE))
    })
  })
}